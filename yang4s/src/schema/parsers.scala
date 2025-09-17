package yang4s.schema

import yang4s.parser.Statement

import yang4s.schema.{Keyword => Kw}
import cats.data.State
import cats.data.StateT
import cats.data.EitherT
import cats.implicits.{*, given}
import cats.parse.Parser
import cats.Applicative
import yang4s.schema.parsers.ErrorOr
import java.net.URI
import scala.util.Try

case class TypeDefScope(unresolved: Map[QName, Statement], resolved: List[SchemaType])

object TypeDefScope {
  def apply(schemaTypes: SchemaType*): TypeDefScope = TypeDefScope.empty.copy(resolved = List(schemaTypes*))
  def empty: TypeDefScope = TypeDefScope(Map.empty, List.empty)
}

opaque type Stack[A] = List[A]

object Stack {
  def apply[A](scopes: A*) = List(scopes*)
  def empty[A]: Stack[A] = Stack()

  extension [A](stack: Stack[A]) {
    def push(a: A): Stack[A] = a :: stack
    def pop(): Stack[A] = {
      stack match
        case _ :: next => next
        case Nil       => List.empty
    }
    def head: Option[A] = stack.headOption

    def modifyHead(fn: Option[A] => A): Stack[A] = {
      pop().push(fn(stack.headOption))
    }
  }
}

case class ParsingCtx(
    namespace: Namespace,
    typeDefs: List[SchemaType],
    schemaCtx: SchemaContext,
    imports: Map[String, Namespace],
    typeDefStack: Stack[TypeDefScope]
) {
  def getNamespace(prefix: Option[String]): ErrorOr[Namespace] = {
    prefix match
      case Some(p) =>
        for {
          namespace1 <- imports
            .lift(p)
            .fold {
              if (namespace.prefix == Some(p))
                Right(namespace)
              else Left(s"Unknown prefix $p")
            }(Right(_))
        } yield (namespace1)
      case None => Right(namespace)
  }
}

object ParsingCtx {
  def fromSchemaCtx(ctx: SchemaContext): ParsingCtx = ParsingCtx(Namespace.DEFAULT, List.empty, ctx, Map.empty, Stack.empty)
}

case class Import(module: String, prefix: String)

object parsers {
  type Error = String
  type ErrorOr[A] = Either[Error, A]
  type ParserResult[A] = StateT[ErrorOr, ParsingCtx, A]

  object ParserResult {
    def fromEither[A](v: ErrorOr[A]): ParserResult[A] = StateT.liftF(v)
    def fail[A](error: Error): ParserResult[A] = fromEither(Left(error))
    def success[A](a: A): ParserResult[A] = fromEither(Right(a))
    def modify = StateT.modify[ErrorOr, ParsingCtx]
    def modifyF = StateT.modifyF[ErrorOr, ParsingCtx]
    def inspectF[A] = StateT.inspectF[ErrorOr, ParsingCtx, A]
    def inspect[A] = StateT.inspect[ErrorOr, ParsingCtx, A]

    def fromValidated[A](stmt: Statement)(fn: ValidStatements => ParserResult[A]) = {
      Grammar.validate(stmt).fold(fail, fn)
    }

    def validate(stmt: Statement): ParserResult[ValidStatements] = fromValidated(stmt)(success)

  }

  def withTypeDefScope[A](body: ParserResult[A]): ParserResult[A] = {
    for {
      _ <- ParserResult.modify(ctx =>
        ctx.copy(typeDefStack = ctx.typeDefStack.push(ctx.typeDefStack.head.getOrElse(TypeDefScope.empty)))
      )
      a <- body
      _ <- ParserResult.modify(ctx => ctx.copy(typeDefStack = ctx.typeDefStack.pop()))
    } yield (a)
  }

  def moduleParser: PartialFunction[Statement, ParserResult[Module]] = {
    case stmt @ Statement(None, Keyword.Module.literal, Some(arg), children) =>
      ParserResult.fromValidated(stmt) { v =>
        for {
          namespace <- namespaceParser(v.required(Kw.Namespace))
          prefix <- prefixParser(v.required(Kw.Prefix)).flatTap(p =>
            ParserResult.modify(ctx => ctx.copy(namespace = ctx.namespace.copy(prefix = Some(p))))
          )
          imports <- importsParser(v)
          _ <- typeDefsParser(v)
          dataDefs <- dataDefParser(v)
          typeDefs <- ParserResult.inspect(_.typeDefStack.head.map(_.resolved).getOrElse(List.empty))
        } yield (Module(arg, namespace, prefix, dataDefs, typeDefs))
      }
  }

  def resolveImports(imports: List[Import]): ParserResult[Unit] = {
    val moduleNames = imports.map(i => ModuleName(i.module, None))

    ParserResult.modifyF { ctx =>
      for {
        (schemaCtx, modules) <- ctx.schemaCtx.loadModules(moduleNames)
      } yield {

        val scope = ctx.typeDefStack.head.getOrElse(TypeDefScope.empty)
        val stack = ctx.typeDefStack.pop()
        println(modules)

        ctx.copy(
          schemaCtx = schemaCtx,
          imports = imports.map(_.prefix).zip(modules).map((p, m) => (p, m.namespace.copy(prefix = Some(p)))).toMap,
          typeDefStack = stack.push(scope.copy(resolved = scope.resolved ++ modules.flatMap(_.typeDefs)))
        )
      }
    }
  }

  def namespaceParser(stmt: Statement): ParserResult[Namespace] =
    parseURI(stmt).map(Namespace(_, None)).flatTap(ns => ParserResult.modify(_.copy(namespace = ns)))

  def prefixParser(stmt: Statement): ParserResult[String] =
    parseString(stmt)

  def containerParser(stmt: Statement): ParserResult[SchemaNode] = ParserResult.fromValidated(stmt) { v =>
    for {
      ctx <- StateT.get
      dataDefs <- dataDefParser(v)
    } yield (ContainerNode(SchemaMeta(QName(ctx.namespace, stmt.arg.get), None), dataDefs))
  }

  def listParser(stmt: Statement): ParserResult[SchemaNode] = ParserResult.fromValidated(stmt) { v =>
    for {
      ctx <- StateT.get
      dataDefs <- dataDefParser(v)
      key <- ParserResult.success(v.optional(Keyword.Key)).flatMap(_.map(keyParser).sequence)
    } yield (ListNode(SchemaMeta(QName(ctx.namespace, stmt.arg.get), None), dataDefs, key))
  }

  def keyParser(stmt: Statement): ParserResult[String] = parseString(stmt)

  def leafParser(stmt: Statement): ParserResult[SchemaNode] = ParserResult.fromValidated(stmt) { v =>
    for {
      ctx <- StateT.get
      dataDefs <- dataDefParser(v)
      tpe <- typeParser(v.required(Keyword.Type))
    } yield (LeafNode(SchemaMeta(QName(ctx.namespace, stmt.arg.get), None), dataDefs, tpe))
  }

  def typeParser(stmt: Statement): ParserResult[SchemaType] = ParserResult.fromValidated(stmt) { v =>
    def getType(ctx: ParsingCtx, qName: QName): ErrorOr[SchemaType] = {
      val fromBuiltIn = BuiltInType.fromLiteral(qName.localName).map(SchemaType.fromBuiltIn(_))
      val fromScope = ctx.typeDefStack.head.flatMap(_.resolved.find(_.qName == qName))

      // Todo: Show instance for qname
      (fromBuiltIn orElse fromScope).toRight(s"Unknown type ${qName} ${ctx.typeDefStack.head}")
    }

    def resolve(qName: QName): ParserResult[Unit] = {
      for {
        scope <- ParserResult.inspect(_.typeDefStack.head.getOrElse(TypeDefScope.empty))
        _ <- scope.unresolved.lift(qName).map(typeDefParser(_)).sequence
        _ <- ParserResult.modify(ctx =>
          ctx.copy(typeDefStack = ctx.typeDefStack.modifyHead { maybeScope =>
            maybeScope.map(s => s.copy(unresolved = s.unresolved.removed(qName))).getOrElse(TypeDefScope.empty)
          })
        )
      } yield ()
    }

    for {
      qName <- qNameFromStmt(stmt)
      _ <- resolve(qName)
      schemaType <- StateT.inspectF(getType(_, qName))
    } yield (schemaType)
  }

  def typeDefParser(stmt: Statement): ParserResult[SchemaType] = ParserResult.fromValidated(stmt) { v =>
    val result = for {
      schemaType <- typeParser(v.required(Keyword.Type))
      qName <- qNameFromStmt(stmt)
    } yield (schemaType.copy(qName = qName))
    result.flatTap(st =>
      ParserResult.modify(ctx =>
        ctx.copy(typeDefStack = ctx.typeDefStack.modifyHead { maybeScope =>
          maybeScope.map(s => s.copy(resolved = st :: s.resolved)).getOrElse(TypeDefScope.empty)
        })
      )
    )
  }

  def typeDefsParser(v: ValidStatements): ParserResult[Unit] = {
    v
      .many0(Keyword.TypeDef)
      .map(stmt => qNameFromStmt(stmt).map((_, stmt)))
      .sequence
      .flatTap { unresolved =>
        ParserResult.modify(ctx =>
          ctx.copy(typeDefStack = ctx.typeDefStack.modifyHead { maybeScope =>
            maybeScope.map(s => s.copy(unresolved = s.unresolved ++ unresolved.toMap)).getOrElse(TypeDefScope.empty)
          })
        )
      }
      .map(_.map(_._2))
      .flatMap(stmts => stmts.map(typeDefParser).sequence)
      .as(())
  }

  def importParser(stmt: Statement): ParserResult[Import] = ParserResult.fromValidated(stmt) { v =>
    for {
      prefix <- prefixParser(v.required(Keyword.Prefix))
    } yield (Import(stmt.arg.get, prefix))
  }

  def importsParser(v: ValidStatements): ParserResult[List[Import]] = {
    v.many0(Keyword.Import).map(importParser).sequence.flatTap(resolveImports)
  }

  def dataDefParser(vStmts: ValidStatements): ParserResult[List[SchemaNode]] = {
    // Todo: We should maintain order based on definition in source file.
    Seq(
      (Keyword.Container, containerParser),
      (Keyword.List, listParser),
      (Keyword.Leaf, leafParser)
    ).foldLeft[List[ParserResult[SchemaNode]]](List.empty) { case (acc, (kw, fn)) =>
      acc.concat(vStmts.stmts.lift(kw).getOrElse(List.empty).map(fn))
    }.sequence
  }

  def qNameFromStmt(stmt: Statement): ParserResult[QName] = {
    val (prefix, identifier): (Option[String], String) = stmt.arg.get.split(":", 2) match
      case Array(prefix, tpe) => (Some(prefix), tpe)
      case _                  => (None, stmt.arg.get)

    for {
      ctx <- StateT.get
      namespace <- ParserResult.fromEither(ctx.getNamespace(prefix))
    } yield (QName(namespace, identifier))
  }

  def parseString(stmt: Statement): ParserResult[String] = ParserResult.validate(stmt).as(stmt.arg.get)
  def parseURI(stmt: Statement): ParserResult[URI] = ParserResult.validate(stmt).flatMap { _ =>
    ParserResult.fromEither(Try(URI(stmt.arg.get)).toOption.toRight(s"${stmt.arg.get} is not a valid namespace."))
  }
}
