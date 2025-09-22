package yang4s.schema

import yang4s.parser.Statement

import yang4s.schema.{Keyword => Kw}
import yang4s.schema.SchemaNode.*
import cats.data.State
import cats.data.StateT
import cats.data.EitherT
import cats.implicits.{*, given}
import cats.parse.Parser
import cats.Applicative
import alleycats.Empty
import yang4s.schema.parsers.ErrorOr
import java.net.URI
import scala.util.Try
import yang4s.utils.Stack


object parsers {
  type Error = String
  type ErrorOr[A] = Either[Error, A]
  type ParserResult[A] = StateT[ErrorOr, ParsingCtx, A]

  case class TypeDefScope(unresolved: Map[QName, Statement], resolved: List[SchemaType])

  object TypeDefScope {
    def apply(schemaTypes: SchemaType*): TypeDefScope = TypeDefScope.empty.copy(resolved = List(schemaTypes*))
    def empty: TypeDefScope = TypeDefScope(Map.empty, List.empty)

    given Empty[TypeDefScope]:
      def empty: TypeDefScope = TypeDefScope.empty

    extension (self: TypeDefScope) {
      def mergeTypeDefs(sts: SchemaType*) = self.copy(resolved = self.resolved ++ sts)
      def mergeUnresolved(unresolved: Tuple2[QName, Statement]*) =
        self.copy(unresolved = self.unresolved ++ unresolved.toMap)

      def resolve(qName: QName, st: SchemaType): TypeDefScope = {
        mergeTypeDefs(st).copy(unresolved = self.unresolved.removed(qName))
      }
    }
  }

  case class ParsingCtx(
      namespace: Namespace,
      typeDefs: List[SchemaType],
      schemaCtx: SchemaContext,
      imports: Map[String, Namespace],
      typeDefStack: Stack[TypeDefScope]
  ) 

  object ParsingCtx {
    def fromSchemaCtx(ctx: SchemaContext): ParsingCtx = ParsingCtx(Namespace.DEFAULT, List.empty, ctx, Map.empty, Stack.empty)

    extension (self: ParsingCtx) {
      def getNamespace(maybePrefix: Option[String]): ErrorOr[Namespace] = {
        maybePrefix match
          case Some(p) =>
            self.imports
              .get(p)
              .orElse(self.namespace.prefix.filter(_ == p).map(_ => self.namespace))
              .toRight(s"Unknown prefix $p")
          case None => Right(self.namespace)
      }
    }
  }

  case class Import(module: String, prefix: String)

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
        ctx.copy(typeDefStack = ctx.typeDefStack.push(ctx.typeDefStack.peak))
      )
      a <- body
      _ <- ParserResult.modify(ctx => ctx.copy(typeDefStack = ctx.typeDefStack.pop._2))
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
          features <- v.many0(Keyword.Feature).map(featureDefinitionParser).sequence
          _ <- resolveTypeDefs(v)
          dataDefs <- dataDefParser(v, true)
          typeDefs <- ParserResult.inspect(_.typeDefStack.peak.resolved)
        } yield (Module(arg, namespace, prefix, dataDefs, typeDefs, features))
      }
  }

  def resolveImports(imports: List[Import]): ParserResult[Unit] = {
    val moduleNames = imports.map(i => ModuleName(i.module, None))

    ParserResult.modifyF { ctx =>
      for {
        (schemaCtx, modules) <- ctx.schemaCtx.loadModules(moduleNames)
      } yield {

        val scope = ctx.typeDefStack.peak
        val stack = ctx.typeDefStack.pop._2


        ctx.copy(
          schemaCtx = schemaCtx,
          imports = imports.map(_.prefix).zip(modules).map((p, m) => (p, m.namespace.copy(prefix = Some(p)))).toMap,
          typeDefStack = ctx.typeDefStack.withModifiedHead(_.mergeTypeDefs(modules.flatMap(_.typeDefs)*))
        )
      }
    }
  }

  def namespaceParser(stmt: Statement): ParserResult[Namespace] =
    parseURI(stmt).map(Namespace(_, None)).flatTap(ns => ParserResult.modify(_.copy(namespace = ns)))

  def prefixParser(stmt: Statement): ParserResult[String] =
    parseString(stmt)

  def containerParser(stmt: Statement, config: Boolean): ParserResult[DataNode] = ParserResult.fromValidated(stmt) { v =>
    for {
      schemaMeta <- dataNodeSchemaMetaParser(stmt, v, config)
      dataDefs <- dataDefParser(v, schemaMeta.config)
    } yield (containerNode(schemaMeta, dataDefs))
  }

  def listParser(stmt: Statement, config: Boolean): ParserResult[DataNode] = ParserResult.fromValidated(stmt) { v =>
    for {
      schemaMeta <- dataNodeSchemaMetaParser(stmt, v, config)
      dataDefs <- dataDefParser(v, schemaMeta.config)
      key <- ParserResult.success(v.optional(Keyword.Key)).flatMap(_.map(keyParser).sequence)
    } yield (listNode(schemaMeta, dataDefs, key))
  }

  def keyParser(stmt: Statement): ParserResult[String] = parseString(stmt)
  def configParser(stmt: Option[Statement], default: Boolean): ParserResult[Boolean] =
    ParserResult.success(stmt).flatMap(_.map(parseBoolean).sequence).map(_.getOrElse(default))

  def leafParser(stmt: Statement, config: Boolean): ParserResult[DataNode] = ParserResult.fromValidated(stmt) { v =>
    for {
      schemaMeta <- dataNodeSchemaMetaParser(stmt, v, config)
      tpe <- typeParser(v.required(Keyword.Type))
    } yield (leafNode(schemaMeta, tpe))
  }

  def leafListParser(stmt: Statement, config: Boolean): ParserResult[DataNode] = ParserResult.fromValidated(stmt) { v => 
      for {
        schemaMeta <- dataNodeSchemaMetaParser(stmt, v, config)
        dataDefs <- dataDefParser(v, schemaMeta.config)
        tpe <- typeParser(v.required(Keyword.Type))
      } yield (leafListNode(schemaMeta, tpe))
    }

  def typeParser(stmt: Statement): ParserResult[SchemaType] = ParserResult.fromValidated(stmt) { v =>
    def getType(ctx: ParsingCtx, qName: QName): ErrorOr[SchemaType] = {
      val fromBuiltIn = BuiltInType.fromLiteral(qName.localName).map(SchemaType.fromBuiltIn(_))
      val fromScope = ctx.typeDefStack.peak.resolved.find(_.qName == qName)

      // Todo: Show instance for qname
      (fromBuiltIn orElse fromScope).toRight(s"Unknown type ${qName}")
    }


    for {
      qName <- qNameFromStmt(stmt)
      _ <- resolveTypeDef(qName)
      schemaType <- StateT.inspectF(getType(_, qName))
    } yield (schemaType)
  }

  def typeDefParser(stmt: Statement): ParserResult[SchemaType] = ParserResult.fromValidated(stmt) { v =>
    for {
      schemaType <- typeParser(v.required(Keyword.Type))
      qName <- qNameFromStmt(stmt)
    } yield (schemaType.copy(qName = qName))
  }

  def resolveTypeDef(qName: QName): ParserResult[Unit] = {
    for {
      scope <- ParserResult.inspect(_.typeDefStack.peak)
      _ <- scope.unresolved.get(qName).map { stmt =>
            typeDefParser(stmt).flatMap { st =>
              ParserResult.modify { ctx =>
                  ctx.copy(
                    typeDefStack = ctx.typeDefStack.withModifiedHead(_.resolve(qName, st))
                  )
                }
              }
          }.sequence
    } yield ()
  }

  def resolveTypeDefs(v: ValidStatements): ParserResult[Unit] = {
    v
      .many0(Keyword.TypeDef)
      .map(stmt => qNameFromStmt(stmt).map((_, stmt)))
      .sequence
      .flatTap { unresolved =>
        ParserResult.modify(ctx =>
          ctx.copy(typeDefStack = ctx.typeDefStack.withModifiedHead { scope =>
            scope.mergeUnresolved(unresolved*)
          })
        )
      }
      .flatMap(_.map(t => resolveTypeDef(t.head)).sequence)
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

  def dataDefParser(vStmts: ValidStatements, config: Boolean): ParserResult[List[DataNode]] = {
    val kwParserMap = Seq(
      (Keyword.Container, containerParser),
      (Keyword.List, listParser),
      (Keyword.Leaf, leafParser),
      (Keyword.LeafList, leafListParser)
    ).toMap

    vStmts.filter(kwParserMap.keySet.contains).foldLeft[List[ParserResult[DataNode]]](List.empty) { case (acc, (kw, stmt)) =>
        kwParserMap(kw)(stmt, config) :: acc
      }.sequence
  }

  def schemaMetaParser(stmt: Statement, v: ValidStatements): ParserResult[SchemaMeta] = {
    for {
      qName <- qNameFromStmt(stmt)
      status <- v.optional(Keyword.Status).map(statusParser(_)).sequence.map(_.getOrElse(Status.Current))
    } yield (SchemaMeta(qName, None, false, status))
  }

  def statusParser(stmt: Statement): ParserResult[Status] = {
    ParserResult.fromEither(Status.fromLiteral(stmt.arg.get).toRight("Invalid status arguement"))
  }

  def dataNodeSchemaMetaParser(stmt: Statement, v: ValidStatements, config: Boolean): ParserResult[SchemaMeta] = {
    for {
      config1 <- configParser(v.optional(Keyword.Config), config)
      meta <- schemaMetaParser(stmt, v)
    } yield (meta.copy(config = config1))
  }

  def featureDefinitionParser(stmt: Statement): ParserResult[FeatureDefinition] = ParserResult.fromValidated(stmt) { v =>
      for {
        meta <- schemaMetaParser(stmt, v)
      } yield (FeatureDefinition(meta))
    }

  def qNameFromStmt(stmt: Statement): ParserResult[QName] = {
    val (prefix, identifier): (Option[String], String) = stmt.arg.get.split(":", 2) match
      case Array(prefix, tpe) => (Some(prefix), tpe)
      case _                  => (None, stmt.arg.get)

    ParserResult.inspectF(_.getNamespace(prefix)).map(QName(_, identifier))
  }

  def parseString(stmt: Statement): ParserResult[String] = ParserResult.validate(stmt).as(stmt.arg.get)
  def parseBoolean(stmt: Statement): ParserResult[Boolean] = ParserResult.validate(stmt).as {
    val arg = stmt.arg.get
    arg == "true"
  }

  def parseURI(stmt: Statement): ParserResult[URI] = ParserResult.validate(stmt).flatMap { _ =>
    ParserResult.fromEither(Try(URI(stmt.arg.get)).toOption.toRight(s"${stmt.arg.get} is not a valid namespace."))
  }
}
