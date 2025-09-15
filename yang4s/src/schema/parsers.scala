package yang4s.schema

import yang4s.parser.Statement

import yang4s.schema.{Keyword => Kw}
import cats.data.State
import cats.data.StateT
import cats.data.EitherT
import cats.implicits.{*, given}
import cats.parse.Parser
import cats.Applicative

case class ParsingCtx(
    namespace: String,
    typeDefs: List[SchemaType],
    schemaCtx: SchemaContext,
    imports: Map[String, SchemaModule]
)
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

    def fromValidated[A](stmt: Statement)(fn: ValidStatements => ParserResult[A]) = {
      Grammar.validate(stmt).fold(fail, fn)
    }

    def validate(stmt: Statement): ParserResult[ValidStatements] = fromValidated(stmt)(success)
  }

  def moduleParser: PartialFunction[Statement, ParserResult[Module]] = {
    case stmt @ Statement(None, Keyword.Module.literal, Some(arg), children) =>
      ParserResult.fromValidated(stmt) { v =>
        for {
          namespace <- namespaceParser(v.required(Kw.Namespace))
          prefix <- prefixParser(v.required(Kw.Prefix))
          imports <- importsParser(v)
          typeDefs <- typeDefsParser(v)
          dataDefs <- dataDefParser(v)
        } yield (Module(arg, namespace, prefix, dataDefs, typeDefs))
      }
  }

  def resolveImports(imports: List[Import]): ParserResult[Unit] = {
    val moduleNames = imports.map(i => ModuleName(i.module, None))

    ParserResult.modifyF { ctx =>
      for {
        (schemaCtx, modules) <- ctx.schemaCtx.loadModules(moduleNames)
      } yield (
        ctx.copy(
          schemaCtx = schemaCtx,
          imports = imports.map(_.prefix).zip(modules).toMap
        )
      )
    }
  }

  def namespaceParser(stmt: Statement): ParserResult[String] =
    parseString(stmt).flatTap(ns => ParserResult.modify(_.copy(namespace = ns)))

  def prefixParser(stmt: Statement): ParserResult[String] = parseString(stmt)

  def containerParser(stmt: Statement): ParserResult[SchemaNode] = ParserResult.fromValidated(stmt) { v =>
    for {
      ctx <- StateT.get
      dataDefs <- dataDefParser(v)
    } yield (ContainerNode(SchemaMeta(stmt.arg.get, ctx.namespace, None), dataDefs))
  }

  def listParser(stmt: Statement): ParserResult[SchemaNode] = ParserResult.fromValidated(stmt) { v =>
    for {
      ctx <- StateT.get
      dataDefs <- dataDefParser(v)
      key <- ParserResult.success(v.optional(Keyword.Key)).flatMap(_.map(keyParser).sequence)
    } yield (ListNode(SchemaMeta(stmt.arg.get, ctx.namespace, None), dataDefs, key))
  }

  def keyParser(stmt: Statement): ParserResult[String] = parseString(stmt)

  def LeafParser(stmt: Statement): ParserResult[SchemaNode] = ParserResult.fromValidated(stmt) { v =>
    for {
      ctx <- StateT.get
      dataDefs <- dataDefParser(v)
      tpe <- typeParser(v.required(Keyword.Type))
    } yield (LeafNode(SchemaMeta(stmt.arg.get, ctx.namespace, None), dataDefs, tpe))
  }

  def typeParser(stmt: Statement): ParserResult[SchemaType] = ParserResult.fromValidated(stmt) { v =>
    val (prefix, tpe): (Option[String], String) = stmt.arg.get.split(":", 2) match
      case Array(prefix, tpe) => (Some(prefix), tpe)
      case _                  => (None, stmt.arg.get)

    def getType(ctx: ParsingCtx, tpe: String, prefix: Option[String]): ErrorOr[SchemaType] = {
      val name = prefix.map(p => s"$p:$tpe").getOrElse(tpe)

      val fromImport = for {
        p <- prefix
        imported <- ctx.imports.get(p)
        td <- imported.typeDefs.find(_.name == tpe)
      } yield td.copy(name = name)

      val fromBuiltInOrCtx = {
        BuiltInType
          .fromLiteral(stmt.arg.get)
          .orElse(ctx.typeDefs.find(_.name == stmt.arg.get).map(_.tpe))
          .map(b => SchemaType(tpe, b))
      }

      (fromImport orElse fromBuiltInOrCtx).toRight(s"Unknown type ${name}")
    }

    StateT.inspectF(getType(_, tpe, prefix))
  }

  def typeDefParser(stmt: Statement): ParserResult[SchemaType] = ParserResult.fromValidated(stmt) { v =>
    for {
      baseType <- typeParser(v.required(Keyword.Type))
    } yield (baseType.copy(name = stmt.arg.get))
  }

  def typeDefsParser(v: ValidStatements): ParserResult[List[SchemaType]] =
    v.many0(Keyword.TypeDef).map(typeDefParser).sequence.flatTap(typeDefs => ParserResult.modify(_.copy(typeDefs = typeDefs)))

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
      (Keyword.Leaf, LeafParser)
    ).foldLeft[List[ParserResult[SchemaNode]]](List.empty) { case (acc, (kw, fn)) =>
      acc.concat(vStmts.stmts.lift(kw).getOrElse(List.empty).map(fn))
    }.sequence
  }

  def parseString(stmt: Statement): ParserResult[String] = ParserResult.validate(stmt).as(stmt.arg.get)

  // def parseMany(v: Vali)

  val schemaModule = moduleParser.lift
}
