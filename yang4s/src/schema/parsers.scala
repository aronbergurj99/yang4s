package yang4s.schema

import yang4s.parser.Statement

import yang4s.schema.{Keyword => Kw}
import cats.data.State
import cats.data.StateT
import cats.data.EitherT
import cats.implicits.{*, given}
import cats.parse.Parser
import cats.Applicative

case class ParsingCtx(namespace: String, typeDefs: List[SchemaType])

object parsers {
  type Error = String
  type ErrorOr[A] = Either[Error, A]
  type ParserResult[A] = StateT[ErrorOr, ParsingCtx, A]

  object ParserResult {
    def fromEither[A](v: ErrorOr[A]): ParserResult[A] = StateT.liftF(v)
    def fail[A](error: Error): ParserResult[A] = fromEither(Left(error))
    def success[A](a: A): ParserResult[A] = fromEither(Right(a))
    def modify = StateT.modify[ErrorOr, ParsingCtx]

    def fromValidated[A](stmt: Statement)(fn: ValidStatements => ParserResult[A]) = {
      Grammar.validate(stmt).fold(fail(_), fn(_))
    }

    def validate(stmt: Statement): ParserResult[ValidStatements] = fromValidated(stmt)(success)
  }

  def moduleParser: PartialFunction[Statement, ParserResult[Module]] = {
    case stmt @ Statement(None, Keyword.Module.literal, Some(arg), children) =>
      ParserResult.fromValidated(stmt) { v =>
        for {
          namespace <- namespaceParser(v.required(Kw.Namespace))
          prefix <- prefixParser(v.required(Kw.Prefix))
          typeDefs <- typeDefsParser(v)
          dataDefs <- dataDefParser(v)
        } yield (Module(arg, namespace, prefix, dataDefs, typeDefs))
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
    for {
      ctx <- StateT.get
      b <- ParserResult.fromEither(
        BuiltInType
          .fromLiteral(stmt.arg.get)
          .orElse(ctx.typeDefs.find(_.name == stmt.arg.get).map(_.tpe))
          .toRight(s"Unknown type ${stmt.arg.get}")
      )
    } yield (SchemaType(stmt.arg.get, b))
  }

  def typeDefParser(stmt: Statement): ParserResult[SchemaType] = ParserResult.fromValidated(stmt) { v =>
    for {
      baseType <- typeParser(v.required(Keyword.Type))
    } yield (baseType.copy(name = stmt.arg.get))
  }

  def typeDefsParser(v: ValidStatements): ParserResult[List[SchemaType]] =
    v.many0(Keyword.TypeDef).map(typeDefParser).sequence.flatTap(typeDefs => ParserResult.modify(_.copy(typeDefs = typeDefs)))

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

  val schemaModule = moduleParser.lift

}
