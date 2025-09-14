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
  type ErrorOr[A] = Either[String, A]
  type ParserResult[A] = StateT[ErrorOr, ParsingCtx, A]

  object ParserResult {
    def lift[A](v: ErrorOr[A]): ParserResult[A] = StateT.liftF(v)
    def modify = StateT.modify[ErrorOr, ParsingCtx]
  }

  def moduleParser: PartialFunction[Statement, ParserResult[Module]] = {
    case stmt @ Statement(None, Keyword.Module.literal, Some(arg), children) => {
      for {
        v <- ParserResult.lift(Grammar.validate(stmt))
        namespace <- namespaceParser(v.required(Kw.Namespace))
        _ <- ParserResult.modify(_.copy(namespace = namespace))
        prefix <- prefixParser(v.required(Kw.Prefix))
        typeDefs <- v.many0(Keyword.TypeDef).map(typeDefParser).sequence
        _ <- ParserResult.modify(_.copy(typeDefs = typeDefs))
        dataDefs <- dataDefParser(v)
      } yield (Module(arg, namespace, prefix, dataDefs, typeDefs))
    }
  }

  def testing: ParserResult[String] = StateT.liftF(Right("testing"))

  // Todo: Validate through grammar.
  def namespaceParser(stmt: Statement): ParserResult[String] =
    StateT.liftF(stmt.arg.toRight("Arguement required for namespace"))

  // Todo: Validate through grammar.
  def prefixParser(stmt: Statement): ParserResult[String] = StateT.liftF(stmt.arg.toRight("Arguement required for prefix"))

  def containerParser(stmt: Statement): ParserResult[SchemaNode] = {
    for {
      v <- ParserResult.lift(Grammar.validate(stmt))
      ctx <- StateT.get
      dataDefs <- dataDefParser(v)
    } yield (ContainerNode(SchemaMeta(stmt.arg.get, ctx.namespace, None), dataDefs))
  }

  def listParser(stmt: Statement): ParserResult[SchemaNode] = {
    for {
      v <- ParserResult.lift(Grammar.validate(stmt))
      ctx <- StateT.get
      dataDefs <- dataDefParser(v)
      key <- ParserResult.lift(Right(v.optional(Keyword.Key))).flatMap(_.map(keyParser).sequence)
    } yield (ListNode(SchemaMeta(stmt.arg.get, ctx.namespace, None), dataDefs, key))
  }

  def keyParser(stmt: Statement): ParserResult[String] = {
    for {
      v <- ParserResult.lift(Grammar.validate(stmt))
    } yield (stmt.arg.get)
  }

  def LeafParser(stmt: Statement): ParserResult[SchemaNode] = {
    for {
      v <- ParserResult.lift(Grammar.validate(stmt))
      ctx <- StateT.get
      dataDefs <- dataDefParser(v)
      tpe <- typeParser(v.required(Keyword.Type))
    } yield (LeafNode(SchemaMeta(stmt.arg.get, ctx.namespace, None), dataDefs, tpe))
  }

  def typeParser(stmt: Statement): ParserResult[SchemaType] = {
    for {
      v <- ParserResult.lift(Grammar.validate(stmt))
      ctx <- StateT.get
      b <- ParserResult.lift(
        BuiltInType
          .fromLiteral(stmt.arg.get)
          .orElse(ctx.typeDefs.find(_.name == stmt.arg.get).map(_.tpe))
          .toRight(s"Unknown type $stmt.arg.get")
      )
    } yield (SchemaType(stmt.arg.get, b))
  }

  def typeDefParser(stmt: Statement): ParserResult[SchemaType] = {
    for {
      v <- ParserResult.lift(Grammar.validate(stmt))
      baseType <- typeParser(v.required(Keyword.Type))
    } yield (baseType.copy(name = stmt.arg.get))
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

  val schemaModule = moduleParser.lift

}
