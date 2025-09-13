package yang4s.schema

import yang4s.parser.Statement

import yang4s.schema.{Keyword => Kw}
import cats.data.State
import cats.data.StateT
import cats.data.EitherT
import cats.implicits.{*, given}
import cats.parse.Parser
import cats.Applicative

case class ParsingCtx(namespace: String)

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
        dataDefs <- dataDefParser(v)
      } yield (Module(arg, namespace, prefix, dataDefs))
    }
  }

  def testing: ParserResult[String] = StateT.liftF(Right("testing"))

  def namespaceParser(stmt: Statement): ParserResult[String] =
    StateT.liftF(stmt.arg.toRight("Arguement required for namespace"))

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
    } yield (ListNode(SchemaMeta(stmt.arg.get, ctx.namespace, None), List.empty))
  }

  def dataDefParser(vStmts: ValidStatements): ParserResult[List[SchemaNode]] = {
    // Todo: We should maintain order based on definition in source file.
    Seq(
        (Keyword.Container, containerParser),
        (Keyword.List, listParser),
      ).foldLeft[List[ParserResult[SchemaNode]]](List.empty) { case (acc, (kw, fn)) =>
        acc.concat(vStmts.stmts.lift(kw).getOrElse(List.empty).map(fn))
      }
      .sequence
  }

  val schemaModule = moduleParser.lift

}
