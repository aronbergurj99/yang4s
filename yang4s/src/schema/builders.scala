package yang4s.schema
import yang4s.schema.SchemaBuilder.*
import yang4s.parser.Statement

import cats.syntax.traverse.*
import cats.syntax.flatMap.*
import yang4s.schema.SchemaNode.*
import scala.util.Success
import scala.util.Try
import java.net.URI
import scala.util.Failure

object builders {
  type Statements = List[Statement]

  /** Scopes the ctx to the provided stmt
    */
  def scoped[A](stmt: Statement, b: SchemaBuilder[A]): SchemaBuilder[A] = { ctx =>
    b(ctx.focus(stmt)) match
      case f @ Result.Failure(_) => f
      case r: Result.Success[A]  => r.copy(ctx = ctx)
  }

  def scopedMany[A](stmts: Statements, b: SchemaBuilder[A]): SchemaBuilder[List[A]] = {
    stmts.map(scoped(_, b)).sequence
  }

  def required[A](kw: Keyword, b: SchemaBuilder[A]): SchemaBuilder[A] = { ctx =>
    ctx.stmt.substatements
      .find(_.keyword == kw.literal)
      .fold(Result.Failure(ctx.toError("Required statement missing.")))(scoped(_, b)(ctx))
  }

  def optional(kw: Keyword): SchemaBuilder[Option[Statement]] = { ctx =>
    Result.Success(ctx.stmt.substatements.find(_.keyword == kw.literal), ctx)
  }

  def moduleBuilder: SchemaBuilder[Module] = {
    for {
      moduleName <- stringArg
      namespace  <- required(Keyword.Namespace, namespaceBuilder)
                      .flatTap(ns => modifyContext(_.copy(namespace = ns)))
      prefix     <- required(Keyword.Prefix, prefixBuilder)
    } yield (Module(moduleName, namespace, prefix, List.empty, List.empty, List.empty))
  }

  def namespaceBuilder: SchemaBuilder[Namespace] = uriArg.map(Namespace(_, None))
  def prefixBuilder: SchemaBuilder[String] = stringArg

  def uriArg: SchemaBuilder[URI] = { ctx =>
    Try(URI(ctx.stmt.arg.get)).fold(_ => Result.Failure(ctx.toError("Not a valid URI.")), Result.Success(_, ctx))
  }

  def stringArg: SchemaBuilder[String] = { ctx =>
    ctx.stmt.arg.fold(Result.Failure(ctx.toError("Expected an argument.")))(Result.Success(_, ctx))
  }
}
