package yang4s.schema

import yang4s.parser.Statement

import SchemaBuilder.*
import cats.Applicative
import yang4s.schema.SchemaBuilder.Result.Success
import cats.Monad
import scala.annotation.tailrec
import yang4s.schema.SchemaNode.TypeDefinition

type SchemaBuilder[A] = BuildCtx => Result[A]

object SchemaBuilder {
  type Error = String
  type Prefix = String

  case class BuildCtx(
      namespace: Namespace,
      scope: Scope,
      schemaCtx: SchemaContext,
      imports: Map[Prefix, Namespace]
  ) {
    def stmt = scope.stmt
  }

  object BuildCtx {
    extension (self: BuildCtx) {
      def focus(stmt: Statement) = self.copy(scope = self.scope.child(stmt))

      def toError(message: String): Error = {
        message
      }

      def addTypeDefToScope(td: TypeDefinition): BuildCtx = {
        self.copy(scope = self.scope.copy(typeDefinitions = td :: self.scope.typeDefinitions))
      }

      def resolvePrefix(prefix: Prefix): Option[Namespace] = {
        self.imports
          .get(prefix)
          .orElse(self.namespace.prefix.filter(_ == prefix).map(_ => self.namespace))
      }
    }

    def fromStmt(stmt: Statement, schemaCtx: SchemaContext): BuildCtx =
      BuildCtx(Namespace.DEFAULT, Scope.fromStmt(stmt), schemaCtx, Map.empty)

  }

  enum Result[+A] {
    case Success(get: A, ctx: BuildCtx)
    case Failure(get: Error) extends Result[Nothing]

  }

  def succeed[A](a: A): SchemaBuilder[A] = ctx => Result.Success(a, ctx)
  def fail(message: String): SchemaBuilder[Nothing] = ctx => Result.Failure(ctx.toError(message))
  def modifyCtx(f: BuildCtx => BuildCtx): SchemaBuilder[Unit] = { ctx =>
    Success((), f(ctx))
  }
  def getCtx: SchemaBuilder[BuildCtx] = ctx => Success(ctx, ctx)
  def inspectCtx[A](f: BuildCtx => A): SchemaBuilder[A] = getCtx.map(f)

  def fromEither[A, B](either: Either[A, B], f: A => Error): SchemaBuilder[B] = {
    either match
      case Left(value)  => fail(f(value))
      case Right(value) => succeed(value)
  }

  extension [A](self: SchemaBuilder[A]) {

    def flatMap[B](f: A => SchemaBuilder[B]): SchemaBuilder[B] = { ctx0 =>
      self(ctx0) match
        case Result.Success(get, ctx1) => f(get)(ctx1)
        case f @ Result.Failure(_)     => f
    }

    def map[B](f: A => B): SchemaBuilder[B] = flatMap(f andThen succeed)

    infix def orElse(other: SchemaBuilder[A]): SchemaBuilder[A] = { ctx =>
      self(ctx) match
        case Result.Failure(_) => other(ctx)
        case r                 => r
    }

    def product[B](other: SchemaBuilder[B]): SchemaBuilder[(A, B)] =
      self.flatMap(a => other.map(b => (a, b)))

    def build(stmt: Statement, schemaCtx: SchemaContext): Either[String, (BuildCtx, A)] = self(
      BuildCtx.fromStmt(stmt, schemaCtx)
    ) match
      case Result.Success(get, ctx) => Right((ctx, get))
      case Result.Failure(get)      => Left(get)
  }

  given Monad[SchemaBuilder] with {
    def pure[A](x: A): SchemaBuilder[A] = succeed(x)
    def flatMap[A, B](fa: SchemaBuilder[A])(f: A => SchemaBuilder[B]): SchemaBuilder[B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => SchemaBuilder[Either[A, B]]): SchemaBuilder[B] = { ctx =>
      @tailrec
      def loop(current: A): Result[B] =
        f(current)(ctx) match {
          case Success(get, _) =>
            get match {
              case Left(nextA)  => loop(nextA)
              case Right(value) => SchemaBuilder.succeed(value)(ctx)
            }
          case failure @ Result.Failure(_) => failure
        }

      loop(a)
    }
  }
}
