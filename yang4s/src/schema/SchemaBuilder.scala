package yang4s.schema

import yang4s.parser.Statement

import SchemaBuilder.*
import cats.Applicative
import yang4s.schema.SchemaBuilder.Result.Success
import cats.Monad
import scala.annotation.tailrec

type SchemaBuilder[A] = BuildCtx => Result[A]

object SchemaBuilder {
  type Error = String

  case class BuildCtx(stmt: Statement, namespace: Namespace)
  object BuildCtx {
    extension (self: BuildCtx) {
      def focus(stmt: Statement) = self.copy(stmt = stmt)

      def toError(message: String): Error = {
        message
      }
    }

    def fromStmt(stmt: Statement): BuildCtx = BuildCtx(stmt, Namespace.DEFAULT)
  }

  enum Result[+A] {
    case Success(get: A, ctx: BuildCtx)
    case Failure(get: Error) extends Result[Nothing]
  }

  def succeed[A](a: A): SchemaBuilder[A] = ctx => Result.Success(a, ctx)
  def fail(message: String): SchemaBuilder[Nothing] = ctx => Result.Failure(ctx.toError(message))
  def modifyContext(f: BuildCtx => BuildCtx): SchemaBuilder[Unit] = { ctx => 
    Success((), f(ctx))
  }


  extension [A](self: SchemaBuilder[A]) {

    def flatMap[B](f: A => SchemaBuilder[B]): SchemaBuilder[B] = { ctx0 =>
      self(ctx0) match
        case Result.Success(get, ctx1) => f(get)(ctx1)
        case f @ Result.Failure(_)     => f
    }

    def map[B](f: A => B): SchemaBuilder[B] = flatMap(f andThen succeed)

    def orElse(other: SchemaBuilder[A]): SchemaBuilder[A] = { ctx =>
      self(ctx) match
        case Result.Failure(_) => other(ctx)
        case r                 => r
    }

    def product[B](other: SchemaBuilder[B]): SchemaBuilder[(A, B)] = self.flatMap(a => other.map(b => (a, b)))

    def build(stmt: Statement): Either[String, A] = self(BuildCtx.fromStmt(stmt)) match
      case Result.Success(get, ctx) => Right(get)
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
    
    
    // def ap[A, B](ff: SchemaBuilder[A => B])(fa: SchemaBuilder[A]): SchemaBuilder[B] =
    //   fa.flatMap(a => ff.map(_(a)))
    // def pure[A](a: A): SchemaBuilder[A] = succeed(a)
    // override def map[A, B](fa: SchemaBuilder[A])(f: A => B): SchemaBuilder[B] = fa.map(f)
  }
}
