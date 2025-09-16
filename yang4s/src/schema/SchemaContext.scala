package yang4s.schema

import cats.data.NonEmptyList
import scala.util.Using
import scala.io.Source
import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*
import java.nio.file.Path
import java.io.File
import yang4s.parser.StatementParser
import scala.annotation.tailrec
import yang4s.parser.StatementParserError
import cats.implicits.{given, *}
import cats.data.StateT
import scala.util.Failure
import scala.util.Success

type SchemaError = String

case class ModuleName(name: String, revision: Option[String] = None) {
  def toFileName: String = s"${toString()}.yang"
  override def toString(): String = revision.map(r => s"$name@$r").getOrElse(name)
}

case class SchemaContext(searchPaths: Seq[String], modules: List[SchemaModule]) {
  def loadModule(moduleName: ModuleName): Either[SchemaError, (SchemaContext, SchemaModule)] = {
    findModule(moduleName.name).fold(
      findModulePath(moduleName)
        .toRight("Module not found")
        .map { p =>
          Using(Source.fromFile(p.toFile)) { source =>
            StatementParser().parse(source.mkString).flatMap { stmt =>
              parsers.moduleParser(stmt).run(ParsingCtx.fromSchemaCtx(this))
            }
          } match
            case Failure(exception) => Left(exception.toString())
            case Success(value)     => value
        }
        .getOrElse(Left(s"${moduleName.toString()} does not exist"))
        .map((pCtx, m) => (pCtx.schemaCtx.copy(modules = m :: pCtx.schemaCtx.modules), m))
    )(m => Right((this, m)))
  }

  def loadModules(moduleNames: List[ModuleName]): Either[SchemaError, (SchemaContext, List[SchemaModule])] = {
    @tailrec
    def loop(ms: List[ModuleName], ctx: SchemaContext, acc: List[SchemaModule]): Either[SchemaError, (SchemaContext, List[SchemaModule])] = {
      ms match
        case head :: next => {
          ctx.loadModule(head) match
            case Left(e) => Left(e)
            case Right(value)   => loop(next, value._1, value._2 :: acc)
        }
        case Nil => Right((ctx,acc))
    }
    loop(moduleNames, this, List.empty)
  }

  def findModule(moduleName: String): Option[SchemaModule] = {
    modules.find(_.name == moduleName)
  }

  def findModulePath(moduleName: ModuleName): Option[Path] = {
    val testing = LazyList(searchPaths.map(Paths.get(_))*).flatMap { path =>
      Using(Files.walk(path)) { stream =>
        stream.toScala(LazyList).find(p => Files.isRegularFile(p) && p.getFileName().toString == moduleName.toFileName)
      }.getOrElse(LazyList.empty)
    }
    testing.take(1).toList.headOption
  }
}

object SchemaContext {
  def empty(searchPaths: Seq[String]): SchemaContext = {
    SchemaContext(searchPaths, List.empty)
  }
}
