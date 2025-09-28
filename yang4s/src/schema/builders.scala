package yang4s.schema

import yang4s.schema.SchemaBuilder.*
import yang4s.schema.SchemaNode.{*, given}
import yang4s.parser.Statement

import cats.syntax.traverse.*
import cats.syntax.flatMap.*

import scala.util.Try
import java.net.URI

object builders {
  type Statements = List[Statement]
  case class Import(prefix: String, moduleName: String)

  // Helpers

  def isDataDefStmt(stmt: Statement): Boolean = Set(
    Keyword.Container,
    Keyword.List
  ).map(_.literal).contains(stmt.keyword)

  def kwPredicate(kw: Keyword): Statement => Boolean = { stmt =>
    stmt.keyword == kw.literal
  }

  // Substatements

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

  def optional[A](kw: Keyword, b: SchemaBuilder[A]): SchemaBuilder[Option[A]] = { ctx =>
    val resultingBuilder = ctx.stmt.substatements
      .find(_.keyword == kw.literal)
      .map(scoped(_, b))
      .sequence
    resultingBuilder(ctx)
  }

  def many[A](predicate: Statement => Boolean, b: SchemaBuilder[A]): SchemaBuilder[List[A]] = {
    ctx =>
      scopedMany(ctx.stmt.substatements.filter(predicate), b)(ctx)
  }

  // Modules

  def moduleBuilder: SchemaBuilder[Module] = {
    for {
      moduleName <- stringArg
      prefix <- required(Keyword.Prefix, prefixBuilder)
      namespace <- required(Keyword.Namespace, namespaceBuilder)
        .flatTap(ns => modifyCtx(_.copy(namespace = ns.copy(prefix = Some(prefix)))))
      imports <- many(kwPredicate(Keyword.Import), importBuilder)
        .flatTap(resolveImports)
      typeDefs <- resolveTypeDefinitions
    } yield (Module(moduleName, namespace, prefix, List.empty, List.empty, typeDefs, List.empty))
  }

  // Data Nodes

  def containerBuilder: SchemaBuilder[DataNode] = ???
  def listBuilder: SchemaBuilder[DataNode] = ???
  def dataDefBuilder: SchemaBuilder[DataNode] = containerBuilder orElse listBuilder

  // Imports

  def importBuilder: SchemaBuilder[Import] = {
    for {
      moduleName <- stringArg
      prefix <- required(Keyword.Prefix, prefixBuilder)
    } yield (Import(prefix, moduleName))
  }

  def resolveImports(imports: List[Import]): SchemaBuilder[Unit] = {
    val moduleNames = imports.map(i => ModuleName(i.moduleName, None))
    for {
      (schemaCtx, scope) <- inspectCtx(ctx => (ctx.schemaCtx, ctx.scope))
      (_, modules) <- fromEither(schemaCtx.loadModules(moduleNames), identity)
      _ <- modifyCtx(
        _.copy(
          imports = imports
            .map(_.prefix)
            .zip(modules)
            .map((p, m) => (p, m.namespace.copy(prefix = Some(p))))
            .toMap,
          scope =
            scope.copy(typeDefinitions = scope.typeDefinitions ++ modules.flatMap(_.typeDefsV2))
        )
      )
    } yield ()
  }
  // Types

  /** Builds the type definition from statement
    */
  def typeDefinitionBuilder: SchemaBuilder[TypeDefinition] =
    for {
      meta <- metaBuilder
      tpe <- required(Keyword.Type, resolveType)
    } yield (TypeDefinition(meta, tpe.builtIn))

  def resolveTypeDefFromScope(qName: QName, scope: Scope): SchemaBuilder[TypeDefinition] = { ctx =>
    val runningCtx = ctx.copy(scope = scope)

    val siblingTypeDefStmts =
      scope.stmt.substatements
        .filter(kwPredicate(Keyword.TypeDef))

    val siblingQNamesBuilder =
      siblingTypeDefStmts
        .map(scoped(_, qNameArg))
        .sequence

    val statement = siblingQNamesBuilder.map { qNames =>
      qNames
        .zip(siblingTypeDefStmts)
        .find(_._1 == qName)
        .map(_._2)
    }(runningCtx) match
      case Result.Success(get, ctx) => get
      case Result.Failure(get)      => None

    statement.fold(
      Result.Failure(ctx.toError(s"Unknown type. $qName ${ctx.scope.typeDefinitions}"))
    )(
      scoped(_, typeDefinitionBuilder)(runningCtx)
    )
  }

  /** Resolves type from ctx or parent sibling statements if not yet resolved
    */
  def resolveTypeDefinition(qName: QName, scope: Scope): SchemaBuilder[TypeDefinition] = { ctx =>
    ctx.scope.typeDefinitions
      .find(_.meta.qName == qName)
      .fold(
        resolveTypeDefFromScope(qName, scope).flatTap(td => modifyCtx(_.addTypeDefToScope(td)))
      )(succeed(_))(ctx)
  }

  def resolveTypeDefinitions: SchemaBuilder[List[TypeDefinition]] = {
    for {
      scope <- inspectCtx(_.scope)
      typeDefs <- many(kwPredicate(Keyword.TypeDef), qNameArg.flatMap(resolveTypeDefinition(_, scope)))
    } yield ({
      typeDefs
    })
  }

  /** Resolve the type definition from type statement
    */
  def resolveType: SchemaBuilder[TypeDefinition] = qNameArg.flatMap { qName =>
    inspectCtx(_.scope).flatMap { scope => 
      BuiltInType
        .fromLiteral(qName.localName)
        .fold(resolveTypeDefinition(qName, scope.parent.flatMap(_.parent).getOrElse(scope)))(b => succeed(TypeDefinition.fromBuiltIn(b)))
    }
  }

  // Misc

  def namespaceBuilder: SchemaBuilder[Namespace] = uriArg.map(Namespace(_, None))
  def prefixBuilder: SchemaBuilder[String] = stringArg

  // Schema Meta Builders

  def metaBuilder: SchemaBuilder[SchemaMeta] = for {
    qName <- qNameArg
  } yield (SchemaMeta(qName, None, false, Status.Current, List.empty))

  // Arg Builders

  def uriArg: SchemaBuilder[URI] = { ctx =>
    Try(URI(ctx.stmt.arg.get))
      .fold(_ => Result.Failure(ctx.toError("Not a valid URI.")), Result.Success(_, ctx))
  }
  def stringArg: SchemaBuilder[String] = { ctx =>
    ctx.stmt.arg.fold(Result.Failure(ctx.toError("Expected an argument.")))(Result.Success(_, ctx))
  }
  def qNameArg: SchemaBuilder[QName] = stringArg.flatMap { arg =>
    val (prefixOption, identifier): (Option[String], String) = arg.split(":", 2) match
      case Array(prefix, identifier) => (Some(prefix), identifier)
      case _                         => (None, arg)

    val namespaceBuilder = getCtx.flatMap { ctx =>
      prefixOption.fold(Some(ctx.namespace))(ctx.resolvePrefix) match
        case Some(value) => succeed(value)
        case None        => fail(ctx.toError("Not a valid identifier."))
    }

    namespaceBuilder.map(QName(_, identifier))
  }
}
