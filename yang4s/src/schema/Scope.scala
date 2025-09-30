package yang4s.schema

import yang4s.schema.SchemaNode.TypeDefinition
import yang4s.parser.Statement

case class Scope(
    stmt: Statement,
    typeDefinitions: List[TypeDefinition],
    parent: Option[Scope],
    config: Boolean
) {
  def child(stmt: Statement): Scope = copy(stmt = stmt, parent = Some(this))
}

object Scope {
  def fromStmt(stmt: Statement): Scope = Scope(stmt, List.empty, None, true)
}
