package yang4s.schema

import yang4s.parser.Statement

sealed trait SchemaModule {
  def dataDefs: List[SchemaNode]
}
case class Module(name: String, namespace: String, prefix: String, dataDefs: List[SchemaNode], typeDefs: List[SchemaType]) extends SchemaModule
case class SubModule(name: String, namespace: String, prefix: String, dataDefs: List[SchemaNode]) extends SchemaModule
