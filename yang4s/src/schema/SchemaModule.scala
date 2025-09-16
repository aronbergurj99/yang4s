package yang4s.schema

import yang4s.parser.Statement

sealed trait SchemaModule {
  def name: String
  def typeDefs: List[SchemaType]
  def dataDefs: List[SchemaNode]
  def namespace: Namespace
}
case class Module(name: String, namespace: Namespace, prefix: String, dataDefs: List[SchemaNode], typeDefs: List[SchemaType]) extends SchemaModule
case class SubModule(name: String, namespace: Namespace, prefix: String, dataDefs: List[SchemaNode], typeDefs: List[SchemaType]) extends SchemaModule
