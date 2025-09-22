package yang4s.schema

import yang4s.parser.Statement
import yang4s.schema.SchemaNode.DataNode
import yang4s.schema.SchemaNode.FeatureDefinition

sealed trait SchemaModule {
  def name: String
  def typeDefs: List[SchemaType]
  def dataDefs: List[DataNode]
  def namespace: Namespace

  def isImplemented: Boolean
}
case class Module(name: String, namespace: Namespace, prefix: String, dataDefs: List[SchemaNode.DataNode], typeDefs: List[SchemaType], features: List[FeatureDefinition])
    extends SchemaModule {
  def isImplemented: Boolean = !dataDefs.isEmpty
}
case class SubModule(name: String, namespace: Namespace, prefix: String, dataDefs: List[SchemaNode.DataNode], typeDefs: List[SchemaType])
    extends SchemaModule {
  def isImplemented: Boolean = false
}
