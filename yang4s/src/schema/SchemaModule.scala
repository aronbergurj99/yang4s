package yang4s.schema

import yang4s.parser.Statement
import yang4s.schema.SchemaNode.*

sealed trait SchemaModule {
  def name: String
  def typeDefs: List[SchemaType]
  def typeDefsV2: List[TypeDefinition]
  def dataDefs: List[DataNode]
  def namespace: Namespace

  def isImplemented: Boolean
}
case class Module(name: String, namespace: Namespace, prefix: String, dataDefs: List[SchemaNode.DataNode], typeDefs: List[SchemaType], typeDefsV2: List[TypeDefinition], features: List[FeatureDefinition])
    extends SchemaModule {
  def isImplemented: Boolean = !dataDefs.isEmpty
}
case class SubModule(name: String, namespace: Namespace, prefix: String, dataDefs: List[SchemaNode.DataNode], typeDefs: List[SchemaType], typeDefsV2: List[TypeDefinition])
    extends SchemaModule {
  def isImplemented: Boolean = false
}
