package yang4s.schema

case class SchemaMeta(
    name: String,
    namespace: String,
    description: Option[String]
)

sealed trait SchemaNode {
  def meta: SchemaMeta
  def dataDefs: List[SchemaNode]

  def name = meta.name
  def namespace = meta.namespace
  def description = meta.description
}

case class ContainerNode(meta: SchemaMeta, dataDefs: List[SchemaNode]) extends SchemaNode
case class ListNode(meta: SchemaMeta, dataDefs: List[SchemaNode]) extends SchemaNode
