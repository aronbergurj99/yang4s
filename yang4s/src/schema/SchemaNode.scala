package yang4s.schema

case class SchemaMeta(
    qName: QName,
    description: Option[String]
)

sealed trait SchemaNode {
  def meta: SchemaMeta
  def dataDefs: List[SchemaNode]

  def name = meta.qName.localName
  def namespace = meta.qName.namespace
  def description = meta.description
}

case class ContainerNode(meta: SchemaMeta, dataDefs: List[SchemaNode]) extends SchemaNode
case class ListNode(meta: SchemaMeta, dataDefs: List[SchemaNode], key: Option[String]) extends SchemaNode

case class LeafNode(meta: SchemaMeta, dataDefs: List[SchemaNode], tpe: SchemaType) extends SchemaNode
