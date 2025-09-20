package yang4s.schema

import yang4s.schema.SchemaNodeKind.LeafNode
import yang4s.schema.SchemaNodeKind.ListNode

case class SchemaMeta(
    qName: QName,
    description: Option[String]
)

sealed trait SchemaNodeKind

object SchemaNodeKind {
  type DataDefiningKind = ListNode | ContainerNode.type
  type TerminalKind = LeafNode.type | LeafList.type

  case object ContainerNode extends SchemaNodeKind
  case class ListNode(key: Option[String]) extends SchemaNodeKind
  case object LeafNode extends SchemaNodeKind
  case object LeafList extends SchemaNodeKind
}

sealed trait SchemaNode {
  def meta: SchemaMeta

  def name = meta.qName.localName
  def namespace = meta.qName.namespace
  def description = meta.description
}

object SchemaNode {
  import SchemaNodeKind.*
  type DataNode = TerminalNode | DataDefiningNode
  case class TerminalNode(meta: SchemaMeta, tpe: SchemaType, kind: TerminalKind) extends SchemaNode
  case class DataDefiningNode(meta: SchemaMeta, dataDefs: List[DataNode], kind: DataDefiningKind)
      extends SchemaNode

  def containerNode(meta: SchemaMeta, dataDefs: List[DataNode]) = DataDefiningNode(meta, dataDefs, ContainerNode)
  def listNode(meta: SchemaMeta, dataDefs: List[DataNode], key: Option[String]) = DataDefiningNode(meta, dataDefs, ListNode(key))
  def leafNode(meta: SchemaMeta, tpe: SchemaType) = TerminalNode(meta, tpe, LeafNode)
  def leafListNode(meta: SchemaMeta, tpe: SchemaType) = TerminalNode(meta, tpe, LeafList)
}
