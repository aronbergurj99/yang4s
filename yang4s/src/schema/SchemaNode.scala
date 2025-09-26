package yang4s.schema

import yang4s.schema.SchemaNodeKind.LeafNode
import yang4s.schema.SchemaNodeKind.ListNode


enum Status(val literal: String) {
  case Current extends Status("current")
  case Deprecated extends Status("deprecated")
  case Obsolete extends Status("Obsolete")
} 
object Status {
  def fromLiteral(literal: String): Option[Status] = {
    Status.values.find(_.literal == literal)
  }
}

case class SchemaMeta(
    qName: QName,
    description: Option[String],
    config: Boolean,
    status: Status,
    ifFeatures: List[QName],
)

sealed trait SchemaNodeKind

object SchemaNodeKind {
  type DataDefiningKind = ListNode | ContainerNode.type
  type TerminalKind = LeafNode | LeafList.type

  case object ContainerNode extends SchemaNodeKind
  case class ListNode(key: QName) extends SchemaNodeKind
  case class LeafNode(mandatory: Boolean) extends SchemaNodeKind
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

  case class TypeDefinition(meta: SchemaMeta, builtIn: BuiltInType) extends SchemaNode {
    def fromBuiltIn(builtIn: BuiltInType): TypeDefinition = TypeDefinition(
      SchemaMeta(QName.defaultNamespace(builtIn.literal), None, false, Status.Current, List.empty), builtIn)
  }

  case class FeatureDefinition(meta: SchemaMeta) extends SchemaNode

  def containerNode(meta: SchemaMeta, dataDefs: List[DataNode]) = DataDefiningNode(meta, dataDefs, ContainerNode)
  def listNode(meta: SchemaMeta, dataDefs: List[DataNode], key: QName) = DataDefiningNode(meta, dataDefs, ListNode(key))
  def leafNode(meta: SchemaMeta, tpe: SchemaType, mandatory: Boolean) = TerminalNode(meta, tpe, LeafNode(mandatory))
  def leafListNode(meta: SchemaMeta, tpe: SchemaType) = TerminalNode(meta, tpe, LeafList)

  extension (self: SchemaNode) {
    def mandatory: Boolean = {
      self match
        case TerminalNode(meta, tpe, kind) => kind match
          case LeafNode(mandatory) => mandatory
          case LeafList => false
        case DataDefiningNode(meta, dataDefs, kind) => dataDefs.find(_.mandatory).fold(false)(_ => true)
        case _ => false
    }
  }
}
