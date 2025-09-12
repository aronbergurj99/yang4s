package yang4s.schema

case class SchemaNode(
  name: Option[String],
  namespace: Option[String],
  description: Option[String],
  children: Seq[SchemaNode],
  kind: SchemaKind
)
