package yang4s.schema

case class SchemaNode(
  name: String,
  namespace: String,
  description: Option[String],
  children: List[SchemaNode],
  kind: SchemaKind
)
