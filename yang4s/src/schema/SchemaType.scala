package yang4s.schema

enum SchemaType(val literal: String) {
  case StringType extends SchemaType("string")
}
