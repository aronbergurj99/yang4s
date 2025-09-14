package yang4s.schema

enum BuiltInType(val literal: String) {
  case StringType extends BuiltInType("string")
}

object BuiltInType {
  def fromLiteral(literal: String): Option[BuiltInType] = {
    BuiltInType.values.find(_.literal == literal)
  }

  def isBuiltin(literal: String): Boolean = fromLiteral(literal).fold(false)(_ => true)
}

case class SchemaType(name: String, tpe: BuiltInType)
