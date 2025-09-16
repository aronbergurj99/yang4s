package yang4s.schema

case class SchemaType(qName: QName, tpe: BuiltInType)

object SchemaType {
  def fromBuiltIn(builtIn: BuiltInType) = {
    SchemaType(QName.defaultNamespace(builtIn.literal), builtIn)
  }
}
