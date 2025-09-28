package yang4s.schema

enum BuiltInType(val literal: String) {
  case Binary extends BuiltInType("binary")
  case Bits extends BuiltInType("bits")
  case Boolean extends BuiltInType("boolean")
  case Decimal64 extends BuiltInType("decimal64")
  case Empty extends BuiltInType("empty")
  case Enumeration extends BuiltInType("enumeration")
  case Identityref extends BuiltInType("identityref")
  case InstanceIdentifier extends BuiltInType("instance-identifier")
  case Int8 extends BuiltInType("int8")
  case Int16 extends BuiltInType("int16")
  case Int32 extends BuiltInType("int32")
  case Int64 extends BuiltInType("int64")
  case LeafRef extends BuiltInType("leafref")
  case String extends BuiltInType("string")
  case UInt8 extends BuiltInType("uint8")
  case UInt16 extends BuiltInType("uint16")
  case UInt32 extends BuiltInType("uint32")
  case UInt64 extends BuiltInType("uint64")
  case Union extends BuiltInType("union")

  def qName: QName = QName(Namespace.DEFAULT, literal)
}

object BuiltInType {
  def fromLiteral(literal: String): Option[BuiltInType] = {
    BuiltInType.values.find(_.literal == literal)
  }

  def fromQName(qName: QName): Option[BuiltInType] = 
    BuiltInType.values.find(_.qName == qName)

  def isBuiltin(literal: String): Boolean = fromLiteral(literal).fold(false)(_ => true)
}