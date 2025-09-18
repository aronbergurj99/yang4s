package yang4s.schema

enum Keyword(val literal: String) {
  case Module extends Keyword("module")
  case Namespace extends Keyword("namespace")
  case Prefix extends Keyword("prefix")
  case Container extends Keyword("container")
  case List extends Keyword("list")
  case Leaf extends Keyword("leaf")
  case Type extends Keyword("type")
  case TypeDef extends Keyword("typedef")
  case Key extends Keyword("key")
  case Import extends Keyword("import")
  case Organization extends Keyword("organization")
  case Contact extends Keyword("contact")
  case Description extends Keyword("description")
  case Revision extends Keyword("revision")
  case Reference extends Keyword("reference")
  case Default extends Keyword("default")
  case Pattern extends Keyword("pattern")
  case Units extends Keyword("units")
  case Status extends Keyword("status")
  case Length extends Keyword("length")
  case YangVersion extends Keyword("yang-version")
  case Identity extends Keyword("identity")
  case Feature extends Keyword("feature")
  case Path extends Keyword("path")
  case Config extends Keyword("config")
  case LeafList extends Keyword("leaf-list")
  case Mandatory extends Keyword("mandatory")
  case IfFeature extends Keyword("if-feature")
  case Range extends Keyword("range")
  case Enum extends Keyword("enum")
  case Base extends Keyword("base")
}

object Keyword {
  def fromLiteral(literal: String): Option[Keyword] = {
    Keyword.values.find(_.literal == literal)
  }
}