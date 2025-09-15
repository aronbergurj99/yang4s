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
}

object Keyword {
  def fromLiteral(literal: String): Option[Keyword] = {
    Keyword.values.find(_.literal == literal)
  }
}