package yang4s.schema

enum Keyword(val literal: String) {
  case Module extends Keyword("module")
  case Namespace extends Keyword("namespace")
  case Prefix extends Keyword("prefix")
}

object Keyword {
  def fromLiteral(literal: String): Option[Keyword] = {
    Keyword.values.find(_.literal == literal)
  }
}