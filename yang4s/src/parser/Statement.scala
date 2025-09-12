package yang4s.parser

final case class Statement(prefix: Option[String], keyword: String, arg: Option[String], substatements: List[Statement])