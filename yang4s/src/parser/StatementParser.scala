package yang4s.parser

import cats.parse.Parser

type StatementParserError = String


trait StatementParser {
  def parse(source: String): Either[StatementParserError, Statement]
}

object StatementParser {
  private final class Impl
      extends StatementParser
      with StatementParsers
      with LexicalParsers {
    private def document = statement <* end.void

    def parse(source: String): Either[StatementParserError, Statement] =
      document
        .parseAll(source)
        .fold(e => Left(e.toString()), Right(_))
  }

  def apply(): StatementParser = new Impl
}
