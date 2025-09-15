package yang4s.parser

import cats.parse.Parser
import cats.syntax.show._  // for .show

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
        .fold(e => Left(e.show), Right(_))
  }

  def apply(): StatementParser = new Impl
}
