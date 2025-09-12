package yang4s.parser

import cats.parse.{Parser => P}

trait StatementParsers { self: LexicalParsers =>
  def prefix = identifier
  def prefixedKeyword = identifier ~ (P.char(':').void *> identifier)
  def keyword = token(
    prefixedKeyword.backtrack.map({ case (prefix, keyword) =>
      (Some(prefix), keyword)
    }) |
      identifier.map(v => (Option.empty[String], v))
  )

  def argument = token0(string.?)

  def statement = P.recursive[Statement] { stmtP =>
    (keyword ~ argument ~ (P.char(';').void.as(List.empty[Statement]) | token(
      P.char('{')
    ) *>
      stmtP.backtrack.rep0
      <* token(P.char('}')))).map {
      case (((optPrefix, kw), optArg), children) =>
        Statement(optPrefix, kw, optArg, children)
    }
  }
}