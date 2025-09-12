package yang4s.parser

import cats.parse.{Parser => P, Rfc5234 => Basic, Parser0}
import cats.data.NonEmptyList

trait LexicalParsers {
  // Strings
  def string = unquotedString | quotedString
  def unquotedString = (P
    .not(
      Basic.wsp | Basic.cr | Basic.lf | Basic.dquote | P.charIn("';{}") | P
        .string("//") | P.string("/*") | P.string("*/")
    )
    .with1 *> P.anyChar).rep.map(_.toList.mkString)

  def quotedString = delimeterString('"') | delimeterString('\'')
  def quotedStringChunk(delimiter: Char) =
    (escape.map(_.toString) | P.charsWhile(c =>
      c != delimiter && c != '\\'
    )).rep.map(_.toList.mkString)
  def delimeterString(delimiter: Char) =
    quoted(delimiter, quotedStringChunk(delimiter))

  // Identifiers
  val identifierInitial = ('A' to 'Z') ++ ('a' to 'z') ++ Seq('_')
  val identifierSubsequent = identifierInitial ++ "-." ++ ('0' to '9')
  def notXml = P.not(P.ignoreCase("xml"))
  def identifier = (notXml.void.with1 *> P
    .charIn(identifierInitial) ~ P.charIn(identifierSubsequent).rep0).map {
    case ((initial, subsequent)) =>
      (initial :: subsequent).mkString
  }
  def identifierString = P.recursive[String] { identifierParser =>
    quoted('"', identifierParser) | quoted('\'', identifierParser) | identifier
  }

  // Comments
  def comment = (lineComment | multiLineComment).void
  def wsComment = (comment | ws ).void.rep0
  def lineComment = P.string("//").void *> sameLineCharChunk
  def sameLineCharChunk =
    P.charWhere(c => c != '\n' && c != '\r').rep0.void *> (newLine | P.end).void
  def multiLineComment = P.recursive { comment =>
    val commentChunk =
      P.peek(P.not(P.string("*/"))).with1 ~ (comment | P.anyChar)
    P.string("/*").void *> commentChunk.rep0.void <* P.string("*/").void
  }

  // Tokens
  def token[A](parser: P[A]) = wsComment.void.with1 *> parser
  def token0[A](parser: Parser0[A]) = wsComment.void *> parser

  // Basic
  def newLine = (Basic.lf | Basic.crlf).void
  def escape = P.string("\\").void *> P.anyChar
  def quoted(quote: Char, parser: P[String]) =
    P.char(quote) *> parser <* P.char(quote)
  def ws = (Basic.wsp | newLine).void
  def end = token0(P.end).void
}