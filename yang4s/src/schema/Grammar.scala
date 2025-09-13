package yang4s.schema

import yang4s.parser.Statement
import yang4s.schema.Cardinality.required
import yang4s.schema.Cardinality.CardinalityError
import cats.data.Validated.Valid
import cats.implicits.{*, given}

enum Version(val literal: String) {
  case Rfc6020 extends Version("1")
  case Rrf7950 extends Version("1.1")
}

type ValidationResult = Either[CardinalityError, ValidStatements]

case class ValidStatements(
    stmts: Map[Keyword, List[Statement]],
    unknowns: List[Statement]
) {
  def required(kw: Keyword): Statement = stmts(kw)(0)
  def optional(kw: Keyword): Option[Statement] = stmts(kw).lift(0)
  def many0(kw: Keyword): List[Statement] = stmts(kw)
}

case class Cardinality(min: Int, max: Option[Int]) {
  import Cardinality.*

  def validate(
      stmts: List[Statement]
  ): Either[CardinalityError, List[Statement]] = {
    val cardinality = stmts.length

    val validMax: Boolean = max.map(cardinality <= _).getOrElse(true)
    val validMin: Boolean = cardinality >= min

    val valid = validMin && validMax
    if (valid) Right(stmts)
    else Left(toError(cardinality))
  }

  def toError(observed: Int): CardinalityError =
    CardinalityError(min, max, observed)
}

object Cardinality {
  case class CardinalityError(min: Int, max: Option[Int], observed: Int)
  def required: Cardinality = Cardinality(1, Some(1))
  def optional: Cardinality = Cardinality(0, Some(1))
  def many0(max: Option[Int] = None): Cardinality = Cardinality(0, max)
  def many(min: Int = 1, max: Option[Int] = None): Cardinality =
    Cardinality(min, max)
}

case class Grammar(cardinality: Cardinality, section: Int = 0)
type Rules = Map[Keyword, Grammar]
type ValidateArgument = Option[String] => Either[String, Unit]
object ValidateArgument {
  def identity: ValidateArgument = { arg =>
    arg.toRight("Missing argument").map(_ => ())
  }
}
object Grammar {
  import Cardinality.*
  private val rfc6020: Map[Keyword, (ValidateArgument, Rules)] = Map(
    Keyword.Module -> (ValidateArgument.identity, Map(
      Keyword.Namespace -> Grammar(required),
      Keyword.Prefix    -> Grammar(required),
      Keyword.Container -> Grammar(many0()),
    )),
    Keyword.Container -> (ValidateArgument.identity, Map(
      Keyword.List -> Grammar(many0()),
    )),
    Keyword.List -> (ValidateArgument.identity, Map())
  )

  def getGrammarDef(kw: Keyword, version: Version): (ValidateArgument, Rules) = {
    rfc6020(kw)
  }

  def validate(
      stmt: Statement,
      version: Version = Version.Rfc6020
  ): Either[String, ValidStatements] = {

    val (v, rules) = getGrammarDef(Keyword.fromLiteral(stmt.keyword).get, version)

    for {
      _ <- v(stmt.arg)
      validStatements <- stmt.substatements
        .foldM[Either[String, _], (Map[Keyword, List[Statement]], List[Statement], Int)](
          (Map.empty, List.empty, 0)
        ) { case ((m, unknowns, section), stmt1) =>
          stmt1 match
            // Unknown statemnet
            case Statement(Some(prefix), _, _, _) =>
              Right((m, stmt1 :: unknowns, section))
            case Statement(None, keyword, _, _) =>
              // Todo: Refine errors
              // Unknown keyword vs keyword not in current grammar and check section based on g
              Keyword
                .fromLiteral(keyword)
                .flatMap(kw => rules.lift(kw).map((_, kw)))
                .fold(Left("Unknown keyword")) { (g, kw) =>
                  Right((m.+((kw, stmt1 :: m.lift(kw).getOrElse(List.empty))), unknowns, g.section))
                }
        }
        .map(r => (r._1, r._2))
    } yield (ValidStatements(validStatements._1, validStatements._2))
    // Todo: Validate cardinality
  }
}
