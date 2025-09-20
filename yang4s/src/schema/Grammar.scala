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
    stmts: List[Tuple2[Keyword, Statement]],
    unknowns: List[Statement]
) {
  def required(kw: Keyword): Statement = find(kw).get
  def optional(kw: Keyword): Option[Statement] = find(kw)
  def many0(kw: Keyword): List[Statement] = query(kw)

  def filter(predicate: Keyword => Boolean): List[Tuple2[Keyword, Statement]] = stmts.filter(v => predicate(v._1))
  def find(kw: Keyword): Option[Statement] = query(kw).lift(0)
  def query(kw: Keyword): List[Statement] = filter(_ == kw).map(_._2)
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
      Keyword.Prefix -> Grammar(required),
      Keyword.YangVersion -> Grammar(optional),
      Keyword.Organization -> Grammar(optional),
      Keyword.Description -> Grammar(optional),
      Keyword.Revision -> Grammar(many0()),
      Keyword.Contact -> Grammar(optional),
      Keyword.Import -> Grammar(many0()),
      Keyword.Container -> Grammar(many0()),
      Keyword.List -> Grammar(many0()),
      Keyword.TypeDef -> Grammar(many0()),
      Keyword.Identity -> Grammar(many0()),
      Keyword.Feature -> Grammar(many0()),
    )),
    Keyword.Container -> (ValidateArgument.identity, Map(
      Keyword.Container -> Grammar(many0()),
      Keyword.List -> Grammar(many0()),
      Keyword.Leaf -> Grammar(many0()),
      Keyword.Config -> Grammar(optional),
      Keyword.Status -> Grammar(optional),
      Keyword.Description -> Grammar(optional),
    )),
    Keyword.List -> (ValidateArgument.identity, Map(
      Keyword.Container -> Grammar(many0()),
      Keyword.List -> Grammar(many0()),
      Keyword.Leaf -> Grammar(many0()),
      Keyword.LeafList -> Grammar(many0()),
      Keyword.Key -> Grammar(optional),
      Keyword.Config -> Grammar(optional),
      Keyword.Status -> Grammar(optional),
      Keyword.Description -> Grammar(optional),
    )),
    Keyword.Leaf -> (ValidateArgument.identity, Map(
      Keyword.Type -> Grammar(required),
      Keyword.Container -> Grammar(many0()),
      Keyword.List -> Grammar(many0()),
      Keyword.Leaf -> Grammar(many0()),
      Keyword.Config -> Grammar(optional),
      Keyword.Status -> Grammar(optional),
      Keyword.Description -> Grammar(optional),
      Keyword.Reference -> Grammar(optional),
      Keyword.Mandatory -> Grammar(optional),
      Keyword.Units -> Grammar(optional),
      Keyword.IfFeature -> Grammar(optional),
      Keyword.Default -> Grammar(optional),
    )),
    Keyword.LeafList -> (ValidateArgument.identity, Map(
      Keyword.Type -> Grammar(required),
      Keyword.Container -> Grammar(many0()),
      Keyword.List -> Grammar(many0()),
      Keyword.Leaf -> Grammar(many0()),
      Keyword.Config -> Grammar(optional),
      Keyword.Status -> Grammar(optional),
      Keyword.Description -> Grammar(optional),
      Keyword.Reference -> Grammar(optional),
      Keyword.Mandatory -> Grammar(optional),
      Keyword.Units -> Grammar(optional),
      Keyword.IfFeature -> Grammar(optional),
      Keyword.Default -> Grammar(optional),
    )),
    Keyword.Type -> (ValidateArgument.identity, Map(
      Keyword.Length -> Grammar(optional),
      Keyword.Pattern -> Grammar(optional),
      Keyword.Units -> Grammar(optional),
      Keyword.Path -> Grammar(optional),
      Keyword.Range -> Grammar(optional),
      Keyword.Enum -> Grammar(optional),
      Keyword.Base -> Grammar(optional),
    )),
    Keyword.Key -> (ValidateArgument.identity, Map()),
    Keyword.TypeDef -> (ValidateArgument.identity, Map(
      Keyword.Type -> Grammar(required),
      Keyword.Description -> Grammar(optional),
      Keyword.Reference -> Grammar(optional),
      Keyword.Units -> Grammar(optional),
      Keyword.Default -> Grammar(optional),
      Keyword.Status -> Grammar(optional),
    )),
    Keyword.Namespace -> (ValidateArgument.identity, Map()),
    Keyword.Prefix -> (ValidateArgument.identity, Map()),
    Keyword.Import -> (ValidateArgument.identity, Map(
      Keyword.Prefix -> Grammar(required)
    )),
    Keyword.Organization -> (ValidateArgument.identity, Map()),
    Keyword.Contact -> (ValidateArgument.identity, Map()),
    Keyword.Description -> (ValidateArgument.identity, Map()),
    // Todo validate revision arg correctly)
    Keyword.Revision -> (ValidateArgument.identity, Map()),
    Keyword.Reference -> (ValidateArgument.identity, Map()),
    Keyword.Length -> (ValidateArgument.identity, Map()),
    Keyword.Pattern -> (ValidateArgument.identity, Map()),
    Keyword.Units -> (ValidateArgument.identity, Map()),
    Keyword.Default -> (ValidateArgument.identity, Map()),
    //Todo: validate yang version arg
    Keyword.YangVersion -> (ValidateArgument.identity, Map()),
    Keyword.Identity -> (ValidateArgument.identity, Map()),
    Keyword.Feature -> (ValidateArgument.identity, Map()),
    Keyword.Status -> (ValidateArgument.identity, Map()),
    Keyword.Path -> (ValidateArgument.identity, Map()),
    Keyword.Config -> (ValidateArgument.identity, Map()),
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
        .foldM[Either[String, _], (List[Tuple2[Keyword, Statement]], List[Statement], Int)](
          (List.empty, List.empty, 0)
        ) { case ((m, unknowns, section), stmt1) =>
          stmt1 match
            // Unknown statemnet
            case Statement(Some(prefix), _, _, _) =>
              Right((m, stmt1 :: unknowns, section))
            case Statement(None, keyword, _, _) =>
              Keyword
                .fromLiteral(keyword).toRight(s"$keyword is not a valid keyword.")
                .flatMap(kw => rules.lift(kw).map((_, kw)).toRight(s"${kw.literal} is not a valid substatement of ${stmt.keyword}"))
                .map { (g, kw) =>
                  ((kw, stmt1) :: m, unknowns, g.section)
                }
        }
        .map(r => (r._1, r._2))
    } yield (ValidStatements(validStatements._1, validStatements._2))
    // Todo: Validate cardinality
  }
}
