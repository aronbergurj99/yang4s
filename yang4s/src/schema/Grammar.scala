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

object Grammar {
  type Valid = Unit
  type Error = String

  import Cardinality.*
  private val rfc6020: Map[Keyword, Rules] = Map(
    Keyword.Module -> Map(
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
      Keyword.Feature -> Grammar(many0())
    ),
    Keyword.Container -> Map(
      Keyword.Container -> Grammar(many0()),
      Keyword.List -> Grammar(many0()),
      Keyword.Leaf -> Grammar(many0()),
      Keyword.Config -> Grammar(optional),
      Keyword.Status -> Grammar(optional),
      Keyword.Description -> Grammar(optional)
    ),
    Keyword.List -> Map(
      Keyword.Container -> Grammar(many0()),
      Keyword.List -> Grammar(many0()),
      Keyword.Leaf -> Grammar(many0()),
      Keyword.LeafList -> Grammar(many0()),
      Keyword.Key -> Grammar(optional),
      Keyword.Config -> Grammar(optional),
      Keyword.Status -> Grammar(optional),
      Keyword.Description -> Grammar(optional)
    ),
    Keyword.Leaf -> Map(
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
      Keyword.Default -> Grammar(optional)
    ),
    Keyword.LeafList -> Map(
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
      Keyword.Default -> Grammar(optional)
    ),
    Keyword.Type -> Map(
      Keyword.Length -> Grammar(optional),
      Keyword.Pattern -> Grammar(optional),
      Keyword.Units -> Grammar(optional),
      Keyword.Path -> Grammar(optional),
      Keyword.Range -> Grammar(optional),
      Keyword.Enum -> Grammar(optional),
      Keyword.Base -> Grammar(optional)
    ),
    Keyword.Key -> Map(),
    Keyword.TypeDef -> Map(
      Keyword.Type -> Grammar(required),
      Keyword.Description -> Grammar(optional),
      Keyword.Reference -> Grammar(optional),
      Keyword.Units -> Grammar(optional),
      Keyword.Default -> Grammar(optional),
      Keyword.Status -> Grammar(optional)
    ),
    Keyword.Namespace -> Map(),
    Keyword.Prefix -> Map(),
    Keyword.Import -> Map(
      Keyword.Prefix -> Grammar(required)
    ),
    Keyword.Organization -> Map(),
    Keyword.Contact -> Map(),
    Keyword.Description -> Map(),
    Keyword.Revision -> Map(),
    Keyword.Reference -> Map(),
    Keyword.Length -> Map(),
    Keyword.Pattern -> Map(),
    Keyword.Units -> Map(),
    Keyword.Default -> Map(),
    Keyword.YangVersion -> Map(),
    Keyword.Identity -> Map(),
    Keyword.Feature -> Map(
      Keyword.Description -> Grammar(optional),
      Keyword.Reference -> Grammar(optional)
    ),
    Keyword.Status -> Map(),
    Keyword.Path -> Map(),
    Keyword.Config -> Map(),
    Keyword.IfFeature -> Map(),
    Keyword.Mandatory -> Map()
  )

  def getGrammarDef(kw: Keyword, version: Version): Option[Rules] = {
    rfc6020.get(kw)
  }


  def validateRootStatement(root: Statement): Either[Error, Valid] = {
    val kwF = Keyword
      .fromLiteral(root.keyword)
      .filter(Seq(Keyword.Module).contains(_))
      .toRight("root stmt must be either module or submodule.")
    ???
  }

  def validate(stmt: Statement, version: Version = Version.Rfc6020): Either[Error, Valid] = ???
}
