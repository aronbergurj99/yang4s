package yang4s.schema

import munit.FunSuite
import yang4s.parser.StatementParser
import yang4s.schema.builders.moduleBuilder

class BuilderSuite extends FunSuite {
  val smallestModule = """
  module testing {
    namespace "http://aronj.is/example";
    prefix test;
  }
  """.strip

  test("Hello builder test") {
    val result = StatementParser().parse(smallestModule).flatMap { stmt =>
      moduleBuilder.build(stmt)
    }
    println(result)
  }
}