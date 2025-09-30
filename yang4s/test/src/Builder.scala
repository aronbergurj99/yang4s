package yang4s.schema

import munit.FunSuite
import yang4s.parser.StatementParser
import yang4s.schema.Builders.moduleBuilder
import yang4s.utils.TreeDiagram.printModules

class BuilderSuite extends FunSuite {
  val smallestModule = """
  module testing {
    namespace "http://aronj.is/example";
    prefix "example";

    typedef third {
        type string;
    }
    typedef hostname {
        type another;
    }
    typedef another {
        type third;
    }
  }
  """.strip

  test("Hello builder test") {
    val result = StatementParser().parse(smallestModule).flatMap { stmt =>
      moduleBuilder.build(stmt, SchemaContext.empty(Seq())).map(_._2)
    }
    println(result.map(mod => printModules(mod)).merge)
  }
}