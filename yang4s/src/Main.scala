package module.foo

import yang4s.schema.{ ModuleName, SchemaContext, SchemaError }
import yang4s.utils.TreeDiagram.{*, given}


object Main {
  def main(args: Array[String]) = {
    val result = for {
      (_, modules) <- SchemaContext.empty(Seq("yang")).loadModules(List(ModuleName("example")))
    } yield (printTreeDiagram(modules))

    println(result.merge)
  }
}