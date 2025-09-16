package module.foo

import yang4s.schema.{ ModuleName, SchemaContext, SchemaError }
import yang4s.utils.TreeDiagram.{*, given}


object Main {
  def main(args: Array[String]) = {
    val result = for {
      (ctx, _) <- SchemaContext.empty(Seq("yang")).loadModule(ModuleName("example"))
    } yield (printTreeDiagram(ctx))

    println(result.merge)
  }
}