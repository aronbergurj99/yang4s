package yang4s

import yang4s.schema.{ ModuleName, SchemaContext, SchemaError }
import yang4s.utils.TreeDiagram.{*, given}


object Main {
  def main(args: Array[String]) = {
    val result = for {
      (ctx, _) <- SchemaContext.empty(Seq("yang")).loadModule(ModuleName("ietf-interfaces"))
    } yield (printModules(ctx.modules*))

    println(result.merge)
  }
}