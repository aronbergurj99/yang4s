package module.foo

import yang4s.schema.{ ModuleName, SchemaContext, SchemaError }


object Main {
  def main(args: Array[String]) = {
    val result = for {
      ctx <- SchemaContext.empty(Seq("yang")).loadModules(List(ModuleName("example")))
    } yield (ctx.modules)

    println(result)
  }
}