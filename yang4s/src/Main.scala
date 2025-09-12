package module.foo

import yang4s.schema.{ ModuleName, SchemaContext, SchemaError }

import cats.data.NonEmptyList


object Main {
  def main(args: Array[String]) = {
    for {
      ctx <- SchemaContext.empty(Seq("yang")).loadModules(List(ModuleName("example")))
    } yield (println(ctx.modules))

    ()
  }
}