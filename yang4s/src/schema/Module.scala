package yang4s.schema

import yang4s.parser.Statement

sealed trait SchemaModule
case class Module(name: String, namespace: String, prefix: String) extends SchemaModule
case class SubModule(name: String, namespace: String, prefix: String) extends SchemaModule

object Module {
}

