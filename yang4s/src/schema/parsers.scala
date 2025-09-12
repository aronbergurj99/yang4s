package yang4s.schema

import yang4s.parser.Statement

import yang4s.schema.{Keyword => Kw}


object parsers {
  type ParserResult[A] = Either[String, A]

  def moduleParser: PartialFunction[Statement, ParserResult[Module]] = {
    case stmt @ Statement(None, Keyword.Module.literal, Some(arg), children) => {
      val moduleResult = for {
        v <- Grammar.validate(stmt)
        namespace <- namespaceParser(v.required(Kw.Namespace))
        prefix    <- prefixParser(v.required(Kw.Prefix))
      } yield (Module(arg, namespace, prefix))
      
      moduleResult
    }
  }

  def namespaceParser(stmt: Statement): ParserResult[String] = {
    stmt.arg.toRight("Arguement required for namespace")
  }

  def prefixParser(stmt: Statement): ParserResult[String] = {
    stmt.arg.toRight("Arguement required for prefix")
  }

  val schemaModule = moduleParser.lift

}