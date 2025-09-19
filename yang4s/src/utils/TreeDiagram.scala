package yang4s.utils

import yang4s.schema.SchemaContext
import yang4s.schema.SchemaModule
import yang4s.schema.SubModule
import yang4s.schema.SchemaNode
import yang4s.schema.SchemaMeta
import yang4s.schema.{Module => SModule}
import yang4s.schema.BuiltInType
import yang4s.schema.SchemaNode.*
import yang4s.schema.SchemaNodeKind.*

// https://datatracker.ietf.org/doc/html/rfc8340

object TreeDiagram {
  private def printDataDef(node: DataNode, mod: SModule, isLast: Boolean, prefix: String, colLength: Int): String = {
    def printRow(meta: SchemaMeta, dataDefs: List[DataNode], opts: String = "", suffix: Option[String] = None): String = {
      prefix ++ s"+--rw ${meta.qName.localName}$opts${suffix.map(s => s" $s").getOrElse("")}\n" ++ printDataDefs(
        dataDefs,
        mod,
        prefix ++ { if (isLast) "   " else "|  " }
      )
    }
    node match
      case TerminalNode(meta, kind) => {
        kind match
          case LeafNode(tpe) => {
            val gap = colLength - meta.qName.localName.length
            val typeName = {
              val qName = tpe.qName
              val isBuiltin = BuiltInType.isBuiltin(qName.localName)
              if (isBuiltin || qName.namespace == mod.namespace) {
                qName.localName
              } else
                qName.qualifiedName
            }
            printRow(meta, List.empty, suffix = Some(s"${" ".repeat(gap + 4)}${typeName}"))
          }
      }
      case DataDefiningNode(meta, dataDefs, kind) => {
        kind match
          case ContainerNode => printRow(meta, dataDefs)
          case ListNode(key) => printRow(meta, dataDefs, opts = "*", suffix = key.map(k => s"[$k]"))
      }
  }

  private def printDataDefs(nodes: List[DataNode], mod: SModule, prefix: String = ""): String = {
    val longestName = nodes
      .map(_.meta.qName.localName)
      .reduceOption { (a, b) =>
        if (a.length >= b.length) a else b
      }
      .map(_.length)
      .getOrElse(0)
    nodes.zipWithIndex
      .map((d, idx) =>
        val isLast = idx == nodes.length - 1
        printDataDef(d, mod, isLast, prefix, longestName)
      )
      .mkString
  }

  def printModules(modules: SchemaModule*): String = {
    val implementedModules = modules
      .filter { mod =>
        mod match
          case m: SModule => m.isImplemented
          case _          => false
      }
      .asInstanceOf[Seq[SModule]]

    implementedModules.map { m =>
      s"module: ${m.name}\n${{ printDataDefs(m.dataDefs, m) }}"
    }.mkString
  }
}
