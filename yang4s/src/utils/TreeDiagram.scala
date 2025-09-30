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
import yang4s.schema.Status
import yang4s.schema.QName

// https://datatracker.ietf.org/doc/html/rfc8340

object TreeDiagram {
  private def printQName(qName: QName, mod: SModule) = {
    if (qName.namespace == mod.namespace) {
      qName.localName
    } else {
      qName.qualifiedName
    }
  }

  private def printDataDef(node: DataNode, mod: SModule, isLast: Boolean, prefix: String, colLength: Int): String = {
    def printRow(meta: SchemaMeta, dataDefs: List[DataNode], opts: String = "", suffix: Option[String] = None): String = {
      val flag = if (meta.config) "rw" else "ro"
      val status = {
        meta.status match
          case Status.Current => "+"
          case yang4s.schema.Status.Deprecated => "x"
          case Status.Obsolete => "o"
      }


      prefix ++ s"$status--$flag ${meta.qName.localName}$opts${suffix.map(s => s" $s").getOrElse("")}\n" ++ printDataDefs(
        dataDefs,
        mod,
        prefix ++ { if (isLast) "   " else "|  " }
      )
    }


    node match
      case TerminalNode(meta, tpe, kind) => {
        val features = {
          meta.ifFeatures match
            case Nil => ""
            case _ @f => f.map(printQName(_, mod)).mkString("{", " ", "}?") 
        }
        val gap = colLength - meta.qName.localName.length
        val typeName = {
          val qName = tpe.meta.qName
          val isBuiltin = BuiltInType.isBuiltin(qName.localName)
          
          if (isBuiltin) {
            qName.localName
          } else
            printQName(qName, mod)
        }
        val indicator = kind match
          case LeafNode(mandatory) => {
            if (!mandatory) {
              "?"
            } else ""
          }
          case LeafList => "*"
        
        printRow(meta, List.empty, opts = indicator, suffix = Some(s"${" ".repeat(gap + (4 - indicator.length))}${typeName} $features"))

      }
      case DataDefiningNode(meta, dataDefs, kind) => {
        kind match
          case ContainerNode => printRow(meta, dataDefs)
          case ListNode(key) => printRow(meta, dataDefs, opts = "*", suffix = Some(s"[${key.localName}]"))
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
      s"module: ${m.name}\n${{ printDataDefs(m.dataDefs, m, "  ")}}"
    }.mkString
  }
}
