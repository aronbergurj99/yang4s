package yang4s.utils

import yang4s.schema.SchemaContext
import yang4s.schema.SchemaModule
import yang4s.schema.SubModule
import yang4s.schema.SchemaNode
import yang4s.schema.ContainerNode
import yang4s.schema.ListNode
import yang4s.schema.LeafNode
import yang4s.schema.SchemaMeta

// https://datatracker.ietf.org/doc/html/rfc8340

trait TreeDiagramPrintable[A] {
  extension (a: A) def print(lvl: Int): String
}
object TreeDiagram {
  given TreeDiagramPrintable[SchemaContext] with {
    extension (ctx: SchemaContext) def print(lvl: Int) = {
      printAll(ctx.modules)
    }
  }

  given TreeDiagramPrintable[SchemaModule] with {
    extension (ctx: SchemaModule) def print(lvl: Int) = {
      def printModule(name: String, dataDefs: List[SchemaNode]): String = {
        s"module: $name\n" ++ printAll(dataDefs, lvl + 1)
      }
      ctx match
        case yang4s.schema.Module(name, _, _, dataDefs, _) => printModule(name, dataDefs)
        case SubModule(name, _, _, dataDefs) => printModule(name, dataDefs)
    }
  }

  given TreeDiagramPrintable[SchemaNode] with {
    extension (node: SchemaNode) def print(lvl: Int) = {
      def printNode(meta: SchemaMeta, dataDefs: List[SchemaNode], opts: String = "", suffix: Option[String] = None): String = {
        " ".repeat(lvl * 3) ++ s"+--rw ${meta.name}$opts${suffix.map(s => s" $s").getOrElse("")}\n" ++ printAll(dataDefs, lvl + 1)
      }

      node match
        case ContainerNode(meta, dataDefs) => printNode(meta, dataDefs)
        case ListNode(meta, dataDefs, key) => printNode(meta, dataDefs, opts="*", suffix= key.map(k => s"[$k]"))
        case LeafNode(meta, dataDefs, tpe) => printNode(meta, dataDefs, suffix = Some(s"${" ".repeat(4)}${tpe.name}"))
      
    }
  }

  def printTreeDiagram[A: TreeDiagramPrintable](v: A): String = v.print(0)

  def printAll[A: TreeDiagramPrintable](vs: List[A], lvl: Int = 0): String = {
    vs.map(_.print(lvl)).mkString
  }
}