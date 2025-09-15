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
  extension (a: A) def print(prefix: String, isLast: Boolean = false): String
}
object TreeDiagram {
  def apply[A](using ev: TreeDiagramPrintable[A]): TreeDiagramPrintable[A] = ev

  given TreeDiagramPrintable[SchemaContext] with {
    extension (ctx: SchemaContext) def print(prefix: String, isLast: Boolean = false) = {
      printAll(ctx.modules, "")
    }
  }

  given TreeDiagramPrintable[SchemaModule] with {
    extension (ctx: SchemaModule) def print(prefix: String, isLast: Boolean = false) = {
      def printModule(name: String, dataDefs: List[SchemaNode]): String = {
        s"module: $name\n" ++ printAll(dataDefs, "  ")
      }
      ctx match
        case yang4s.schema.Module(name, _, _, dataDefs, _) => printModule(name, dataDefs)
        case SubModule(name, _, _, dataDefs, _) => printModule(name, dataDefs)
    }
  }

  given TreeDiagramPrintable[SchemaNode] with {
    extension (node: SchemaNode) def print(prefix: String, isLast: Boolean = false) = {
      def printNode(meta: SchemaMeta, dataDefs: List[SchemaNode], opts: String = "", suffix: Option[String] = None): String = {
        prefix ++ s"+--rw ${meta.name}$opts${suffix.map(s => s" $s").getOrElse("")}\n" ++ printAll(dataDefs, prefix ++ {if (isLast) "   " else "|  "} )
      }

      node match
        case ContainerNode(meta, dataDefs) => printNode(meta, dataDefs)
        case ListNode(meta, dataDefs, key) => printNode(meta, dataDefs, opts="*", suffix= key.map(k => s"[$k]"))
        case LeafNode(meta, dataDefs, tpe) => printNode(meta, dataDefs, suffix = Some(s"${" ".repeat(4)}${tpe.name}"))
      
    }
  }

  given [A](using ev: TreeDiagramPrintable[A]): TreeDiagramPrintable[List[A]] with {
    extension (l: List[A]) def print(prefix: String, isLast: Boolean = false): String = printAll(l, prefix)
  }

  def printTreeDiagram[A: TreeDiagramPrintable](v: A): String = v.print("")


  def printAll[A: TreeDiagramPrintable](vs: List[A], prefix: String): String = {
    vs.zipWithIndex.map { (i, idx) =>
        i.print(prefix, idx == vs.length - 1)
      }.mkString
  }
}