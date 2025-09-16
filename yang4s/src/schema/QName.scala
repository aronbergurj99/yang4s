package yang4s.schema

case class QName(namespace: Namespace, localName: String) {
  def prefix: Option[String] = namespace.prefix

  def qualifiedName: String = {
    namespace.prefix.map(p => s"$p:$localName").getOrElse(localName)
  }
}

object QName {
  val UNKNOWN = defaultNamespace("unknown")

  def defaultNamespace(localName: String): QName = QName(Namespace.DEFAULT, localName)
}