package yang4s.schema

import java.net.URI

case class Namespace(uri: URI, prefix: Option[String]) {
  override def equals(obj: Any): Boolean = obj match {
    case that: Namespace => this.uri.equals(that.uri)
    case _               => false
  }

  override def hashCode(): Int = uri.hashCode()
}

object Namespace {
  val DEFAULT = Namespace(URI.create("urn:ietf:params:xml:ns:yang:1"), Some("yang"))
}