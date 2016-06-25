package app

import org.json4s.JValue
import sangria.ast.Document

case class GraphQLQuery(ast: Document, private val variables: Option[JValue], operation: Option[Operation]) {
  val jsonLibrary = io.fintrospect.formats.json.Json4s.Native

  import jsonLibrary.JsonFormat.obj
  val InputUnmarshaller = sangria.marshalling.json4s.native.Json4sNativeInputUnmarshaller
  val variablesToUse = variables getOrElse obj()
}
