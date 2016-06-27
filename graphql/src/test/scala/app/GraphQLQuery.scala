package app

import argo.jdom.JsonRootNode
import com.twitter.finagle.http.Request
import io.fintrospect.formats.json.Json4s.Native.JsonFormat._
import io.fintrospect.parameters.{Body, Extracted, ExtractionFailed, Extractor, InvalidParameter, NotProvided}
import org.json4s.JValue
import sangria.ast.Document
import sangria.parser.{QueryParser, SyntaxError}

import scala.util.{Failure, Success}

case class GQL(query: String, operation: Option[String])

case class GraphQLQuery(ast: Document, private val variables: Option[JValue], operation: Option[Operation]) {
  val jsonLibrary = io.fintrospect.formats.json.Json4s.Native

  import jsonLibrary.JsonFormat.obj

  val InputUnmarshaller = sangria.marshalling.json4s.native.Json4sNativeInputUnmarshaller
  val variablesToUse = variables getOrElse obj()
}


object GraphQLQuery extends Extractor[Request, GraphQLQuery] {
  val lib = io.fintrospect.formats.json.Json4s.Native

  private val body = Body.json[JsonRootNode](None)

  private val documentExtractor = Extractor.mk[String, Document] {
    query: String =>
      QueryParser.parse(query) match {
        case Success(d) => Extracted(d)
        case Failure(error: SyntaxError) => ExtractionFailed(InvalidParameter(body.iterator.next, compact(toError(error))))
        case Failure(e) => ExtractionFailed(InvalidParameter(body.iterator.next, compact(obj("message" -> string(e.getMessage)))))
      }
  }

  private def toError(error: SyntaxError): JValue = {
    obj(
      "message" -> string(error.getMessage),
      "locations" -> array(
        obj(
          "line" -> number(error.originalError.position.line),
          "column" -> number(error.originalError.position.column))))
  }

  def extractorate(s: Request): Either[Seq[InvalidParameter], GraphQLQuery] =
    this <--? s
    match {
      case Extracted(query) => Right(query)
      case NotProvided => Left(Seq())
      case ExtractionFailed(es) => Left(es)
    }

  def <--?(req: Request) =
    for {
      b <- body <--? req
      doc <- documentExtractor <--? b.get.getStringValue("query")
    } yield GraphQLQuery(doc.get, None, Option(b.get.getNullableStringValue("operation")).map(Operation))


}