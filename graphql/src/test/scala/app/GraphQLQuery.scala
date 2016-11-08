package app

import argo.jdom.JsonRootNode
import com.twitter.finagle.http.Request
import io.fintrospect.formats.Json4s.JsonFormat._
import io.fintrospect.parameters.Body
import io.fintrospect.util.ExtractionError.Invalid
import io.fintrospect.util.{Extracted, ExtractionError, ExtractionFailed, Extractor}
import org.json4s.JValue
import sangria.ast.Document
import sangria.parser.{QueryParser, SyntaxError}

import scala.util.{Failure, Success}

case class GQL(query: String, operation: Option[String])

case class GraphQLQuery(ast: Document, private val variables: Option[JValue], operation: Option[Operation]) {
  val InputUnmarshaller = sangria.marshalling.json4s.native.Json4sNativeInputUnmarshaller
  val variablesToUse = variables getOrElse obj()
}


object GraphQLQuery extends Extractor[Request, GraphQLQuery] {

  private val body = Body.json[JsonRootNode](None)

  private val documentExtractor = Extractor.mk[String, Document] {
    query: String =>
      QueryParser.parse(query) match {
        case Success(d) => Extracted(Some(d))
        case Failure(error: SyntaxError) => ExtractionFailed(Invalid(body.iterator.next))
        case Failure(e) => ExtractionFailed(Invalid(body.iterator.next))
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

  def extractorate(s: Request): Either[Seq[ExtractionError], GraphQLQuery] =
    this <--? s
    match {
      case Extracted(Some(query)) => Right(query)
      case Extracted(None) => Left(Seq(ExtractionError.Missing(null))) // !!
      case ExtractionFailed(es) => Left(es)
    }

  def <--?(req: Request) =
    for {
      b <- body <--? req
      doc <- documentExtractor <--? b.get.getStringValue("query")
    } yield GraphQLQuery(doc.get, None, Option(b.get.getNullableStringValue("operation")).map(Operation))


}