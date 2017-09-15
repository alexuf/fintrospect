package lens

import java.nio.charset.Charset

import com.twitter.finagle.http.Message
import com.twitter.io.{Buf, Bufs}
import io.fintrospect.ContentType
import io.fintrospect.ContentTypes.APPLICATION_XML
import io.fintrospect.parameters.{FileParamType, StringParamType}

import scala.xml.{Elem, XML}

object Body {

  private def root(metas: List[Meta], contentType: ContentType): BiDiBodyLensSpec[Buf] = {
    new BiDiBodyLensSpec(metas, contentType,
      new LensGet[Message, Buf, Buf]((_: String, target: Message) => List(target.content), identity[Buf]),
      new LensSet[Message, Buf, Buf](
        (_: String, values: Seq[Buf], target: Message) => values.foldLeft(target) {
          (m: Message, next: Buf) => {
            m.content = next
            m
          }
        }, identity[Buf]
      ))
  }

  def string(contentType: ContentType, description: String = null,
             contentNegotiation: ContentNegotiation = ContentNegotiation.None): BiDiBodyLensSpec[String] =
    root(List(Meta(true, "body", StringParamType, "body", description)), contentType)
      .map((b: Buf) => Buf.decodeString(b, Charset.defaultCharset()), Bufs.utf8Buf)

  def nonEmptyString(contentType: ContentType, description: String = null,
                     contentNegotiation: ContentNegotiation = ContentNegotiation.None): BodyLensSpec[String] = {
    string(contentType, description, contentNegotiation).map(
      (value: String) => if (value.isEmpty) throw new IllegalArgumentException() else value,
      identity[String]
    )
  }

  def binary(contentType: ContentType, description: String = null, contentNegotiation: ContentNegotiation = ContentNegotiation.None) = {
    root(List(Meta(true, "body", FileParamType, "body", description)), contentType)
  }

  def xml(description: String = null, contentNegotiation: ContentNegotiation = ContentNegotiation.None) =
    string(APPLICATION_XML, description, contentNegotiation).map[Elem]((s: String) => XML.loadString(s), (s: Elem) => s.toString())
}
