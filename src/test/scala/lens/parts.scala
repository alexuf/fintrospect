package lens

import java.nio.charset.Charset
import java.time.format.DateTimeFormatter.{ISO_LOCAL_DATE, ISO_LOCAL_DATE_TIME, ISO_ZONED_DATE_TIME}
import java.time.{LocalDate, LocalDateTime, ZonedDateTime}
import java.util.UUID

import com.twitter.finagle.http.{Message, Request}
import com.twitter.io.Buf
import io.fintrospect.ContentType
import io.fintrospect.parameters.{FileParamType, ParamType, StringParamType}

import scala.util.matching.Regex

class BaseBidiLensSpec[T <: Message](
                                      location: String,
                                      paramMeta: ParamType,
                                      get: LensGet[T, String, String],
                                      private val set: LensSet[T, String, String]
                                    ) extends BiDiLensSpec[T, String, String](location, paramMeta, get, set) {

  def string(): BiDiLensSpec[T, String, String] = this

  def nonEmptyString(): BiDiLensSpec[T, String, String] = this.map(nonEmpty, nonEmpty)

  def int(): BiDiLensSpec[T, String, Int] = this.map(_.toInt, _.toString)

  def long(): BiDiLensSpec[T, String, Long] = this.map(_.toLong, _.toString)

  def double(): BiDiLensSpec[T, String, Double] = this.map(_.toDouble, _.toString)

  def float(): BiDiLensSpec[T, String, Float] = this.map(_.toFloat, _.toString)

  def boolean(): BiDiLensSpec[T, String, Boolean] = this.map(_.toBoolean, _.toString)

  def localDate(): BiDiLensSpec[T, String, LocalDate] = this.map(LocalDate.parse, ISO_LOCAL_DATE.format)

  def dateTime(): BiDiLensSpec[T, String, LocalDateTime] = this.map(LocalDateTime.parse, ISO_LOCAL_DATE_TIME.format)

  def zonedDateTime(): BiDiLensSpec[T, String, ZonedDateTime] = this.map(ZonedDateTime.parse, ISO_ZONED_DATE_TIME.format)

  def uuid(): BiDiLensSpec[T, String, UUID] = this.map(UUID.fromString, _.toString)

  def regex(pattern: String, group: Int = 1): LensSpec[T, String, String] = this.map(new Regex(pattern).findFirstIn(_).get)

  private def nonEmpty(value: String): String = if (value.isEmpty) throw new IllegalArgumentException() else value
}

object Query extends BaseBidiLensSpec[Request]("query", StringParamType,
  new LensGet[Request, String, String]((name: String, target: Request) => target.params.getAll(name).toList, identity[String]),
  new LensSet[Request, String, String](
    (name: String, values: Seq[String], target: Request) => values.foldLeft(target) {
      (m: Request, next: String) => {
        m.params.+(name -> next)
        m
      }
    }, identity[String])
)

object Header extends BaseBidiLensSpec[Message]("header", StringParamType,
  new LensGet[Message, String, String]((name: String, target: Message) => target.headerMap.getAll(name), identity[String]),
  new LensSet[Message, String, String](
    (name: String, values: Seq[String], target: Message) => values.foldLeft(target) {
      (m: Message, next: String) => {
        m.headerMap.+(name -> next)
        m
      }
    }, identity[String])

) {
  val CONTENT_TYPE: BiDiLens[Message, Option[ContentType]] = Header.map(ContentType(_), (ct: ContentType) => ct.value).optional("Content-Type")
}

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
             contentNegotiation: ContentNegotiation = ContentNegotiation.None): BodyLensSpec[String] = {
    root(List(Meta(true, "body", StringParamType, "body", description)), contentType).map(Buf.decodeString(_, Charset.defaultCharset()))
  }
//
//  def nonEmptyString(contentType: ContentType, description: String = null,
//                     contentNegotiation: ContentNegotiation = ContentNegotiation.None): BodyLensSpec[String] = {
//    string(contentType, description, contentNegotiation).map(
//       (value: String) => if (value.isEmpty) throw new IllegalArgumentException() else value
//      , (s: String) => s)
//  }

  def binary(contentType: ContentType, description: String = null, contentNegotiation: ContentNegotiation = ContentNegotiation.None) = {
    root(List(Meta(true, "body", FileParamType, "body", description)), contentType)
  }
}