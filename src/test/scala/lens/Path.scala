package lens

import java.time.format.DateTimeFormatter.{ISO_LOCAL_DATE, ISO_LOCAL_DATE_TIME, ISO_ZONED_DATE_TIME}
import java.time.{LocalDate, LocalDateTime, ZonedDateTime}
import java.util.UUID

import com.twitter.finagle.http.Request
import io.fintrospect.parameters.StringParamType

import scala.util.matching.Regex

object Path extends BiDiPathLensSpec[String](StringParamType,
  new LensGet[String, String, String]((_: String, target: String) => Seq(target), identity[String]),
  new LensSet[Request, String, String](
    (name: String, values: Seq[String], target: Request) => values.foldLeft(target) {
      (m: Request, next: String) => {
        m.uri(m.uri.replaceFirst(s"{$name}", next))
      }
    }, identity[String])) {

  def fixed(name: String): PathLens[String] = {
    val getLens = get(name)
    val meta = Meta(true, "path", StringParamType, name)
    new PathLens[String](meta,
      it => getLens(it).find(_ == name).getOrElse(throw LensFailure(null, Missing(meta)))) {
      override def toString(): String = name

      def unapply(str: String): Option[String] = if (str == meta.name) Option(str) else None
    }
  }

  def string(): BiDiPathLensSpec[String] = this

  def char(): BiDiPathLensSpec[Char] = this.map((s: String) => s.charAt(0), _.toString)

  def int(): BiDiPathLensSpec[Int] = this.map(_.toInt, _.toString)

  def long(): BiDiPathLensSpec[Long] = this.map(_.toLong, _.toString)

  def double(): BiDiPathLensSpec[Double] = this.map(_.toDouble, _.toString)

  def float(): BiDiPathLensSpec[Float] = this.map(_.toFloat, _.toString)

  def boolean(): BiDiPathLensSpec[Boolean] = this.map(_.toBoolean, _.toString)

  def localDate(): BiDiPathLensSpec[LocalDate] = this.map(LocalDate.parse, ISO_LOCAL_DATE.format)

  def dateTime(): BiDiPathLensSpec[LocalDateTime] = this.map(LocalDateTime.parse, ISO_LOCAL_DATE_TIME.format)

  def zonedDateTime(): BiDiPathLensSpec[ZonedDateTime] = this.map(ZonedDateTime.parse, ISO_ZONED_DATE_TIME.format)

  def uuid(): BiDiPathLensSpec[UUID] = this.map(UUID.fromString, _.toString)

  def regex(pattern: String, group: Int = 1): PathLensSpec[String] = this.map(new Regex(pattern).findFirstIn(_).get)
}

