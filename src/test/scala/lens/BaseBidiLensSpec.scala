package lens

import java.time.format.DateTimeFormatter.{ISO_LOCAL_DATE, ISO_LOCAL_DATE_TIME, ISO_ZONED_DATE_TIME}
import java.time.{LocalDate, LocalDateTime, ZonedDateTime}
import java.util.UUID

import io.fintrospect.parameters.ParamType

import scala.util.matching.Regex

class BaseBidiLensSpec[T <: Any](
                                      location: String,
                                      paramMeta: ParamType,
                                      get: LensGet[T, String, String],
                                      private val set: LensSet[T, String, String]
                                    ) extends BiDiLensSpec[T, String, String](location, paramMeta, get, set) {

  def string(): BiDiLensSpec[T, String, String] = this

  def char(): BiDiLensSpec[T, String, Char] = this.map((s: String) => s.charAt(0), _.toString)

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





