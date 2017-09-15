package lens

import com.twitter.finagle.http.Message
import io.fintrospect.ContentType
import io.fintrospect.parameters.StringParamType

object Header extends BaseBidiLensSpec[Message]("header", StringParamType,
  new LensGet[Message, String, String]((name: String, target: Message) => target.headerMap.getAll(name), identity[String]),
  new LensSet[Message, String, String](
    (name: String, values: Seq[String], target: Message) => values.foldLeft(target) {
      (m: Message, next: String) => {
        m.headerMap.add(name, next)
        m
      }
    }, identity[String])

) {
  val CONTENT_TYPE: BiDiLens[Message, Option[ContentType]] = Header.map(ContentType(_), (ct: ContentType) => ct.value).optional("Content-Type")
}
