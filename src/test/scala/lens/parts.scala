package lens

import com.twitter.finagle.http.{Message, Request}
import io.fintrospect.ContentType
import io.fintrospect.parameters.StringParamType

object Query extends BiDiLensSpec[Request, String, String]("query", StringParamType,
  new LensGet[Request, String, String]((name: String, target: Request) => target.params.getAll(name).toList, identity[String]),
  new LensSet[Request, String, String](
    (name: String, values: Seq[String], target: Request) => values.foldLeft(target) {
      (m: Request, next: String) => {
        m.params.+(name -> next)
        m
      }
    }, identity[String])
)

object Header extends BiDiLensSpec[Message, String, String]("header", StringParamType,
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
