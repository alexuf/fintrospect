package lens

import com.twitter.finagle.http.{Message, Request}
import io.fintrospect.parameters.StringParamType

object Query extends BiDiLensSpec[Request, String, String]("query", StringParamType,
  new LensGet[Request, String, String]((name: String, target: Request) => target.params.getAll(name).toList, (i: String) => i),
  new LensSet[Request, String, String](
    (name: String, values: Seq[String], target: Request) => values.foldLeft(target) {
      (m: Request, next: String) => {
        m.params.+(name -> next)
        m
      }
    }, (i: String) => i)
)

object Header extends BiDiLensSpec[Message, String, String]("header", StringParamType,
  new LensGet[Message, String, String]((name: String, target: Message) => target.headerMap.getAll(name), (i: String) => i),
  new LensSet[Message, String, String](
    (name: String, values: Seq[String], target: Message) => values.foldLeft(target) {
      (m: Message, next: String) => {
        m.headerMap.+(name -> next)
        m
      }
    }, (i: String) => i)
)
