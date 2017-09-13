package lens

import com.twitter.finagle.http.{Message, Request}
import io.fintrospect.parameters.StringParamType

object Query extends BiDiLensSpec[Request, String, String]("query", StringParamType,
  new LensGet[Request, String, String]((name: String, target: Request) => target.params.getAll(name)),
  new LensSet[Request, String, String](
    (name: String, values: List[String], target: Request) => values.fold(target, {
      (m: Request, next: String) => m.params.+(name -> next)
    }))
)

object Header extends BiDiLensSpec[Message, String, String]("header", StringParamType,
  new LensGet[Message, String, String]((name: String, target: Message) => target.headerMap.getAll(name)),
  new LensSet[Message, String, String](
    (name: String, values: List[String], target: Request) => values.fold(target, {
      (m: Request, next: String) => m.headerMap.+(name -> next)
    }))
)
