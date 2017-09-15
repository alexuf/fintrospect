package lens


import com.twitter.finagle.http.{Method, Request, Response, Status}
import io.fintrospect.formats.{Argo, JsonLibrary}
import io.fintrospect.{ContentType, ResponseSpec, TagInfo}
import lens.RouteSpec.QueryOrHeader

/**
  * Encapsulates the specification of an HTTP endpoint, for use by either a Finagle server or client.
  */
case class RouteSpec private(summary: String,
                             description: Option[String],
                             produces: Set[ContentType],
                             consumes: Set[ContentType],
                             body: Option[BodyLens[_]],
                             requestParams: Seq[QueryOrHeader[_]],
                             responses: Map[Status, ResponseSpec],
                             tags: Set[TagInfo]) {

  def validate(request: Request) = {
    requestParams.map(_.extract(request))
    body.map(_.extract(request))
  }

  /**
    * Register content types which the route will consume. This is informational only and is NOT currently enforced.
    */
  def consuming(contentTypes: ContentType*): RouteSpec = copy(consumes = consumes ++ contentTypes)

  /**
    * Register content types which thus route will produce. This is informational only and NOT currently enforced.
    */
  def producing(contentTypes: ContentType*): RouteSpec = copy(produces = produces ++ contentTypes)

  /**
    * Register a header/query parameter. Mandatory parameters are checked for each request, and a 400 returned if any are missing.
    */
  def taking(rp: QueryOrHeader[_]): RouteSpec = copy(requestParams = rp +: requestParams)

  /**
    * Register the expected content of the body.
    */
  def body(bp: BodyLens[_]): RouteSpec = copy(body = Option(bp), consumes = consumes + bp.contentType)

  /**
    * Register a possible response which could be produced by this route, with an example JSON body (used for schema generation).
    */
  def returning(newResponse: ResponseSpec): RouteSpec = copy(responses = responses + ((newResponse.status, newResponse)))

  /**
    * Register one or more possible responses which could be produced by this route.
    */
  def returning(codesAndReasons: (Status, String)*): RouteSpec = copy(responses = responses ++ codesAndReasons.map(c => c._1 -> new ResponseSpec(c, null)))

  /**
    * Register an exact possible response which could be produced by this route. Will be used for schema generation if content is JSON.
    */
  def returning(response: Response): RouteSpec = returning(new ResponseSpec(response.status -> response.status.reason, Option(response.contentString)))

  /**
    * Register a possible response which could be produced by this route, with an example JSON body (used for schema generation).
    */
  def returning[T](codeAndReason: (Status, String), example: T, jsonLib: JsonLibrary[T, _] = Argo): RouteSpec = {
    copy(responses = responses + (codeAndReason._1 -> ResponseSpec.json(codeAndReason, example, jsonLib)))
  }

  /**
    * Register a possible response which could be produced by this route, with an example body (used for schema generation).
    */
  def returning(codeAndReason: (Status, String), example: String): RouteSpec = copy(responses = responses + (codeAndReason._1 -> new ResponseSpec(codeAndReason, Option(example))))

  /**
    * Add tags to this routes. Provides the ability to group routes by tag in a generated schema.
    */
  def taggedWith(tag: String): RouteSpec = taggedWith(TagInfo(tag))

  /**
    * Add tags to this routes. Provides the ability to group routes by tag in a generated schema.
    */
  def taggedWith(tags: TagInfo*): RouteSpec = copy(tags = this.tags ++ tags)

  def at(method: Method) = UnboundRoute(this, method)
}

object RouteSpec {
  type QueryOrHeader[T] = BiDiLens[Request, T]

  def apply(summary: String = "<unknown>", description: String = null): RouteSpec =
    RouteSpec(summary, Option(description), Set.empty, Set.empty, None, Nil, Map.empty, Set.empty)
}
