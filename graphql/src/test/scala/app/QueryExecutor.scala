package app

import app.FutureConversions.asTwitter
import com.twitter.finagle.http.Response
import com.twitter.finagle.http.Status.{BadRequest, InternalServerError, Ok}
import com.twitter.util.Future
import io.fintrospect.formats.Json4s.ResponseBuilder.implicits._
import models.{CharacterRepo, FriendsResolver, SchemaDefinition}
import sangria.execution.{ErrorWithResolver, Executor, QueryAnalysisError}

import scala.concurrent.ExecutionContext.Implicits.global

class QueryExecutor {

  def execute(query: GraphQLQuery): Future[Response] = {
    import sangria.marshalling.json4s.native._

    asTwitter(Executor.execute(SchemaDefinition.StarWarsSchema, query.ast, new CharacterRepo,
      operationName = query.operation.map(_.value),
      variables = query.variablesToUse,
      deferredResolver = new FriendsResolver,
      maxQueryDepth = Some(10)))
      .flatMap(Ok(_))
      .handle {
        case error: QueryAnalysisError ⇒ BadRequest(error.resolveError.toString)
        case error: ErrorWithResolver ⇒ InternalServerError(error.resolveError.toString)
      }
  }

}
