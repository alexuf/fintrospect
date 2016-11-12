package app

import app.FutureConversions.asTwitter
import com.twitter.finagle.http.Response
import com.twitter.finagle.http.Status.{BadRequest, InternalServerError, Ok}
import com.twitter.util.Future
import io.fintrospect.formats.Json4s.ResponseBuilder.implicits._
import models.{CharacterRepo, SchemaDefinition}
import sangria.execution.deferred.DeferredResolver
import sangria.execution.{ErrorWithResolver, Executor, QueryAnalysisError}
import sangria.marshalling.json4s.native._

import scala.concurrent.ExecutionContext.Implicits.global

class QueryExecutor {

  def execute(query: GraphQLQuery): Future[Response] = {

    asTwitter(Executor.execute(SchemaDefinition.StarWarsSchema, query.ast, new CharacterRepo,
      variables = query.variablesToUse,
      deferredResolver = DeferredResolver.fetchers(SchemaDefinition.characters),
      maxQueryDepth = Some(10)))
      .flatMap(Ok(_))
      .handle {
        case error: QueryAnalysisError ⇒ BadRequest(error.resolveError.toString)
        case error: ErrorWithResolver ⇒ InternalServerError(error.resolveError.toString)
      }
  }

}
