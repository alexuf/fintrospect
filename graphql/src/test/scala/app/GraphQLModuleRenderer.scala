package app

import com.twitter.finagle.http.path.Path
import com.twitter.finagle.http.{Response, Status}
import io.fintrospect.formats.PlainText.ResponseBuilder.implicits._
import io.fintrospect.renderers.ModuleRenderer
import io.fintrospect.util.ExtractionError
import io.fintrospect.{Security, ServerRoute}
import sangria.renderer.SchemaRenderer

class GraphQLModuleRenderer(schema: sangria.schema.Schema[_, _]) extends ModuleRenderer {

  override def description(basePath: Path, security: Security, routes: Seq[ServerRoute[_, _]]): Response = {
    Status.Ok(SchemaRenderer.renderSchema(schema))
  }

  override def badRequest(badParameters: Seq[ExtractionError]): Response = {
    Status.BadRequest(badParameters.toString())
  }
}
