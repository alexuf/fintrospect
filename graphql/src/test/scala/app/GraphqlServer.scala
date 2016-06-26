package app

import com.twitter.finagle.http.Method.{Get, Post}
import com.twitter.finagle.http.Request
import com.twitter.finagle.http.filter.Cors
import com.twitter.finagle.http.filter.Cors.HttpFilter
import com.twitter.finagle.http.path.Root
import com.twitter.finagle.{Http, Service}
import io.fintrospect.Module.{combine, toService}
import io.fintrospect.formats.json.Json4s
import io.fintrospect.parameters.{ParameterSpec, Query}
import io.fintrospect.{ModuleSpec, RouteSpec, StaticModule}
import models.SchemaDefinition
import sangria.parser.QueryParser

import scala.language.reflectiveCalls


object GraphQLServer extends App {

  val jsonLibrary = io.fintrospect.formats.json.Json4s.Native

  private val query = Query.required(ParameterSpec.string("query").map(QueryParser.parse(_).get))
  private val variables = Query.optional.json("variables", "", jsonLibrary.JsonFormat)
  private val operation = Query.optional(ParameterSpec.string("operation").map(Operation))

  val graphQlQuery = Service.mk {
    req: Request => {
      new QueryExecutor().execute(GraphQLQuery(query <-- req, variables <-- req, operation <-- req))
    }
  }

  val getQuery = RouteSpec()
    .taking(query)
    .taking(variables)
    .taking(operation)
    .at(Get) / "graphql" bindTo graphQlQuery

  //  val body = Body.json(None, null, jsonLibrary.JsonFormat)
  //
  //  val postQuery = RouteSpec()
  //    .body(body)
  //    .at(Post) / "graphqlBody" bindTo graphQlQuery

  //    Action.async(executeQuery(query, variables map parseVariables, operation))

  //  def graphqlBody = Action.async(parse.json) { request =>
  //    val query = (request.body \ "query").as[String]
  //    val operation = (request.body \ "operationName").asOpt[String]
  //
  //    val variables = (request.body \ "variables").toOption.flatMap {
  //      case JsString(vars) => Some(parseVariables(vars))
  //      case obj: JsObject => Some(obj)
  //      case _ => None
  //    }
  //
  //    executeQuery(query, variables, operation)
  //  }


  val body = Json4s.Native.JsonFormat.body[GQL]()

  val postQuery = RouteSpec().body(body).at(Post) / "graphql" bindTo Service.mk {
    req: Request =>

      val q = body <-- req
      new QueryExecutor().execute(
        GraphQLQuery(
          QueryParser.parse(q.query).get,
          None,
          q.operation.map(Operation)))
  }

  val graphQLModule = ModuleSpec(Root, new GraphQLModuleRenderer(SchemaDefinition.StarWarsSchema))
    .withDescriptionPath(_ / "render-schema")
    .withRoute(getQuery)
    .withRoute(postQuery)

  val overallSvc = toService(combine(StaticModule(Root, "public"), graphQLModule))

  Http.serve(":9000", new HttpFilter(Cors.UnsafePermissivePolicy).andThen(overallSvc))

  Thread.currentThread().join()
}
