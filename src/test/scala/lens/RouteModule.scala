package lens

import com.twitter.finagle.http.Status.NotFound
import com.twitter.finagle.http.path.{Path => FP}
import com.twitter.finagle.http.{Method, Request, Response, Status}
import com.twitter.finagle.{Filter, Service}
import com.twitter.util.Future
import io.fintrospect.Module.ServiceBinding
import io.fintrospect.RouteModule.ModifyPath
import io.fintrospect.{Headers, Module, Security}

import scala.PartialFunction.empty

trait ModuleRenderer {
  def notFound(request: Request): Response = Response(NotFound)

  def badRequest(badParameters: Seq[Failure]): Response

  def description(basePath: FP, security: Security, routes: Seq[ServerRoute[_, _]]): Response
}

abstract class ServerRoute[RQ, RS](val routeSpec: RouteSpec,
                                   val method: Method,
                                   pathFn: FP => FP,
                                   val pathParams: PathLens[_]*) {

  def matches(actualMethod: Method, basePath: FP, actualPath: FP) = actualMethod == method && actualPath == pathFn(basePath)

  def toPf(filter: Filter[Request, Response, RQ, RS], basePath: FP): PartialFunction[(Method, FP), Service[Request, Response]]

  def describeFor(basePath: FP): String = (pathFn(basePath).toString +: pathParams.map(_.toString())).mkString("/")
}

class RouteModule[RQ, RS] private(basePath: FP,
                                  moduleRenderer: ModuleRenderer,
                                  descriptionRoutePath: ModifyPath,
                                  routes: Seq[ServerRoute[RQ, RS]],
                                  security: Security,
                                  moduleFilter: Filter[Request, Response, RQ, RS]) extends Module {

  private def validationFilter(route: ServerRoute[RQ, RS]) = Filter.mk[Request, Response, Request, Response] {
    (request, svc) =>
      try {
        route.routeSpec.validate(request)
        svc(request)
      } catch {
        case e: LensFailure => Future(Response(Status.BadRequest))
      }
  }

  override def serviceBinding: ServiceBinding =
    withDefault(routes.foldLeft(empty[(Method, FP), Service[Request, Response]]) {
      (currentBinding, route) =>
        val filter = identify(route).andThen(security.filter).andThen(validationFilter(route)).andThen(moduleFilter)
        currentBinding.orElse(route.toPf(filter, basePath))
    })

  /**
    * Set the API security for this module. This is implemented though a Filter which is invoked before the
    * parameter validation takes place, and will return Unauthorized HTTP response codes when a request does
    * not pass authentication.
    */
  def securedBy(newSecurity: Security): RouteModule[RQ, RS] =
    new RouteModule[RQ, RS](basePath, moduleRenderer, descriptionRoutePath, routes, newSecurity, moduleFilter)

  /**
    * Override the path from the root of this module (incoming) where the default module description will live.
    */
  def withDescriptionPath(newDefaultRoutePath: ModifyPath): RouteModule[RQ, RS] =
    new RouteModule[RQ, RS](basePath, moduleRenderer, newDefaultRoutePath, routes, security, moduleFilter)

  /**
    * Attach described Route(s) to the module. Request matching is attempted in the same order as in which this method is called.
    */
  def withRoute(newRoutes: ServerRoute[RQ, RS]*): RouteModule[RQ, RS] = new RouteModule(basePath, moduleRenderer, descriptionRoutePath, routes ++ newRoutes, security, moduleFilter)

  /**
    * Attach described Route(s) to the module. Request matching is attempted in the same order as in which this method is called.
    */
  def withRoutes(newRoutes: Iterable[ServerRoute[RQ, RS]]*): RouteModule[RQ, RS] = newRoutes.flatten.foldLeft(this)(_.withRoute(_))

  private def withDefault(otherRoutes: ServiceBinding): ServiceBinding = {
    val descriptionRoute: ServerRoute[Request, Response] = new UnboundRoute0(RouteSpec("Description route"), Method.Get, descriptionRoutePath) bindTo {
      Service.mk {
        _: Request => Future(moduleRenderer.description(basePath, security, routes))
      }
    }

    val fallback: ServiceBinding = {
      case _ => Service.mk {
        request: Request => Future(moduleRenderer.notFound(request))
      }
    }

    val value: PartialFunction[(Method, FP), Service[Request, Response]] = otherRoutes.orElse(descriptionRoute.toPf(identify(descriptionRoute), basePath))
    val totalPf = value.orElse(fallback)

    {
      case (method, path: FP) if path.startsWith(basePath) => totalPf.apply((method, path))
    }
  }

  private def identify(route: ServerRoute[_, _]) = Filter.mk[Request, Response, Request, Response] {
    (request, svc) => {
      val url = if (route.describeFor(basePath).length == 0) "/" else route.describeFor(basePath)
      request.headerMap(Headers.IDENTIFY_SVC_HEADER) = request.method + ":" + url
      svc(request)
    }
  }
}
