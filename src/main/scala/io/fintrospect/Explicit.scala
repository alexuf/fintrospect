package io.fintrospect

import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.path.{->, /, Path}
import com.twitter.finagle.http.{Method, Request, Response}
import com.twitter.finagle.{Filter, Service}
import io.fintrospect.ModuleSpec.ModifyPath
import io.fintrospect.TTTypes.ReqParam
import io.fintrospect.parameters.{Header, Parameter, PathParameter, Query, Retrieval, Validatable}

object TTTypes {
  type ReqParam[A] = Parameter with Retrieval[A, Request] with Validatable[A, Request]
}

trait ExplicitRouteSpec {
  val params: Seq[Parameter with Retrieval[_, Request] with Validatable[_, Request]]
}

case class ExplicitRouteSpec0(summary: String,
                              description: Option[String]) extends ExplicitRouteSpec {

  val params = Nil

  def taking[A](rp: ReqParam[A]): ExplicitRouteSpec1[A] = ExplicitRouteSpec1(this, rp)

  def at(method: Method) = new ExplicitIncompletePath0(this, method, identity)
}

case class ExplicitRouteSpec1[A](base: ExplicitRouteSpec0,
                                 p0: ReqParam[A]) extends ExplicitRouteSpec {
  val params = Seq(p0)

  def at(method: Method) = {
    val a = (request: Request) => p0 <-- request
    new ExplicitIncompletePath0(this, method, identity)
  }
}

abstract class ExplicitServerRoute[RQ, RS, R <: ExplicitRouteSpec](val routeSpec: R,
                                                                   val method: Method,
                                                                   pathFn: Path => Path,
                                                                   val pathParams: PathParameter[_]*) {

  def missingOrFailedFrom(request: Request) = routeSpec.params.map(_.validate(request)).collect { case Left(l) => l }

  def matches(actualMethod: Method, basePath: Path, actualPath: Path) = actualMethod == method && actualPath == pathFn(basePath)

  def toPf(filter: Filter[Request, Response, RQ, RS], basePath: Path): PartialFunction[(Method, Path), Service[Request, Response]]

  def describeFor(basePath: Path): String = (pathFn(basePath).toString +: pathParams.map(_.toString())).mkString("/")
}


object ExplicitIncompletePath {
  def apply[R <: ExplicitRouteSpec](routeSpec: R, method: Method): ExplicitIncompletePath0[R] = new ExplicitIncompletePath0(routeSpec, method, identity)
}

trait ExplicitIncompletePath {
  type ModifyPath = Path => Path
  val routeSpec: ExplicitRouteSpec
  val method: Method
  val pathFn: ModifyPath
}

class ExplicitIncompletePath0[R <: ExplicitRouteSpec](val routeSpec: R, val method: Method, val pathFn: ModifyPath) extends ExplicitIncompletePath {
  def /(part: String) = new ExplicitIncompletePath0(routeSpec, method, pathFn = pathFn.andThen(_ / part))

  def /[T](pp0: PathParameter[T]) = new ExplicitIncompletePath1(routeSpec, method, pathFn, pp0)

  def bindTo[RQ, RS](svc: Service[RQ, RS]): ExplicitServerRoute[RQ, RS, R] = bindTo(() => svc)

  def bindTo[RQ, RS](fn: () => Service[RQ, RS]): ExplicitServerRoute[RQ, RS, R] = new ExplicitServerRoute[RQ, RS, R](routeSpec, method, pathFn) {
    override def toPf(filter: Filter[Request, Response, RQ, RS], basePath: Path) = {
      case actualMethod -> path if matches(actualMethod, basePath, path) => filter.andThen(fn())
    }
  }
}

class ExplicitIncompletePath1[A, R <: ExplicitRouteSpec](val routeSpec: R, val method: Method, val pathFn: ModifyPath,
                                                         pp1: PathParameter[A]) extends ExplicitIncompletePath {

  def bindTo[RQ, RS](fn: (A) => Service[RQ, RS]): ExplicitServerRoute[RQ, RS, R] = new ExplicitServerRoute[RQ, RS, R](routeSpec, method, pathFn, pp1) {
    override def toPf(filter: Filter[Request, Response, RQ, RS], basePath: Path) = {
      case actualMethod -> path / pp1(s1) if matches(actualMethod, basePath, path) => filter.andThen(fn(s1))
    }
  }
}

object TheTest {
  ExplicitRouteSpec0("", null)
    .taking(Header.required.string("bob"))
    .at(Get)
}
