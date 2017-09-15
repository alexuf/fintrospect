package lens

import com.twitter.finagle.http.path.{->, /, Path => FP}
import com.twitter.finagle.http.{Method, Request, Response}
import com.twitter.finagle.{Filter, Service}
import io.fintrospect.RouteModule.ModifyPath

/**
  * An unbound route represents a RouteSpec which has been assigned a Method (and a partial or complete FP), but has
  * not yet been bound to a server or client Service.
  */
object UnboundRoute {
  def apply(routeSpec: RouteSpec, method: Method): UnboundRoute0 = new UnboundRoute0(routeSpec, method, identity)
}

trait UnboundRoute {
  type ModifyPath = FP => FP
  val routeSpec: RouteSpec
  val method: Method
  val pathFn: ModifyPath
}

/**
  * An UnboundRoute with 0 path parts
  */
class UnboundRoute0(val routeSpec: RouteSpec, val method: Method, val pathFn: ModifyPath) extends UnboundRoute {
  def /(part: String) = new UnboundRoute0(routeSpec, method, pathFn = pathFn.andThen(_ / part))

  def /[T](pp0: PathLens[T]) = new UnboundRoute1(routeSpec, method, pathFn, pp0)

  /**
    * Non-lazy bindTo(). Depending on use-case, can use this instead of the lazy version.
    */
  def bindTo[RQ, RS](svc: Service[RQ, RS]): ServerRoute[RQ, RS] = bindTo(() => svc)

  /**
    * Lazy bindTo(). Depending on use-case, can use this instead of the non-lazy version.
    */
  def bindTo[RQ, RS](fn: () => Service[RQ, RS]): ServerRoute[RQ, RS] = new ServerRoute[RQ, RS](routeSpec, method, pathFn) {
    override def toPf(filter: Filter[Request, Response, RQ, RS], basePath: FP) = {
      case actualMethod -> path if matches(actualMethod, basePath, path) => filter.andThen(fn())
    }
  }
}

/**
  * An UnboundRoute with 1 path part
  */
class UnboundRoute1[A](val routeSpec: RouteSpec, val method: Method, val pathFn: ModifyPath,
                       private val pp1: PathLens[A]) extends UnboundRoute {
  def /(part: String): UnboundRoute2[A, String] = /(Path.fixed(part))

  def /[B](pp2: PathLens[B]): UnboundRoute2[A, B] = new UnboundRoute2(routeSpec, method, pathFn, pp1, pp2)

  def bindTo[RQ, RS](fn: (A) => Service[RQ, RS]): ServerRoute[RQ, RS] = new ServerRoute[RQ, RS](routeSpec, method, pathFn, pp1) {
    override def toPf(filter: Filter[Request, Response, RQ, RS], basePath: FP) = {
      case actualMethod -> path / pp1(s1) if matches(actualMethod, basePath, path) => filter.andThen(fn(s1))
    }
  }

}

/**
  * An UnboundRoute with 2 path parts
  */
class UnboundRoute2[A, B](val routeSpec: RouteSpec, val method: Method, val pathFn: ModifyPath,
                          private val pp1: PathLens[A],
                          private val pp2: PathLens[B]) extends UnboundRoute {
  def /(part: String): UnboundRoute3[A, B, String] = /(Path.fixed(part))

  def /[C](pp3: PathLens[C]): UnboundRoute3[A, B, C] = new UnboundRoute3(routeSpec, method, pathFn, pp1, pp2, pp3)

  def bindTo[RQ, RS](fn: (A, B) => Service[RQ, RS]): ServerRoute[RQ, RS] = new ServerRoute[RQ, RS](routeSpec, method, pathFn, pp1, pp2) {
    override def toPf(filter: Filter[Request, Response, RQ, RS], basePath: FP) = {
      case actualMethod -> path / pp1(s1) / pp2(s2) if matches(actualMethod, basePath, path) => filter.andThen(fn(s1, s2))
    }
  }
}

/**
  * An UnboundRoute with 3 path parts
  */
class UnboundRoute3[A, B, C](val routeSpec: RouteSpec, val method: Method, val pathFn: ModifyPath,
                             private val pp1: PathLens[A],
                             private val pp2: PathLens[B],
                             private val pp3: PathLens[C]) extends UnboundRoute {
  def /(part: String): UnboundRoute4[A, B, C, String] = /(Path.fixed(part))

  def /[D](pp4: PathLens[D]): UnboundRoute4[A, B, C, D] = new UnboundRoute4(routeSpec, method, pathFn, pp1, pp2, pp3, pp4)

  def bindTo[RQ, RS](fn: (A, B, C) => Service[RQ, RS]): ServerRoute[RQ, RS] = new ServerRoute[RQ, RS](routeSpec, method, pathFn, pp1, pp2, pp3) {
    override def toPf(filter: Filter[Request, Response, RQ, RS], basePath: FP) = {
      case actualMethod -> path / pp1(s1) / pp2(s2) / pp3(s3) if matches(actualMethod, basePath, path) => filter.andThen(fn(s1, s2, s3))
    }
  }
}

/**
  * An UnboundRoute with 4 path parts
  */
class UnboundRoute4[A, B, C, D](val routeSpec: RouteSpec, val method: Method, val pathFn: ModifyPath,
                                private val pp1: PathLens[A],
                                private val pp2: PathLens[B],
                                private val pp3: PathLens[C],
                                private val pp4: PathLens[D]
                               ) extends UnboundRoute {
  def /(part: String): UnboundRoute5[A, B, C, D, String] = /(Path.fixed(part))

  def /[E](pp5: PathLens[E]): UnboundRoute5[A, B, C, D, E] = new UnboundRoute5(routeSpec, method, pathFn, pp1, pp2, pp3, pp4, pp5)

  def bindTo[RQ, RS](fn: (A, B, C, D) => Service[RQ, RS]): ServerRoute[RQ, RS] = new ServerRoute[RQ, RS](routeSpec, method, pathFn, pp1, pp2, pp3, pp4) {
    override def toPf(filter: Filter[Request, Response, RQ, RS], basePath: FP) = {
      case actualMethod -> path / pp1(s1) / pp2(s2) / pp3(s3) / pp4(s4) if matches(actualMethod, basePath, path) => filter.andThen(fn(s1, s2, s3, s4))
    }
  }
}

/**
  * An UnboundRoute with 5 path parts
  */
class UnboundRoute5[A, B, C, D, E](val routeSpec: RouteSpec, val method: Method, val pathFn: ModifyPath,
                                   private val pp1: PathLens[A],
                                   private val pp2: PathLens[B],
                                   private val pp3: PathLens[C],
                                   private val pp4: PathLens[D],
                                   private val pp5: PathLens[E]
                                  ) extends UnboundRoute {
  def /(part: String): UnboundRoute6[A, B, C, D, E, String] = /(Path.fixed(part))

  def /[F](pp6: PathLens[F]): UnboundRoute6[A, B, C, D, E, F] = new UnboundRoute6(routeSpec, method, pathFn, pp1, pp2, pp3, pp4, pp5, pp6)

  def bindTo[RQ, RS](fn: (A, B, C, D, E) => Service[RQ, RS]): ServerRoute[RQ, RS] = new ServerRoute[RQ, RS](routeSpec, method, pathFn, pp1, pp2, pp3, pp4, pp5) {
    override def toPf(filter: Filter[Request, Response, RQ, RS], basePath: FP) = {
      case actualMethod -> path / pp1(s1) / pp2(s2) / pp3(s3) / pp4(s4) / pp5(s5) if matches(actualMethod, basePath, path) => filter.andThen(fn(s1, s2, s3, s4, s5))
    }
  }
}

/**
  * An UnboundRoute with 6 path parts
  */
class UnboundRoute6[A, B, C, D, E, F](val routeSpec: RouteSpec, val method: Method, val pathFn: ModifyPath,
                                      private val pp1: PathLens[A],
                                      private val pp2: PathLens[B],
                                      private val pp3: PathLens[C],
                                      private val pp4: PathLens[D],
                                      private val pp5: PathLens[E],
                                      private val pp6: PathLens[F]
                                     ) extends UnboundRoute {
  def /(part: String): UnboundRoute7[A, B, C, D, E, F, String] = /(Path.fixed(part))

  def /[G](pp7: PathLens[G]): UnboundRoute7[A, B, C, D, E, F, G] = new UnboundRoute7(routeSpec, method, pathFn, pp1, pp2, pp3, pp4, pp5, pp6, pp7)

  def bindTo[RQ, RS](fn: (A, B, C, D, E, F) => Service[RQ, RS]): ServerRoute[RQ, RS] = new ServerRoute[RQ, RS](routeSpec, method, pathFn, pp1, pp2, pp3, pp4, pp5, pp6) {
    override def toPf(filter: Filter[Request, Response, RQ, RS], basePath: FP) = {
      case actualMethod -> path / pp1(s1) / pp2(s2) / pp3(s3) / pp4(s4) / pp5(s5) / pp6(s6) if matches(actualMethod, basePath, path) => filter.andThen(fn(s1, s2, s3, s4, s5, s6))
    }
  }
}

/**
  * An UnboundRoute with 7 path parts
  */
class UnboundRoute7[A, B, C, D, E, F, G](val routeSpec: RouteSpec, val method: Method, val pathFn: ModifyPath,
                                         private val pp1: PathLens[A],
                                         private val pp2: PathLens[B],
                                         private val pp3: PathLens[C],
                                         private val pp4: PathLens[D],
                                         private val pp5: PathLens[E],
                                         private val pp6: PathLens[F],
                                         private val pp7: PathLens[G]
                                        ) extends UnboundRoute {
  def bindTo[RQ, RS](fn: (A, B, C, D, E, F, G) => Service[RQ, RS]): ServerRoute[RQ, RS] = new ServerRoute[RQ, RS](routeSpec, method, pathFn, pp1, pp2, pp3, pp4, pp5, pp6, pp7) {
    override def toPf(filter: Filter[Request, Response, RQ, RS], basePath: FP) = {
      case actualMethod -> path / pp1(s1) / pp2(s2) / pp3(s3) / pp4(s4) / pp5(s5) / pp6(s6) / pp7(s7) if matches(actualMethod, basePath, path) => filter.andThen(fn(s1, s2, s3, s4, s5, s6, s7))
    }
  }
}
