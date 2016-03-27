package io.fintrospect

import com.twitter.finagle.Service
import com.twitter.finagle.http.path.{->, /, Path => FPath}
import com.twitter.finagle.http.{Method, Request, Response}
import com.twitter.util.Future
import io.fintrospect.TTTypes.ReqParam
import io.fintrospect.parameters.{Parameter, PathParameter, Retrieval, Validatable}

object TTTypes {
  type ReqParam[RA, VA] = Parameter with Retrieval[RA, Request] with Validatable[VA, Request]
}

abstract class SR {
  def toPf(): PartialFunction[(Method, FPath), Service[Request, Response]]
}

case class IP0_R0(m: Method) {
  def /[P0](p0: PathParameter[P0]) = IP1_R0(m, p0)

  def bindTo(svc: () => Future[Response]): SR = new SR {
    override def toPf() = {
      case actualMethod -> path => Service.mk { r: Request => svc() }
    }
  }
}

case class IP1_R0[P0](m: Method, p0: PathParameter[P0]) {
  def /[P1](p1: PathParameter[P1]) = IP2_R0(m, p0, p1)

  def bindTo(fn: P0 => Future[Response]) = new SR {
    override def toPf() = {
      case actualMethod -> path / p0(s1) => Service.mk { r: Request => fn(s1) }
    }
  }
}

case class IP2_R0[P0, P1](m: Method, p0: PathParameter[P0], p1: PathParameter[P1]) {
  def bindTo(fn: (P0, P1) => Future[Response]) = new SR {
    override def toPf() = {
      case actualMethod -> path / p0(s1) / p1(s2) => Service.mk { r: Request => fn(s1, s2) }
    }
  }
}

case class IP0_Rn[R, -T <: Rn[R]](m: Method, rn: Request => R) {
  def /[P0](p0: PathParameter[P0]) = IP1_Rn[P0, R, T](m, p0, rn)

  def bindTo(fn: R => Future[Response]): SR = new SR {
    override def toPf() = {
      case actualMethod -> path => Service.mk { r: Request => fn(rn(r)) }
    }
  }
}

case class IP1_Rn[P0, R, -T <: Rn[R]](m: Method, p0: PathParameter[P0], rn: Request => R) {
  def /[P1](p1: PathParameter[P1]) = IP2_Rn[P0, P1, R, T](m, p0, p1, rn)

  def bindTo(fn: P0 => R => Future[Response]) = new SR {
    override def toPf() = {
      case actualMethod -> path / p0(s0) => Service.mk { r: Request => fn(s0)(rn(r)) }
    }
  }
}

case class IP2_Rn[P0, P1, R, -T <: Rn[R]](m: Method, p0: PathParameter[P0], p1: PathParameter[P1], rn: Request => R) {
  def bindTo(fn: P0 => P1 => R => Future[Response]) = new SR {
    override def toPf() = {
      case actualMethod -> path / p0(s0) / p1(s1) => Service.mk { r: Request => fn(s0)(s1)(rn(r)) }
    }
  }
}

case class RS0(name: String) {
  def taking[RA, VA](r0: ReqParam[RA, VA]): RS1[RA, VA] = RS1[RA, VA](name, r0)

  def at(m: Method) = IP0_R0(m)
}

trait Rn[T] {
  def fn(r: Request): T
}

case class RS1[RA, VA](name: String, ra: ReqParam[RA, VA]) extends Rn[RA] {

  def taking[RB, VB](rb: ReqParam[RB, VB]) = RS2(name, ra, rb)

  def at(m: Method): IP0_Rn[RA, RS1[RA, VA]] = IP0_Rn[RA, RS1[RA, VA]](m, ra.from)

  override def fn(r: Request): RA = ra <-- r
}

case class RS2[RA, VA, RB, VB](name: String, ra: ReqParam[RA, VA], rb: ReqParam[RB, VB]) extends Rn[(RA, RB)] {
  def at(m: Method) = IP0_Rn[(RA, RB), RS2[RA, VA, RB, VB]](m, fn)

  override def fn(r: Request): (RA, RB) = (ra <-- r, rb <-- r)
}
