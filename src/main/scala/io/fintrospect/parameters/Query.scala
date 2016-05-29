package io.fintrospect.parameters

import com.twitter.finagle.http.Request
import io.fintrospect.parameters.types._

/**
  * Parameters which are bound to the query part of a URL
  */
object Query {

  trait Mandatory[T] extends MandatoryParameter[Request, T, QueryBinding] {
    self: ExtParameter[Request, T, QueryBinding] =>
  }

  trait MandatorySeq[T] extends MandatoryParameter[Request, Seq[T], QueryBinding] {
    self: ExtParameter[Request, Seq[T], QueryBinding] =>
  }

  trait Optional[T] extends OptionalParameter[Request, T, QueryBinding] {
    self: ExtParameter[Request, T, QueryBinding] =>
  }

  trait OptionalSeq[T] extends OptionalParameter[Request, Seq[T], QueryBinding] {
    self: ExtParameter[Request, Seq[T], QueryBinding] =>
  }


  val required = new Parameters[QueryParameter, Mandatory] with MultiParameters[MultiQueryParameter, MandatorySeq] {
    override def apply[T](spec: ParameterSpec[T]) = new SingleQueryParameter(spec) with Mandatory[T]

    override val multi = new Parameters[MultiQueryParameter, MandatorySeq] {
      override def apply[T](spec: ParameterSpec[T]) = new MultiQueryParameter(spec) with MandatorySeq[T]
    }
  }

  val optional = new Parameters[QueryParameter, Optional] with MultiParameters[MultiQueryParameter, OptionalSeq] {
    override def apply[T](spec: ParameterSpec[T]) = new SingleQueryParameter(spec) with Optional[T]

    override val multi = new Parameters[MultiQueryParameter, OptionalSeq] {
      override def apply[T](spec: ParameterSpec[T]) = new MultiQueryParameter(spec) with OptionalSeq[T]
    }
  }
}
