package examples.explicit

import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.{Response, Status}
import com.twitter.util.Future
import io.fintrospect.RS0
import io.fintrospect.parameters.{Path, Query}

object ExplicitApp extends App {

  def svc0(foo: String): Future[Response] = Future.value(Response(Status.Ok))

  def svc(foo: String, bar: String, queryB: (Option[Boolean], Int)): Future[Response] = Future.value(Response(Status.Ok))

  RS0("name")
    .taking(Query.optional.boolean("queryB"))
    .taking(Query.required.int("queryInt"))
    .at(Get) / Path.string("foo") / Path.string("bar") bindTo (svc _).curried

  val a = RS0("name")
    .taking(Query.required.string("bob"))
    .at(Get) bindTo svc0
}
