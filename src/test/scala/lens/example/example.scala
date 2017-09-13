package lens.example

import com.twitter.finagle.http.{Method, Request}
import io.fintrospect.ContentTypes
import lens.Injector._
import lens.{Body, Header}

object example extends App{

  private val value = Body.string(ContentTypes.APPLICATION_ATOM_XML).toLens

  case class Bob(i: Int)

  val a = Header.int().map(Bob, (s: Bob) => s.i).required("booo")
  println(Request(Method.Get, "/asd").inject(a --> Bob(123), value --> "asdasd").headerMap)

}
