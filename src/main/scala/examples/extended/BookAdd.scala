package examples.extended

import com.twitter.finagle.Service
import com.twitter.finagle.http.{Method, Request, Response, Status}
import io.fintrospect.formats.Argo.ResponseBuilder._
import io.fintrospect.parameters.{Body, Path}
import io.fintrospect.{ResponseSpec, RouteSpec}

class BookAdd(books: Books) {
  private val exampleBook = Book("the title", "the author", 666)
  private val bookExistsResponse = Conflict("Book with that ISBN exists")
  private val jsonBody = Body.json("book content", exampleBook.toJson)

  private def addBook(isbn: String) = Service.mk {
    request: Request =>
      books.lookup(isbn) match {
        case Some(_) => bookExistsResponse
        case None => {
          val book = Book.unapply(jsonBody <-- request).get
          books.add(isbn, book)
          Created(book.toJson)
        }
      }
  }

  val route = RouteSpec("add book by isbn number", "This book must not already exist")
    .body(jsonBody)
    .returning(ResponseSpec.json(Status.Created -> "we added your book", exampleBook.toJson))
    .returning(bookExistsResponse)
    .at(Method.Post) / "book" / Path.string("isbn", "the isbn of the book") bindTo addBook
}


