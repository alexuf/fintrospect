package lens

import com.twitter.finagle.http.Message
import io.fintrospect.ContentType
import io.fintrospect.parameters.StringParamType

class BodyLens[FINAL](val metas: List[Meta], protected val contentType: ContentType, protected val get: (Message) => FINAL) extends LensExtractor[Message, FINAL] {
  override def apply(target: Message): FINAL = try {
    get(target)
  } catch {
    case e: LensFailure => throw e
    case e: Exception => throw LensFailure(metas.map(Invalid), e)
  }
}

class BiDiBodyLens[FINAL](meta: List[Meta], contentType: ContentType, get: (Message) => FINAL, private val set: (FINAL, Message) => Message)
  extends BodyLens[FINAL](meta, contentType, get) with LensInjector[Message, FINAL] {

  override def apply[R <: Message](value: FINAL, target: R): R = set(value, target).asInstanceOf[R]
}


/**
  * Modes for determining if a passed content type is acceptable.
  */
trait ContentNegotiation {
  def apply(expected: ContentType, actual: ContentType)
}

object ContentNegotiation {

  private val meta = Meta(false, "header", StringParamType, "Content-Type")

  /**
    * The received Content-type header passed back MUST equal the expected Content-type, including directive
    */
  val Strict =
    new ContentNegotiation {
      override def apply(expected: ContentType, actual: ContentType) {
        if (actual != expected) throw LensFailure(null, Unsupported(meta))
      }
    }
  /**
    * The received Content-type header passed back MUST equal the expected Content-type, not including the directive
    */
  val StrictNoDirective =
    new ContentNegotiation {
      override def apply(expected: ContentType, actual: ContentType) {
        if (actual != expected) throw LensFailure(null, Unsupported(meta))
//        if (expected.value != actual ?
//        .value
//        ) throw LensFailure(null, Unsupported(Header.CONTENT_TYPE.head))
      }
    }

  /**
    * If present, the received Content-type header passed back MUST equal the expected Content-type, including directive
    */
  val NonStrict =
    new ContentNegotiation {
      override def apply(expected: ContentType, actual: ContentType) {
        if (actual != null && actual != expected) throw LensFailure(null, Unsupported(meta))
      }
    }

  /**
    * No validation is done on the received content type at all
    */
  val None =
    new ContentNegotiation {
      override def apply(expected: ContentType, actual: ContentType) {}
    }
}