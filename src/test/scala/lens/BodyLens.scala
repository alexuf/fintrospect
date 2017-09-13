package lens

import com.twitter.finagle.http.Message
import io.fintrospect.ContentType

class BodyLens[FINAL](val metas: List[Meta], val contentType: ContentType, private val get: (Message) => FINAL) extends LensExtractor[Message, FINAL] {
  override def apply(target: Message): FINAL = try {
    get(target)
  } catch {
    case e: LensFailure => throw e
    case e: Exception => throw LensFailure(metas.map(Invalid), e)
  }
}

class BiDiBodyLens[FINAL](meta: Meta, get: (Message) => FINAL, private val set: (FINAL, Message) => Message)
  extends Lens[Message, FINAL](meta, get) with LensInjector[Message, FINAL] {

  override def apply[R <: Message](value: FINAL, target: R): R = set(value, target).asInstanceOf[R]
}

