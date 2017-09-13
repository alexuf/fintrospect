package lens

import com.twitter.finagle.http.Message
import com.twitter.io.Buf.ByteBuffer
import io.fintrospect.ContentType

class BodyLensSpec[OUT](val metas: List[Meta], val contentType: ContentType, val get: LensGet[Message, ByteBuffer, OUT]) {
  /**
    * Create a lens for this Spec
    */
  def toLens: BodyLens[OUT] = {
    val getLens = get("")
    new BodyLens(metas, contentType, (it: Message) => getLens(it).headOption.getOrElse(throw LensFailure(metas.map(Missing))))
  }

  /**
    * Create another BodyLensSpec which applies the uni-directional transformation to the result. Any resultant Lens can only be
    * used to extract the final type from a Body.
    */
  def map[NEXT](nextIn: (OUT) => NEXT): BodyLensSpec[NEXT] = new BodyLensSpec(metas, contentType, get.map(nextIn))
}

/**
  * Represents a bi-directional extraction of an entity from a target Body, or an insertion into a target Body.
  */
class BiDiBodyLensSpec[OUT](metas: List[Meta], contentType: ContentType,
                            get: LensGet[Message, ByteBuffer, OUT],
                            private val set: LensSet[Message, ByteBuffer, OUT])
  extends BodyLensSpec[OUT](metas, contentType, get) {

  /**
    * Create another BiDiBodyLensSpec which applies the bi-directional transformations to the result. Any resultant Lens can be
    * used to extract or insert the final type from/into a Body.
    */
  def map[NEXT](nextIn: (OUT) => NEXT, nextOut: (NEXT) => OUT) = new BiDiBodyLensSpec(metas, contentType, get.map(nextIn), set.map(nextOut))

  /**
    * Create a lens for this Spec
    */
  override def toLens: BiDiBodyLens[OUT] = {
    val getLens = get("")
    val setLens = set("")
    new BiDiBodyLens(metas, contentType,
      (it: Message) => getLens(it).headOption.getOrElse(throw LensFailure(metas.map(Missing))),
      (out: OUT, target: Message) => setLens(if (out == null) List.empty else List(out), target)
    )
  }
}