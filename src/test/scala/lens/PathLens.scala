package lens

import com.twitter.finagle.http.Request
import io.fintrospect.parameters.ParamType

abstract class PathLens[FINAL](meta: Meta, get: (String) => FINAL)
  extends Lens[String, FINAL](meta, get) {
  override def toString(): String = "{${meta.name}}"

  def unapply(str: String): Option[FINAL]
}

abstract class BiDiPathLens[FINAL](meta: Meta, get: (String) => FINAL, private val set: (FINAL, Request) => Request)
  extends PathLens[FINAL](meta, get) with LensInjector[FINAL, Request] {
  override def apply[R <: FINAL](value: Request, target: R): R = set(target, value).asInstanceOf[R]
}

class PathLensSpec[OUT](protected val paramType: ParamType, protected val get: LensGet[String, String, OUT]) {

  def of(name: String, description: String = null): PathLens[OUT] = {
    val getLens = get(name)
    val meta = Meta(true, "path", paramType, name, description)
    new PathLens(meta, getLens(_).headOption.getOrElse(throw LensFailure(null, Missing(meta)))) {
      override def unapply(str: String): Option[OUT] = ???
    }
  }

  def map[NEXT](nextIn: (OUT) => NEXT) = new PathLensSpec(paramType, get.map(nextIn))
}

class BiDiPathLensSpec[OUT](paramType: ParamType,
                            get: LensGet[String, String, OUT],
                            private val set: LensSet[Request, String, OUT])
  extends PathLensSpec[OUT](paramType, get) {

  private def mapWithNewType[NEXT](nextIn: (OUT) => NEXT, nextOut: (NEXT) => OUT,
                                   newParamType: ParamType): BiDiPathLensSpec[NEXT] =
    new BiDiPathLensSpec(newParamType, get.map(nextIn), set.map(nextOut))

  def map[NEXT](nextIn: (OUT) => NEXT, nextOut: (NEXT) => OUT): BiDiPathLensSpec[NEXT] = mapWithNewType(nextIn, nextOut, paramType)


  override def of(name: String, description: String = null): BiDiPathLens[OUT] = {
    val getLens = get(name)
    val setLens = set(name)

    val meta = Meta(true, "path", paramType, name, description)

    new BiDiPathLens(meta,
      getLens(_).headOption.getOrElse(throw LensFailure(null, Missing(meta))),
      (out: OUT, target: Request) => setLens(List(out), target)) {
      def unapply(str: String): Option[OUT] = ???
    }
  }
}
