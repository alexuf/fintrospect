package lens

import com.twitter.finagle.http.Request
import io.fintrospect.parameters.ParamType

class PathLens[FINAL](meta: Meta, get: (String) => FINAL)
  extends Lens[String, FINAL](meta, get) {
  override def toString(): String = "{${meta.name}}"
}

class BiDiPathLens[FINAL](meta: Meta, get: (String) => FINAL, private val set: (FINAL, Request) => Request)
  extends LensInjector[FINAL, Request] {
  override def apply[R <: FINAL](value: Request, target: R): R = set(target, value).asInstanceOf[R]
}

class PathLensSpec[OUT](protected val paramType: ParamType, get: LensGet[String, String, OUT]) {

  def of(name: String, description: String = null): PathLens[OUT] = {
    val getLens = get(name)
    val meta = Meta(true, "path", paramType, name, description)
    new PathLens(meta, getLens(_).headOption.getOrElse(throw LensFailure(null, Missing(meta))))
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
}
