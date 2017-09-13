package lens


/**
  * A Lens provides the uni-directional extraction of an entity from a target.
  */
class Lens[IN, FINAL](val meta: Meta,
                      protected val get: (IN) => FINAL) extends LensExtractor[IN, FINAL] with Iterable[Meta] {

  override def iterator(): Iterator[Meta] = List(meta).iterator

  override def toString(): String = if (meta.required) "Required " else "Optional" + s"} ${meta.location} '${meta.name}'"

  override def apply(target: IN): FINAL = {
    try {
      get(target)
    } catch {
      case e: LensFailure => throw e
      case e: Exception => throw LensFailure(e, Invalid(meta))
    }
  }
}

/**
  * A BiDiLens provides the bi-directional extraction of an entity from a target, or the insertion of an entity
  * into a target.
  */
class BiDiLens[IN, FINAL](meta: Meta, get: (IN) => FINAL, private val set: (FINAL, IN) => IN)
  extends Lens[IN, FINAL](meta, get) with LensInjector[IN, FINAL] {

  override def apply[R <: IN](value: FINAL, target: R): R = set(value, target).asInstanceOf[R]
}

