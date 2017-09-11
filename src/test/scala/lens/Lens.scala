package lens


///**
//  * A Lens provides the uni-directional extraction of an entity from a target.
//  */
//class Lens[IN, FINAL](val meta: Meta,
//                      private val get: (IN) => FINAL) extends LensExtractor[IN, FINAL] with Iterable[Meta] {
//
//  override def iterator(): Iterator[Meta] = List(meta).iterator()
//
////  override def toString(): String = "${if (meta.required) " Required " else " Optional"} ${meta.location} '${meta.name}'"
//
//  override def invoke(target: IN): FINAL = try {
//    get(target)
//  } catch (e: LensFailure) {
//    throw e
//  }
//
//  catch (e: Exception) {
//    throw LensFailure(Invalid(meta), cause = e)
//  }
//}

//
//  /**
//    * A BiDiLens provides the bi-directional extraction of an entity from a target, or the insertion of an entity
//    * into a target.
//    */
//  class BiDiLens[IN, FINAL](meta: Meta,
//  get: (IN) =] FINAL,
//  private val set: (FINAL, IN) =] IN) : LensInjector[IN, FINAL], Lens[IN, FINAL](meta, get) {
//
//  @Suppress("UNCHECKED_CAST")
//  override  def [R : IN] invoke(value: FINAL, target: R): R = set(value, target) as R
//}

