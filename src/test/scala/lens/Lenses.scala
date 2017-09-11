package lens

import io.fintrospect.parameters.ParamType

class LensGet[IN, MID, OUT] private(private val rootFn: (String, IN) => List[MID], private val fn: (MID) => OUT) {
  def map[NEXT](nextFn: (OUT) => NEXT): LensGet[IN, MID, NEXT] = new LensGet[IN, MID, NEXT](rootFn, (i: MID) => nextFn(fn(i)))
}

object LensGet {
  def apply[IN, OUT](rootFn: (String, IN) => List[OUT]): LensGet[IN, OUT, OUT] = new LensGet(rootFn, (i: OUT) => i)
}

class LensSet[IN, MID, OUT] private(private val rootFn: (String, List[MID], IN) => IN, private val fn: (OUT) => MID) {
  def map[NEXT](nextFn: (NEXT) => OUT): LensSet[IN, MID, NEXT] = new LensSet[IN, MID, NEXT](rootFn, (value: NEXT) => fn(nextFn(value)))
}

object LensSet {
  def apply[IN, OUT](rootFn: (String, List[OUT], IN) => IN): LensSet[IN, OUT, OUT] = new LensSet(rootFn, (i: OUT) => i)
}

///**
//  * A Lens provides the uni-directional extraction of an entity from a target.
//  */
// class Lens[IN, FINAL](val meta: Meta,
//private val get: (IN) =] FINAL) : LensExtractor[IN, FINAL], Iterable[Meta] {
//
//  import shapeless.PolyDefns.=]
//
//  override defiterator(): Iterator[Meta] = listOf(meta).iterator()
//
//  override deftoString(): String = "${if (meta.required) "Required" else "Optional"} ${meta.location} '${meta.name}'"
//
//  override  definvoke(target: IN): FINAL = try {
//  get(target)
//} catch (e: LensFailure) {
//  throw e
//} catch (e: Exception) {
//  throw LensFailure(Invalid(meta), cause = e)
//}
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
//  override  def[R : IN] invoke(value: FINAL, target: R): R = set(value, target) as R
//}


case class Meta(required: Boolean, location: String, paramMeta: ParamType, name: String, description: Option[String] = null)