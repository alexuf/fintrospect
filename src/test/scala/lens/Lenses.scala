package lens

import shapeless.Lens

/**
  * Represents a uni-directional extraction of a list of entities from a target.
  */
trait MultiLensSpec[IN, OUT] {
  def defaulted(name: String, default: List[OUT], description: String = null): Lens[IN, List[OUT]]

  def optional(name: String, description: String = null): Lens[IN, Option[List[OUT]]]

  def required(name: String, description: String = null): Lens[IN, List[OUT]]
}


class LensGet[IN, MID, OUT] private(private val rootFn: (String, IN) => List[MID], private val fn: (MID) => OUT) {
  def map[NEXT](nextFn: (OUT) => NEXT): LensGet[IN, MID, NEXT] = new LensGet[IN, MID, NEXT](rootFn, (i: MID) => nextFn(fn(i)))
}

object LensGet {
  def apply[IN, OUT](rootFn: (String, IN) => List[OUT]): LensGet[IN, OUT, OUT] = new LensGet(rootFn, (i: OUT) => i)
}


