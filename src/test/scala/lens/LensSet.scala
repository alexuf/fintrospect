package lens

class LensSet[IN, MID, OUT] (private val rootFn: (String, Seq[MID], IN) => IN, private val fn: (OUT) => MID) {

  def apply(name: String): (Seq[OUT], IN) => IN = (values: Seq[OUT], target: IN) => rootFn(name, values.map(fn), target)

  def map[NEXT](nextFn: (NEXT) => OUT): LensSet[IN, MID, NEXT] = new LensSet[IN, MID, NEXT](rootFn, (value: NEXT) => fn(nextFn(value)))
}

object LensSet {
  def apply[IN, OUT](rootFn: (String, Seq[OUT], IN) => IN): LensSet[IN, OUT, OUT] = new LensSet(rootFn, (i: OUT) => i)
}