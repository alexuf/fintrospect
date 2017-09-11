package lens

class LensSet[IN, MID, OUT] private(private val rootFn: (String, List[MID], IN) => IN, private val fn: (OUT) => MID) {
  def map[NEXT](nextFn: (NEXT) => OUT): LensSet[IN, MID, NEXT] = new LensSet[IN, MID, NEXT](rootFn, (value: NEXT) => fn(nextFn(value)))
}

object LensSet {
  def apply[IN, OUT](rootFn: (String, List[OUT], IN) => IN): LensSet[IN, OUT, OUT] = new LensSet(rootFn, (i: OUT) => i)
}