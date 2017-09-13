package lens

class LensGet[IN, MID, OUT] (private val rootFn: (String, IN) => Seq[MID], private val fn: (MID) => OUT) {

  def apply(name: String): (IN) => Seq[OUT] = (target: IN) => rootFn(name, target).map(fn)

  def map[NEXT](nextFn: (OUT) => NEXT): LensGet[IN, MID, NEXT] = new LensGet[IN, MID, NEXT](rootFn, (i: MID) => nextFn(fn(i)))
}

object LensGet {
  def apply[IN, OUT](rootFn: (String, IN) => Seq[OUT]): LensGet[IN, OUT, OUT] = new LensGet(rootFn, (i: OUT) => i)
}