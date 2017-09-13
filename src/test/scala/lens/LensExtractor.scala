package lens

trait LensExtractor[IN, OUT] extends Function[IN, OUT] {
  /**
    * Lens operation to get the value from the target
    *
    * @throws LensFailure if the value could not be retrieved from the target (missing/invalid etc)
    */
  override def apply(target: IN): OUT

  /**
    * Lens operation to get the value from the target. Synonym for apply(IN)
    *
    * @throws LensFailure if the value could not be retrieved from the target (missing/invalid etc)
    */
  def extract(target: IN): OUT = apply(target)

  /**
    * Lens operation to get the value from the target. Synonym for apply(IN)
    *
    * @throws LensFailure if the value could not be retrieved from the target (missing/invalid etc)
    */
  def <--(target: IN): OUT = apply(target)
}
