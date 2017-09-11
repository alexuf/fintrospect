package lens

trait LensInjector[IN, OUT] {
  /**
    * Lens operation to set the value into the target
    */
  def apply[R <: IN](value: OUT, target: R): R

  /**
    * Lens operation to set the value into the target. Synomym for invoke(OUT, IN)
    */
  def inject[R <: IN](value: OUT, target: R): R = apply(value, target)

  /**
    * Bind this Lens to a value, so we can set it into a target
    */
  def of[R <: IN](value: OUT): (R) => R = apply(value, _)
}
