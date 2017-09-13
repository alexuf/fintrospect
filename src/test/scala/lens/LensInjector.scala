package lens

trait LensInjector[IN, OUT] {
  /**
    * Lens operation to set the value into the target
    */
  def apply[R <: IN](value: OUT, target: R): R

  /**
    * Lens operation to set the value into the target. Synomym for apply(OUT, IN)
    */
  def inject[R <: IN](value: OUT, target: R): R = apply(value, target)

  /**
    * Lens operation to set the value into the target. Synomym for apply(OUT, IN)
    */
  def -->[R <: IN](value: OUT): (R) => R = apply(value, _)
}

object Injector {
  implicit class Injector[T](r: T) {
    def inject(a: ((T) => T)*) = a.foldLeft(r) {
      (memo, next) => next(memo)
    }
  }
}