package lens

import lens.Failure.Type

abstract sealed class Failure(val fType: Failure.Type.Type) {
  val meta: Meta
}

object Failure {

  object Type extends Enumeration {
    type Type = Value
    val Invalid, Missing, Unsupported = Value
  }

}

case class Missing(override val meta: Meta) extends Failure(Type.Missing) {
  override def toString: String = "${meta.location} '${meta.name}' is required"
}

case class Invalid(override val meta: Meta) extends Failure(Type.Invalid) {
  override def toString: String = "${meta.location} '${meta.name}' must be ${meta.paramMeta.value}"
}

case class Unsupported(override val meta: Meta) extends Failure(Type.Unsupported) {
  override def toString: String = "${meta.location} '${meta.name}' is not acceptable"
}


case class LensFailure(failures: List[Failure], cause: Exception = null) extends Exception(failures.map {
  _.toString()
}.mkString(","), cause) {

  def overall(): Failure.Type.Type = {
    val all = failures.map { i => i.fType }
    if (all.contains(Failure.Type.Unsupported)) Failure.Type.Unsupported
    else if (all.isEmpty || all.contains(Failure.Type.Invalid)) Failure.Type.Invalid
    else Type.Missing
  }
}

object LensFailure {
  def apply(cause: Exception, failures: Failure*): LensFailure = LensFailure(failures.toList, cause)
}