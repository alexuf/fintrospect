package lens

import io.fintrospect.parameters.ParamType

/**
  * Represents a uni-directional extraction of a list of entities from a target.
  */
trait MultiLensSpec[IN, OUT] {
  def defaulted(name: String, default: List[OUT], description: String = null): Lens[IN, List[OUT]]

  def optional(name: String, description: String = null): Lens[IN, Option[List[OUT]]]

  def required(name: String, description: String = null): Lens[IN, List[OUT]]
}

/**
  * Represents a bi-directional extraction of a list of entities from a target, or an insertion into a target.
  */
trait BiDiMultiLensSpec[IN, OUT] extends MultiLensSpec[IN, OUT] {
  override def defaulted(name: String, default: List[OUT], description: String): BiDiLens[IN, List[OUT]]

  override def optional(name: String, description: String): BiDiLens[IN, Option[List[OUT]]]

  override def required(name: String, description: String): BiDiLens[IN, List[OUT]]
}

/**
  * Represents a uni-directional extraction of an entity from a target.
  */
class LensSpec[IN, MID, OUT](protected val location: String, protected val paramMeta: ParamType, private val get: LensGet[IN, MID, OUT]) {

  /**
    * Create another LensSpec which applies the uni-directional transformation to the result. Any resultant Lens can only be
    * used to extract the final type from a target.
    */
  def map[NEXT](nextIn: (OUT) => NEXT) = new LensSpec(location, paramMeta, get.map(nextIn))


  /**
    * Make a concrete Lens for this spec that falls back to the default value if no value is found in the target.
    */
  def defaulted(name: String, default: OUT, description: String = null): Lens[IN, OUT] =
    new Lens(Meta(false, location, paramMeta, name, description), it => {
      val out = get(name)(it)
      if (out.isEmpty) default else out.head
    })


  /**
    * Make a concrete Lens for this spec that looks for an optional value in the target.
    */
  def optional(name: String, description: String = null): Lens[IN, Option[OUT]] = {
    val meta = Meta(false, location, paramMeta, name, description)
    new Lens(meta, it => get(name)(it).headOption)
  }

  /**
    * Make a concrete Lens for this spec that looks for a required value in the target.
    */
  def required(name: String, description: String = null): Lens[IN, OUT] = {
    val meta = Meta(false, location, paramMeta, name, description)
    new Lens(meta, (it: IN) => {
      val out = get(name)(it)
      if (out.isEmpty) throw LensFailure(null, Missing(meta)) else out.head
    })
  }

  val multi = new MultiLensSpec[IN, OUT] {
    /**
      * Make a concrete Lens for this spec that falls back to the default list of values if no values are found in the target.
      */
    override def defaulted(name: String, default: List[OUT], description: String = null): Lens[IN, List[OUT]] =
      new Lens(Meta(false, location, paramMeta, name, description), (it: IN) => {
        val out = get(name)(it)
        if (out.isEmpty) default else out
      })


    /**
      * Make a concrete Lens for this spec that looks for an optional list of values in the target.
      */
    override def optional(name: String, description: String = null): Lens[IN, Option[List[OUT]]] = {
      val meta = Meta(false, location, paramMeta, name, description)
      new Lens(meta, (it: IN) => {
        val out = get(name)(it)
        if (out.isEmpty) None else Some(out)
      })
    }

    /**
      * Make a concrete Lens for this spec that looks for a required list of values in the target.
      */
    override def required(name: String, description: String = null): Lens[IN, List[OUT]] = {
      val meta = Meta(true, location, paramMeta, name, description)
      new Lens(meta, (it: IN) => {
        val out = get(name)(it)
        if (out.isEmpty) throw LensFailure(null, Missing(meta)) else out
      })
    }
  }
}


