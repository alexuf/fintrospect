package lens

import io.fintrospect.parameters.{MultiPartFile, StringParamType}

/**
  * The body entity of a encoded HTML form. Basically a wrapper for Form construction and field extraction.
  */
case class Form protected[lens](fields: Map[String, Seq[String]] = Map.empty,
                                files: Map[String, Seq[MultiPartFile]] = Map.empty,
                                errors: Seq[Failure] = Nil) {

  def isValid = errors.isEmpty

  def +(key: String, value: String) = Form(fields + (key -> (value :: fields.getOrElse(key, Seq()).toList)), files, errors)

  def +(key: String, value: MultiPartFile) = Form(fields, files + (key -> (value :: files.getOrElse(key, Seq()).toList)), errors)
}

object FormField extends BaseBidiLensSpec[Form]("form", StringParamType,
  new LensGet[Form, String, String]((name: String, target: Form) => target.fields(name), identity[String]),
  new LensSet[Form, String, String](
    (name: String, values: Seq[String], target: Form) =>
      values.foldLeft(target) {
        (m: Form, next: String) => m + (name, next)
      }, identity[String])
)