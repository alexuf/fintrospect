package lens

import io.fintrospect.parameters.ParamType

case class Meta(required: Boolean, location: String, paramMeta: ParamType, name: String, description: String = null)
