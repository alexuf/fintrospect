package lens

import java.util

import com.twitter.finagle.http.Request
import io.fintrospect.parameters.StringParamType
import org.jboss.netty.handler.codec.http.QueryStringDecoder

import scala.collection.JavaConverters._

object Query extends BaseBidiLensSpec[Request]("query", StringParamType,
  new LensGet[Request, String, String]((name: String, target: Request) => target.params.getAll(name).toList, identity[String]),
  new LensSet[Request, String, String](
    (name: String, values: Seq[String], target: Request) => values.foldLeft(target) {
      (m: Request, next: String) => {
        val parameters: util.Map[String, util.List[String]] = new QueryStringDecoder(m.uri).getParameters
        val asdas = parameters.asScala.flatMap(it => it._2.asScala.map(it._1 -> _)).toList ++ List(name -> next)
        m.uri(Request.queryString(m.path, asdas: _*))
        m
      }
    }, identity[String])
)