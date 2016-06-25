package app

import com.twitter.util.{Future => TwitterF, Promise => PromiseF}

import scala.concurrent.{ExecutionContext, Future => ScalaF, Promise => ScalaP}
import scala.language.implicitConversions

object FutureConversions {
  implicit def asScala[T](twitterF: TwitterF[T])(implicit ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global): ScalaF[T] = {
    val scalaP = ScalaP[T]()
    twitterF.onSuccess { r: T =>
      scalaP.success(r)
    }
    twitterF.onFailure { e: Throwable =>
      scalaP.failure(e)
    }
    scalaP.future
  }

  implicit def asTwitter[T](scalaF: ScalaF[T])(implicit ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global): TwitterF[T] = {
    val tP = PromiseF[T]()
    scalaF.onSuccess { case r: T => tP.setValue(r)}
    scalaF.onFailure { case e: Throwable => tP.setException(e)}
    tP
  }
}