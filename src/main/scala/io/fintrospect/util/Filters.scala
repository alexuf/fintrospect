package io.fintrospect.util

import java.nio.charset.StandardCharsets.ISO_8859_1
import java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME
import java.time.{Clock, Duration, ZonedDateTime}
import java.util.Base64

import com.twitter.finagle.Filter
import com.twitter.finagle.http.{Request, Response}
import io.fintrospect.configuration.{Authority, Credentials}
import io.fintrospect.{ContentType, Headers}

/**
  * Useful filters
  */
object Filters {

  /**
    * These filters operate on Requests (pre-flight)
    */
  object Request {
    def AddContentType[T](contentType: ContentType) = Filter.mk[Request, T, Request, T] {
      (req, svc) => {
        req.headerMap("Content-Type") = contentType.value
        svc(req)
      }
    }

    def AddHost[T](authority: Authority) = Filter.mk[Request, T, Request, T] {
      (req, svc) => {
        req.headerMap("Host") = authority.toString
        svc(req)
      }
    }

    def BasicAuthorization[T](credentials: Credentials) = Filter.mk[Request, T, Request, T] {
      (req, svc) => {
        val base64Credentials = Base64.getEncoder.encodeToString(s"${credentials.username}:${credentials.password}".getBytes(ISO_8859_1))
        req.headerMap("Authorization") = "Basic " + base64Credentials.trim
        svc(req)
      }
    }
  }

  /**
    * These filters operate on Responses (post-flight)
    */
  object Response {

    def AddDate[T](clock: Clock = Clock.systemUTC()) = Filter.mk[T, Response, T, Response] {
      (req, svc) => {
        svc(req)
          .map(rsp => {
            rsp.headerMap("Date") = RFC_1123_DATE_TIME.format(ZonedDateTime.now(clock))
            rsp
          })
      }
    }

    def ReportingRouteLatency(clock: Clock)(recordFn: (String, Duration) => Unit) = Filter.mk[Request, Response, Request, Response] {
      (req, svc) => {
        val start = clock.instant()
        for {
          resp <- svc(req)
        } yield {
          val identifier = List(
            req.headerMap.get(Headers.IDENTIFY_SVC_HEADER)
              .map(_.replace('.', '_').replace(':', '.'))
              .getOrElse(req.method.toString() + ".UNMAPPED")
              .replace('/', '_'),
            resp.status.code / 100 + "xx",
            resp.status.code.toString).mkString(".")

          recordFn(identifier, Duration.between(start, clock.instant()))
          resp
        }
      }
    }
  }
}