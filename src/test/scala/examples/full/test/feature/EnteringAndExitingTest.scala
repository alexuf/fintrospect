package examples.full.test.feature

import com.twitter.finagle.http.Method.Post
import com.twitter.finagle.http.Request
import com.twitter.finagle.http.Status._
import examples.full.main._
import examples.full.test.env.RunningTestEnvironment
import org.scalatest.{FunSpec, ShouldMatchers}

class EnteringAndExitingTest extends FunSpec with ShouldMatchers with RunningTestEnvironment {

  describe("when a user is unknown") {
    it("does not allow user in") {
      val request = Request(Post, "/security/knock?username=Rita")
      request.headerMap("key") = "realSecret"
      env.responseTo(request).status shouldBe NotFound
    }
  }

  describe("entry endpoint") {
    it("rejects missing username in entry endpoint") {
      val request = Request(Post, "/security/knock")
      request.headerMap("key") = "realSecret"
      env.responseTo(request).status shouldBe BadRequest
    }

    it("is protected with a secret key") {
      val request = Request(Post, "/security/knock?username=Bob")
      request.headerMap("key") = "fakeSecret"
      env.responseTo(request).status shouldBe Unauthorized
    }
  }

  describe("exit endpoint") {
    it("rejects missing username in exit endpoint") {
      val request = Request(Post, "/security/bye")
      request.headerMap("key") = "realSecret"
      env.responseTo(request).status shouldBe BadRequest
    }

    it("is protected with a secret key") {
      val request = Request(Post, "/security/bye?username=Bob")
      request.headerMap("key") = "fakeSecret"
      env.responseTo(request).status shouldBe Unauthorized
    }
  }

  describe("when a user is not in the building") {
    it("does not allow user to exit") {
      val request = Request(Post, "/security/bye?username=Bob")
      request.headerMap("key") = "realSecret"
      env.responseTo(request).status shouldBe BadRequest
    }
  }

  describe("when a known user tries to enter") {
    env.userDirectory.contains(User(Id(1), Username("Bob"), EmailAddress("bob@bob.com")))

    it("allows the user in") {
      val request = Request(Post, "/security/knock?username=Bob")
      request.headerMap("key") = "realSecret"
      env.responseTo(request).status shouldBe Accepted
    }

    it("logs entry") {
      env.entryLogger.entries shouldBe Seq(UserEntry("Bob", goingIn = true, env.clock.millis()))
    }

    it("does not allow user to enter once inside") {
      val request = Request(Post, "/security/knock?username=Bob")
      request.headerMap("key") = "realSecret"
      env.responseTo(request).status shouldBe BadRequest
    }

    it("allows the user to exit") {
      val request = Request(Post, "/security/bye?username=Bob")
      request.headerMap("key") = "realSecret"
      env.responseTo(request).status shouldBe Accepted
    }

    it("logs exit") {
      env.entryLogger.entries shouldBe Seq(
        UserEntry("Bob", goingIn = true, env.clock.millis()),
        UserEntry("Bob", goingIn = false, env.clock.millis())
      )
    }
  }
}