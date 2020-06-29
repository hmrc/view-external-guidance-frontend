/*
 * Copyright 2020 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package filters

import akka.stream.Materializer
import javax.inject.Inject
import org.joda.time.{DateTime, Duration}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.{Matchers, OptionValues, WordSpecLike}
import play.api.http.{DefaultHttpFilters, HttpFilters}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.mvc._
import Results.Ok
import play.api.inject.bind
import play.api.routing.Router
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.Application
import uk.gov.hmrc.http.SessionKeys._
import uk.gov.hmrc.play.bootstrap.filters.frontend.SessionTimeoutFilterConfig
import filters.SessionTimeoutFilter._

import scala.concurrent.ExecutionContext

object SessionTimeoutFilterSpec {

  val now = new DateTime(2017, 1, 12, 14, 56)

  class Filters @Inject()(timeoutFilter: SessionTimeoutFilter) extends DefaultHttpFilters(timeoutFilter)

  class StaticDateSessionTimeoutFilter @Inject()(
    config: SessionTimeoutFilterConfig
  )(
    implicit
    ec: ExecutionContext,
    mat: Materializer)
      extends SessionTimeoutFilter(config)(ec, mat) {

    override val clock: DateTime = now
  }
}

class SessionTimeoutFilterSpec
    extends WordSpecLike
    with Matchers
    with ScalaFutures
    with OptionValues {

  import SessionTimeoutFilterSpec._


  private val Action = stubControllerComponents().actionBuilder

  val builder: GuiceApplicationBuilder = {
    import play.api.routing.sird._
    new GuiceApplicationBuilder()
      .router(
        Router.from {
          case GET(p"/test") =>
            Action { request =>
              Ok(
                Json.obj(
                  "session" -> request.session.data,
                  "cookies" -> request.cookies.toSeq
                    .map { cookie =>
                      cookie.name -> cookie.value
                    }
                    .toMap[String, String]
                ))
            }
        }
      )
      .overrides(
        bind[SessionTimeoutFilter].to[StaticDateSessionTimeoutFilter],
        bind[HttpFilters].to[Filters]
      )
  }

  "SessionTimeoutFilter" should {

    val timestamp = now.minusMinutes(5).getMillis.toString

    val config = SessionTimeoutFilterConfig(
      timeoutDuration       = Duration.standardMinutes(1),
      additionalSessionKeys = Set("whitelisted")
    )

    def app(config: SessionTimeoutFilterConfig = config): Application = {

      import play.api.inject._

      builder
        .overrides(
          bind[SessionTimeoutFilterConfig].toInstance(config)
        )
        .build()
    }

    "strip non-whitelist session variables from request if timestamp is old" in {

      running(app()) {

        val Some(result) = route(
          app(),
          FakeRequest(GET, "/test").withSession(
            lastRequestTimestamp -> timestamp,
            authToken            -> "a-token",
            userId               -> "some-userId",
            "whitelisted"        -> "whitelisted"
          ))

        val rhSession = (contentAsJson(result) \ "session").as[Map[String, String]]

        rhSession                                 should onlyContainWhitelistedKeys(Set("whitelisted"))
        rhSession.get(lastRequestTimestamp).value shouldEqual timestamp
        rhSession.get("whitelisted").value        shouldEqual "whitelisted"
      }
    }

    "strip non-whitelist session variables from result if timestamp is old" in {

      running(app()) {

        val Some(result) = route(
          app(),
          FakeRequest(GET, "/test").withSession(
            lastRequestTimestamp -> timestamp,
            loginOrigin          -> "gg",
            authToken            -> "a-token",
            "whitelisted"        -> "whitelisted"
          ))

        val rhSession = (contentAsJson(result) \ "session").as[Map[String, String]]

        rhSession                    should onlyContainWhitelistedKeys(Set("whitelisted"))
        rhSession.get(loginOrigin)   shouldBe Some("gg")
        rhSession.get("whitelisted") shouldBe Some("whitelisted")
      }
    }

    "pass through all session values if timestamp is recent" in {

      val timestamp = now.minusSeconds(5).getMillis.toString

      running(app()) {

        val Some(result) = route(
          app(),
          FakeRequest(GET, "/test").withSession(
            lastRequestTimestamp -> timestamp,
            authToken            -> "a-token",
            "custom"             -> "custom"
          ))

        val rhSession = (contentAsJson(result) \ "session").as[Map[String, String]]

        rhSession               shouldNot onlyContainWhitelistedKeys(Set("whitelisted"))
        rhSession.get("custom") shouldBe Some("custom")

        session(result).get("custom") shouldBe Some("custom")
      }
    }

    "create timestamp if it's missing" in {

      running(app()) {

        val Some(result) = route(
          app(),
          FakeRequest(GET, "/test").withSession(
            authToken -> "a-token",
            token     -> "another-token",
            userId    -> "a-userId",
            "custom"  -> "custom"
          ))

        val rhSession = (contentAsJson(result) \ "session").as[Map[String, String]]

        rhSession.get(authToken).value      shouldEqual "a-token"
        rhSession.get(userId).value         shouldEqual "a-userId"
        rhSession.get(token).value          shouldEqual "another-token"
        rhSession.get("custom").value       shouldEqual "custom"
        rhSession.get(lastRequestTimestamp) shouldBe None

        session(result).get(lastRequestTimestamp) shouldBe Some(now.getMillis.toString)
      }
    }

    "strip only auth-related keys if timestamp is old, and onlyWipeAuthToken == true" in {

      val altConfig    = config.copy(onlyWipeAuthToken = true)
      val oldTimestamp = now.minusMinutes(5).getMillis.toString

      running(app(altConfig)) {

        val Some(result) = route(
          app(altConfig),
          FakeRequest(GET, "/test").withSession(
            lastRequestTimestamp -> oldTimestamp,
            authToken            -> "a-token",
            token                -> "another-token",
            userId               -> "a-userId",
            "custom"             -> "custom",
            "whitelisted"        -> "whitelisted"
          )
        )

        val rhSession = (contentAsJson(result) \ "session").as[Map[String, String]]

        rhSession.get("custom").value shouldEqual "custom"
        rhSession.get(authToken)      shouldNot be(defined)
        rhSession.get(userId)         shouldNot be(defined)
        rhSession.get(token)          shouldNot be(defined)

        session(result).get("custom").value shouldEqual "custom"
        session(result).get(authToken)      shouldNot be(defined)
      }
    }

    "update old timestamp with current time" in {

      running(app()) {
        val Some(result) = route(
          app(),
          FakeRequest(GET, "/test").withSession(
            lastRequestTimestamp -> now.minusDays(1).getMillis.toString
          ))
        session(result).get(lastRequestTimestamp).value shouldEqual now.getMillis.toString
      }
    }

    "update recent timestamp with current time" in {

      running(app()) {
        val Some(result) = route(
          app(),
          FakeRequest(GET, "/test").withSession(
            lastRequestTimestamp -> now.minusSeconds(1).getMillis.toString
          ))
        session(result).get(lastRequestTimestamp).value shouldEqual now.getMillis.toString
      }
    }

    "treat an invalid timestamp as a missing timestamp" in {

      running(app()) {

        val Some(result) = route(
          app(),
          FakeRequest(GET, "/test").withSession(
            lastRequestTimestamp -> "invalid-format",
            authToken            -> "a-token",
            token                -> "another-token",
            userId               -> "a-userId",
            loginOrigin          -> "gg",
            "custom"             -> "custom"
          )
        )

        session(result).get(authToken).value            shouldEqual "a-token"
        session(result).get(userId).value               shouldEqual "a-userId"
        session(result).get(token).value                shouldEqual "another-token"
        session(result).get(loginOrigin).value          shouldEqual "gg"
        session(result).get("custom").value             shouldEqual "custom"
        session(result).get(lastRequestTimestamp).value shouldEqual now.getMillis.toString
      }
    }

    "ensure non-session cookies are passed through to the action untouched" in {

      val timestamp = now.minusMinutes(5).getMillis.toString

      running(app()) {

        val request = FakeRequest(GET, "/test")
          .withCookies(Cookie("aTestName", "aTestValue"))
          .withSession(
            lastRequestTimestamp -> timestamp,
            authToken            -> "a-token",
            userId               -> "some-userId"
          )

        val Some(result) = route(app(), request)
        val rhCookies    = (contentAsJson(result) \ "cookies").as[Map[String, String]]

        rhCookies      should contain("aTestName" -> "aTestValue")
        status(result) shouldEqual OK
      }
    }
  }

  private def onlyContainWhitelistedKeys(additionalSessionKeysToKeep: Set[String] = Set.empty) =
    new Matcher[Map[String, String]] {
      override def apply(data: Map[String, String]): MatchResult =
        MatchResult(
          (data.keySet -- whitelistedSessionKeys -- additionalSessionKeysToKeep).isEmpty,
          s"""Session keys ${data.keySet} did not contain only whitelisted keys: $whitelistedSessionKeys""",
          s"""Session keys ${data.keySet} contained only whitelisted keys: $whitelistedSessionKeys"""
        )
    }
}
