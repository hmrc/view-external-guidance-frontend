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
import uk.gov.hmrc.http.SessionKeys
import uk.gov.hmrc.play.bootstrap.filters.frontend.SessionTimeoutFilterConfig

import scala.concurrent.ExecutionContext

object SessionTimeoutFilterSessionIdSpec {

  class Filters @Inject()(timeoutFilter: SessionTimeoutFilter) extends DefaultHttpFilters(timeoutFilter)

  class StaticDateSessionTimeoutFilter @Inject()(
    config: SessionTimeoutFilterConfig
  )(
    implicit
    ec: ExecutionContext,
    mat: Materializer)
      extends SessionTimeoutFilter(config)(ec, mat) {

    override val clock: DateTime = DateTime.now
  }
}

class SessionTimeoutFilterSessionIdSpec
    extends WordSpecLike
    with Matchers
    with ScalaFutures
    with OptionValues {

  import SessionTimeoutFilterSessionIdSpec._

  val sessionId = s"session-${java.util.UUID.randomUUID().toString}"
  val staleSessionId = s"session-${java.util.UUID.randomUUID().toString}"
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
                )).withSession(SessionKeys.sessionId-> sessionId)
            }
        }
      )
      .overrides(
        bind[SessionTimeoutFilter].to[StaticDateSessionTimeoutFilter],
        bind[HttpFilters].to[Filters]
      )
  }

  "SessionTimeoutFilter" should {

    val timestamp = DateTime.now.minusMinutes(16).getMillis.toString
    val recentTimestamp = DateTime.now.minusSeconds(10).getMillis().toString

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

    "not delete a sessionId created within the action after timeout has taken place" in {
      
      running(app()) {

        val Some(result) = route(
          app(),
          FakeRequest(GET, "/test").withSession(
            SessionKeys.lastRequestTimestamp -> timestamp,
            SessionKeys.authToken            -> "a-token",
            SessionKeys.userId               -> "some-userId",
            SessionKeys.sessionId            -> staleSessionId
          ))

        val rhSession = (contentAsJson(result) \ "session").as[Map[String, String]]
        rhSession.get(SessionKeys.sessionId) shouldEqual None
        rhSession.get(SessionKeys.lastRequestTimestamp).value shouldEqual timestamp
        session(result).get(SessionKeys.sessionId).value        shouldEqual sessionId
      }
    }

    "not delete a sessionId which exists prior to the request when no timeout has taken place" in {
      
      running(app()) {

        val Some(result) = route(
          app(),
          FakeRequest(GET, "/test").withSession(
            SessionKeys.lastRequestTimestamp -> recentTimestamp,
            SessionKeys.authToken            -> "a-token",
            SessionKeys.userId               -> "some-userId",
            SessionKeys.sessionId            -> sessionId
          ))

        val rhSession = (contentAsJson(result) \ "session").as[Map[String, String]]
        rhSession.get(SessionKeys.lastRequestTimestamp).value shouldEqual recentTimestamp
        session(result).get(SessionKeys.sessionId).value        shouldEqual sessionId
      }
    }

  }
}
