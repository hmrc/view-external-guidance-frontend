/*
 * Copyright 2021 HM Revenue & Customs
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

package controllers

import java.time.Instant

import base.BaseSpec
import mocks.{MockAppConfig, MockGuidanceService}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.test.FakeRequest
import play.api.test.Helpers._
import config.ErrorHandler
import core.models.errors._
import core.models.ocelot.{Process, ProcessJson}
import models.GuidanceSession
import views.html.{delete_your_answers, session_timeout}
import play.api.test.Helpers.stubMessagesControllerComponents
import uk.gov.hmrc.http.SessionKeys
import play.api.mvc.{AnyContentAsEmpty, Result}

import scala.concurrent.Future

class SessionTimeoutPageControllerSpec extends BaseSpec with GuiceOneAppPerSuite {

  private trait Test extends MockGuidanceService with ProcessJson {

    lazy val errorHandler: ErrorHandler = app.injector.instanceOf[ErrorHandler]
    lazy val view: session_timeout = app.injector.instanceOf[session_timeout]
    lazy val delete_answers_view: delete_your_answers = app.injector.instanceOf[delete_your_answers]

    lazy val processCode = "cup-of-tea"
    lazy val sessionId = "sessionId"
    lazy val invalidProcessCode = "cup-of-coffee"

    lazy val process: Process = validOnePageJson.as[Process]
    lazy val session: GuidanceSession = GuidanceSession(process, Map(), Map(), Nil, Map(), Map(), Nil, None, None)

    val timeout: Int = MockAppConfig.timeoutInSeconds * MockAppConfig.toMilliSeconds

    val target = new SessionTimeoutPageController( MockAppConfig,
      mockGuidanceService,
      errorHandler,
      stubMessagesControllerComponents(),
      view,
      delete_answers_view)

  }

  "SessionTimeoutPageController method getPage invoked by delete your answers link" should {

    "delete the current session data if it exists" in new Test {

      val now: String = Instant.now.toEpochMilli.toString

      val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> now)

      MockGuidanceService.getCurrentGuidanceSession(processCode)(sessionId).returns(Future.successful(Right(session)))

      val result: Future[Result] = target.getPage(processCode)(fakeRequest)

      status(result) shouldBe Status.OK
    }

    "return an OK if the process cannot be found when retrieving the session data" in new Test {

      val now: String = Instant.now.toEpochMilli.toString

      val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> now)

      MockGuidanceService.getCurrentGuidanceSession(processCode)(sessionId).returns(Future.successful(Left(NotFoundError)))

      val result: Future[Result] = target.getPage(processCode)(fakeRequest)

      status(result) shouldBe Status.OK
    }


    "return an internal server error if a database error occurs retrieving the session data" in new Test {

      val now: String = Instant.now.toEpochMilli.toString

      val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> now)

      MockGuidanceService.getCurrentGuidanceSession(processCode)(sessionId).returns(Future.successful(Left(DatabaseError)))

      val result: Future[Result] = target.getPage(processCode)(fakeRequest)

      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return OK if the process code in the session data does not match value of input argument" in new Test {

      val now: String = Instant.now.toEpochMilli.toString

      val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> now)

      MockGuidanceService.getCurrentGuidanceSession(invalidProcessCode)(sessionId).returns(Future.successful(Right(session)))

      val result: Future[Result] = target.getPage(invalidProcessCode)(fakeRequest)

      status(result) shouldBe Status.OK
    }

  }

  "SessionTimeoutPageController method getPage invoked after timeout dialog expires" should {

    "return a successful response if the session has expired" in new Test {

      val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")

      val result: Future[Result] = target.getPage(processCode)(fakeRequest)

      status(result) shouldBe Status.OK
    }

    "return successful response when session remains but session timeout exceeded by 60 seconds " in new Test {

      // This scenario should not occur but is catered for as a possible extension of the timeout expiry calculation
      val now: Long = Instant.now.toEpochMilli
      val ts: Long = now - (timeout + (60 * MockAppConfig.toMilliSeconds))

      val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> ts.toString
      )

      val result: Future[Result] = target.getPage(processCode)(fakeRequest)

      status(result) shouldBe Status.OK
    }

    "return successful response when method is triggered just before session should have timed out" in new Test {

      // This scenario occurs when the timeout dialog disappears fractionally before the session times out
      val now: Long = Instant.now.toEpochMilli
      val ts: Long = now - (timeout - 10)

      val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> ts.toString
      )

      val result: Future[Result] = target.getPage(processCode)(fakeRequest)

      status(result) shouldBe Status.OK
    }
  }

}
