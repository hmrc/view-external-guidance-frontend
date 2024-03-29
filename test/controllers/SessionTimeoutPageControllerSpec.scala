/*
 * Copyright 2023 HM Revenue & Customs
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

import base.BaseSpec
import config.ErrorHandler
import core.models.errors._
import core.models.ocelot.{Process, ProcessJson, Published}
import mocks.{MockAppConfig, MockGuidanceService}
import models.GuidanceSession
import play.api.http.Status
import play.api.mvc.{AnyContentAsEmpty, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.SessionKeys
import views.html.{system_timedout_session, user_deleted_session}

import java.time.Instant
import scala.concurrent.Future

class SessionTimeoutPageControllerSpec extends BaseSpec {

  private trait Test extends MockGuidanceService with ProcessJson {

    lazy val errorHandler: ErrorHandler = app.injector.instanceOf[ErrorHandler]
    lazy val view: system_timedout_session = app.injector.instanceOf[system_timedout_session]
    lazy val delete_answers_view: user_deleted_session = app.injector.instanceOf[user_deleted_session]

    lazy val processCode = "cup-of-tea"
    lazy val sessionId = "sessionId"
    lazy val invalidProcessCode = "cup-of-coffee"

    lazy val process: Process = validOnePageJson.as[Process]
    lazy val session: GuidanceSession = GuidanceSession(process, Map(), Map(), Nil, Map(), Map(), Nil, None, None, Published, Nil)

    val timeout: Int = MockAppConfig.timeoutInSeconds * 1000

    val target = new SessionTimeoutPageController( MockAppConfig,
      mockGuidanceService,
      errorHandler,
      stubMessagesControllerComponents(),
      view,
      delete_answers_view)

  }

  "SessionTimeoutPageController method sessionTimeout invoked by delete your answers link" should {

    "delete the current session data if it exists" in new Test {

      val now: String = Instant.now.toEpochMilli.toString

      val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> now)

      MockGuidanceService.getCurrentGuidanceSession(processCode)(sessionId).returns(Future.successful(Right(session)))
      MockGuidanceService.deleteSession(processCode, sessionId).returns(Future.successful(Right(())))

      val result: Future[Result] = target.sessionTimeout(processCode)(fakeRequest)

      status(result) shouldBe Status.OK
    }

    "return an OK if the process cannot be found when retrieving the session data" in new Test {

      val now: String = Instant.now.toEpochMilli.toString

      val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> now)

      MockGuidanceService.getCurrentGuidanceSession(processCode)(sessionId).returns(Future.successful(Left((SessionNotFoundError, None))))

      val result: Future[Result] = target.sessionTimeout(processCode)(fakeRequest)

      status(result) shouldBe Status.OK
    }


    "return an internal server error if a database error occurs deleting the session data" in new Test {

      val now: String = Instant.now.toEpochMilli.toString

      val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> now)

      MockGuidanceService.getCurrentGuidanceSession(processCode)(sessionId).returns(Future.successful(Left((DatabaseError, None))))

      val result: Future[Result] = target.sessionTimeout(processCode)(fakeRequest)

      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

  }

  "SessionTimeoutPageController method endSession invoked after timeout dialog expires" should {

    "return a successful response if the session has expired" in new Test {

      val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")

      val result: Future[Result] = target.endSession(processCode)(fakeRequest)
      status(result) shouldBe Status.OK
    }

    "return successful response when session remains but session timeout exceeded by 60 seconds " in new Test {
      // This scenario should not occur but is catered for as a possible extension of the timeout expiry calculation
      val now: Long = Instant.now.toEpochMilli
      val ts: Long = now - (timeout + (60 * 1000))
      val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> ts.toString
      )

      MockGuidanceService.getCurrentGuidanceSession(processCode)(sessionId).returns(Future.successful(Right(session)))
      MockGuidanceService.deleteSession(processCode, sessionId).returns(Future.successful(Right(())))
      val result: Future[Result] = target.endSession(processCode)(fakeRequest)

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

      MockGuidanceService.getCurrentGuidanceSession(processCode)(sessionId).returns(Future.successful(Right(session)))
      MockGuidanceService.deleteSession(processCode, sessionId).returns(Future.successful(Right(())))
      val result: Future[Result] = target.endSession(processCode)(fakeRequest)

      status(result) shouldBe Status.OK
    }
  }


  "sessionStillActive" should {
    val now = 1657796066757L
    val lastRequestTime = 1657796057930L

    "detect session has not expired" in new Test {
      val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> lastRequestTime.toString
      )

      sessionStillActive(fakeRequest, MockAppConfig, now) shouldBe true
    }

    "detect session is acceptably close to expiry" in new Test {
      val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> lastRequestTime.toString
      )
      val currentTime = lastRequestTime + MockAppConfig.timeoutInSeconds * 1000 - 50

      sessionStillActive(fakeRequest, MockAppConfig, currentTime) shouldBe false
    }

    "detect session has expired" in new Test {
      val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> lastRequestTime.toString
      )
      val currentTime = lastRequestTime + MockAppConfig.timeoutInSeconds * 1000L + 20000L

      sessionStillActive(fakeRequest, MockAppConfig, currentTime) shouldBe false
    }

    "Return true with lastRequest times in the future" in new Test {
      val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> (now + 20000L).toString
      )

      sessionStillActive(fakeRequest, MockAppConfig, now) shouldBe true
    }

  }

}
