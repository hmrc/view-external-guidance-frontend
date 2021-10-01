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

import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.test._
import play.api._
import play.api.test.Helpers.stubMessagesControllerComponents
import uk.gov.hmrc.http.HeaderNames
import mocks.MockAppConfig
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import play.api.mvc._
import config.AppConfig
import play.api.Logger
import scala.concurrent.Future
import core.models.RequestOutcome


class SessionFrontendControllerSpec extends WordSpec with Matchers with GuiceOneAppPerSuite {
  val requestId = "1234567890"
  val sessionId = "0987654321"
  val env = Environment.simple()
  val configuration = Configuration.load(env)
  val appConfig = MockAppConfig
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  class TestController(appConfig: AppConfig, cc: MessagesControllerComponents) extends FrontendController(cc) with SessionFrontendController{
    val logger = Logger(getClass)

    def test(sId: String, rId: Option[String])(implicit request: Request[_]): Future[RequestOutcome[Unit]] = {
        def check(s: String, r: Option[String]): Future[RequestOutcome[Unit]] = {
          s shouldBe sId
          r shouldBe rId

          Future.successful(Right(()))
        }

        withExistingSession( check )
    }
  }


  "withExistingSession" should {
    val controller = new TestController(appConfig, stubMessagesControllerComponents())

    "Provide sessionId and requestId when supplied" in {

      val fakeRequest = FakeRequest("GET", "/").withHeaders((HeaderNames.xRequestId, requestId), (HeaderNames.xSessionId, sessionId))

      controller.test(sessionId, Some(requestId))(fakeRequest).map{
        case Right(()) => succeed
        case _ => fail
      }

    }

    "Provide sessionId and None when no requestId available" in {

      val fakeRequest = FakeRequest("GET", "/").withHeaders((HeaderNames.xSessionId, sessionId))

      controller.test(sessionId, None)(fakeRequest).map{
        case Right(()) => succeed
        case _ => fail
      }

    }

    "Fail when no sessionId is available" in {

      val fakeRequest = FakeRequest("GET", "/")

      controller.test(sessionId, None)(fakeRequest).map{
        case Left(err) if err.code == EXPECTATION_FAILED => succeed
        case _ => fail
      }

    }
  }

}
