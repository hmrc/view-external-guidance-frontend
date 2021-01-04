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

import mocks.MockAppConfig
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.bootstrap.tools.Stubs.stubMessagesControllerComponents
import base.ViewFns

import scala.concurrent.Future
import scala.collection.JavaConverters._
import mocks._
import models.errors.DatabaseError
import repositories.ProcessContext
import uk.gov.hmrc.http.SessionKeys
import models.ocelot.{Process, ProcessJson}

class AccessibilityStatementControllerSpec extends WordSpec with Matchers with ViewFns with GuiceOneAppPerSuite {

  private trait Test extends MockGuidanceService with ProcessJson {
    val fakeRequest = FakeRequest("GET", "/").withSession((SessionKeys.sessionId -> "sessionId"))
    lazy val errorHandler = app.injector.instanceOf[config.ErrorHandler]
    private val view = app.injector.instanceOf[views.html.accessibility_statement]
    val process: Process = validOnePageJson.as[Process]
    val controller = new AccessibilityStatementController(MockAppConfig, stubMessagesControllerComponents(), view, mockGuidanceService, errorHandler)
  }

  "GET /accessibility" should {
    "return 200" in new Test {
      MockGuidanceService.getProcessContext(
        "sessionId",
        "accessibility",
        previousPageByLink = false).returns(Future.successful(Right(ProcessContext(process, Map(), Map(), Map(), None))))
      val result: Future[Result] = controller.getPage(None)(fakeRequest)
      status(result) shouldBe Status.OK
    }

    "return HTML" in new Test {
      MockGuidanceService.getProcessContext(
        "sessionId",
        "accessibility",
        previousPageByLink = false).returns(Future.successful(Right(ProcessContext(process, Map(), Map(), Map(), None))))
      val result: Future[Result] = controller.getPage(None)(fakeRequest)
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")

    }

    "return InternalServerError when a database error occurs" in new Test {
      MockGuidanceService.getProcessContext(
        "sessionId",
        "accessibility",
        previousPageByLink = false).returns(Future.successful(Left(DatabaseError)))
      val result: Future[Result] = controller.getPage(None)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }


  }

  "Get /accessibility?p=1" should {

    "return 200" in new Test {

      // Note this an unlikely scenario as it is not usual for the guidance to contain
      // links to the accessibility page

      override val fakeRequest = FakeRequest("GET", "/accessibility?p=1").withSession(SessionKeys.sessionId -> "sessionId")

      MockGuidanceService.getProcessContext(
        "sessionId",
        "accessibility",
        previousPageByLink = true).returns(Future.successful(Right(ProcessContext(process, Map(), Map(), Map(), None))))

      val result: Future[Result] = controller.getPage(Some("1"))(fakeRequest)

      status(result) shouldBe Status.OK
    }

  }

  "GET /accessibility" should {
    "generate page with header link url pointing to nominated url" in new Test {
      MockGuidanceService.getProcessContext(
        "sessionId",
        "accessibility",
        previousPageByLink = false).returns(Future.successful(Right(ProcessContext(process, Map(), Map(), Map(), None))))
      val result: Future[Result] = controller.getPage(None)(fakeRequest)
      status(result) shouldBe Status.OK
      val doc = asDocument(contentAsString(result))
      val processCode = process.meta.processCode
      doc.getElementsByTag("a").asScala.toList.find(elementAttrs(_)("class") == "govuk-header__link govuk-header__link--service-name")
          .fold(fail("Missing header link"))(elementAttrs(_)("href") shouldBe s"${MockAppConfig.baseUrl}/${processCode}/feeling-bad")

    }

    "generate page with header using title of Guidance" in new Test {
      MockGuidanceService.getProcessContext(
        "sessionId",
        "accessibility",
        previousPageByLink = false).returns(Future.successful(Right(ProcessContext(process, Map(), Map(), Map(), None))))
      val result: Future[Result] = controller.getPage(None)(fakeRequest)
      status(result) shouldBe Status.OK
      val doc = asDocument(contentAsString(result))
      doc.getElementsByTag("a").asScala.toList.find(elementAttrs(_)("class") == "govuk-header__link govuk-header__link--service-name")
          .fold(fail("Missing header link"))(a => a.text shouldBe process.title.langs(0))

    }

  }

}
