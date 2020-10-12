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

package controllers.entry

import base.BaseSpec
import mocks.MockGuidanceService
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc._
import play.api.mvc.{BodyParsers,AnyContentAsEmpty}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.bootstrap.tools.Stubs.stubMessagesControllerComponents
import models.ui._
import models.errors._
import scala.concurrent.{ExecutionContext, Future}
import controllers.actions.SessionIdAction

class StartGuidanceControllerSpec extends BaseSpec with GuiceOneAppPerSuite {

  trait TestData {
    lazy val uuid = "683d9aa0-2a0e-4e28-9ac8-65ce453d2730"
    lazy val path = "/some-path"
    lazy val pageViewBaseUrl = "/guidance"
    lazy val relativePath = path.drop(1)
    lazy val expectedUrl = "/start-url"
    lazy val processId = "ext90002"

    lazy val ans1 = Answer(Text("ANS1", "ANS1"), Some(Text("", "")))
    lazy val ans2 = Answer(Text("ANS2", "ANS2"), Some(Text("", "")))

    lazy val expectedPage: Page = QuestionPage(
      path,
      Question(Text("QUESTION", "QUESTION"), None, Seq(Paragraph(Text("QUESTION", "QUESTION"))), Seq(ans1, ans2))
    )

    val standardPagePath = "/std-page"
    val relativeStdPath = standardPagePath.drop(1)

    val standardPage: Page = Page(
      standardPagePath,
      Seq(H1(Text("hello", "Welsh: hello")))
    )

    val fakeSessionIdAction = new SessionIdAction {
      def parser: BodyParsers.Default = app.injector.instanceOf[BodyParsers.Default]
      implicit protected def executionContext: ExecutionContext = ExecutionContext.global
      override def invokeBlock[A](request: Request[A], block: Request[A] => Future[Result]): Future[Result] = block(request)
    }

    lazy val errorHandler = app.injector.instanceOf[config.ErrorHandler]
    lazy val view = app.injector.instanceOf[views.html.standard_page]
    lazy val questionView = app.injector.instanceOf[views.html.question_page]

  }


  trait ProcessTest extends MockGuidanceService with TestData {
    lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")

    lazy val target =
      new StartGuidanceController(
        errorHandler,
        mockGuidanceService,
        fakeSessionIdAction,
        stubMessagesControllerComponents()
      )

  }

  "Calling the scratch process endpoint with a valid UUID" should {

    trait ScratchTestWithValidUUID extends ProcessTest {
      val repositoryId = "683d9aa0-2a0e-4e28-9ac8-65ce453d2731"
      MockGuidanceService
        .retrieveAndCacheScratch(uuid, repositoryId)
        .returns(Future.successful(Right((expectedUrl,uuid))))
      lazy val result = target.scratch(uuid)(fakeRequest)
    }

    "redirect the caller to another page" in new ScratchTestWithValidUUID {
      status(result) shouldBe Status.SEE_OTHER
    }

    "redirect the caller to the start page of the process" in new ScratchTestWithValidUUID {
      redirectLocation(result) shouldBe Some(s"$pageViewBaseUrl/$uuid$expectedUrl")
    }

  }

  "Calling the scratch process endpoint with an invalid UUID" should {

    "return a NOT_FOUND error" in new ProcessTest {
      MockGuidanceService
        .retrieveAndCacheScratch(uuid, uuid)
        .returns(Future.successful(Left(NotFoundError)))
      val result = target.scratch(uuid)(fakeRequest)
      status(result) shouldBe Status.NOT_FOUND
    }

  }

  "Calling the scratch process endpoint and a database error occurs" should {

    "return an INTERNAL_SERVER_ERROR error" in new ProcessTest {
      MockGuidanceService
        .retrieveAndCacheScratch(uuid, uuid)
        .returns(Future.successful(Left(DatabaseError)))

      val result = target.scratch(uuid)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

  }

  "Calling the published endpoint with a valid process ID" should {

    "redirect the caller to another page" in new ProcessTest {
      MockGuidanceService
        .retrieveAndCachePublished(processId, processId)
        .returns(Future.successful(Right((expectedUrl,processId))))

      val result = target.published(processId)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "redirect the caller to the start page of the process" in new ProcessTest {
      MockGuidanceService
        .retrieveAndCachePublished(processId, processId)
        .returns(Future.successful(Right((expectedUrl,processId))))
      val result = target.published(processId)(fakeRequest)
      redirectLocation(result) shouldBe Some(s"$pageViewBaseUrl/$processId$expectedUrl")
    }

  }

  "Calling published endpoint with a invalid process ID" should {

    "return a NOT_FOUND error" in new ProcessTest {
      val unknownProcessId = "ext90077"
      MockGuidanceService
        .retrieveAndCachePublished(unknownProcessId, unknownProcessId)
        .returns(Future.successful(Left(NotFoundError)))
      val result = target.published(unknownProcessId)(fakeRequest)
      status(result) shouldBe Status.NOT_FOUND
    }

  }

  "Calling the approval endpoint with a valid process ID" should {

    "redirect the caller to another page" in new ProcessTest {
      MockGuidanceService
        .retrieveAndCacheApproval(processId, processId)
        .returns(Future.successful(Right((expectedUrl, processId))))

      val result = target.approval(processId)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "redirect the caller to the start page of the process" in new ProcessTest {
      MockGuidanceService
        .retrieveAndCacheApproval(processId, processId)
        .returns(Future.successful(Right((expectedUrl,processId))))
      val result = target.approval(processId)(fakeRequest)
      redirectLocation(result) shouldBe Some(s"$pageViewBaseUrl/$processId$expectedUrl")
    }
  }

  "Calling approval endpoint with a invalid process ID" should {

    "return a NOT_FOUND error" in new ProcessTest {
      val unknownProcessId = "ext90077"
      MockGuidanceService
        .retrieveAndCacheApproval(unknownProcessId, unknownProcessId)
        .returns(Future.successful(Left(NotFoundError)))
      val result = target.approval(unknownProcessId)(fakeRequest)
      status(result) shouldBe Status.NOT_FOUND
    }

  }

  "Calling the approvalPage endpoint with a valid process ID and Url" should {

    val url = "blah"

    "redirect the caller to another page" in new ProcessTest {
      MockGuidanceService
        .retrieveAndCacheApproval(processId, processId)
        .returns(Future.successful(Right((expectedUrl, processId))))

      val result = target.approvalPage(processId, url)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "redirect the caller to the start page of the process" in new ProcessTest {
      MockGuidanceService
        .retrieveAndCacheApproval(processId, processId)
        .returns(Future.successful(Right((expectedUrl, processId))))
      val result = target.approvalPage(processId, url)(fakeRequest)
      redirectLocation(result) shouldBe Some(s"$pageViewBaseUrl/$processId/$url")
    }

  }

  "Calling approvalPage endpoint with a invalid process ID" should {

    "return a NOT_FOUND error" in new ProcessTest {
      val unknownProcessId = "ext90077"
      MockGuidanceService
        .retrieveAndCacheApproval(unknownProcessId, unknownProcessId)
        .returns(Future.successful(Left(NotFoundError)))
      val result = target.approvalPage(unknownProcessId, "/")(fakeRequest)
      status(result) shouldBe Status.NOT_FOUND
    }

  }

}
