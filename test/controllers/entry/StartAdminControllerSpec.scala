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

package controllers.entry

import base.BaseSpec
import core.models.errors._
import core.models.ocelot.stanzas._
import core.models.ocelot._
import mocks.{MockDebugService, MockRetrieveAndCacheService}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import views.html._

import scala.concurrent.Future

class StartAdminControllerSpec extends BaseSpec with GuiceOneAppPerSuite {

  trait TestData {
    lazy val uuid = "683d9aa0-2a0e-4e28-9ac8-65ce453d2730"
    lazy val sessionId = s"session-$uuid"
    lazy val path = "/some-path"
    lazy val pageViewBaseUrl = "/guidance"
    lazy val relativePath = path.drop(1)
    lazy val expectedUrl = "/start-url"
    lazy val processId = "ext90002"
    lazy val processCode = "process-code"
    val meta = Meta(processId, "", None, None, 0, "", 1L, 0, None, None, processCode)
    val emptyProcess = Process(meta, Map(), Vector(), Vector())

    val standardPagePath = "/std-page"
    val relativeStdPath = standardPagePath.drop(1)

    lazy val errorHandler = app.injector.instanceOf[config.ErrorHandler]
    lazy val view = app.injector.instanceOf[admin.process_structure]

    val seq = Sequence(
      Phrase("Select a working day of the week", "Welsh: Select a working day of the week"),
      Seq("end", "end"),
      Seq(
        Phrase("Monday", "Welsh: Monday"),
        Phrase("Tuesday", "Welsh: Tuesday")
      ),
      None,
      None,
      stack = false
    )

    val titlePageStanzas = List(
      KeyedStanza("start",PageStanza("/bulletPoints",Vector("1"),false)),
      KeyedStanza("1",TitleCallout(Phrase("Bullet Point List","Welsh: Bullet Point List"),Vector("2"),false)),
      KeyedStanza("2",Instruction(Phrase("Test bulletpoint list here 1st testing here","Welsh Test bulletpoint list here 1st testing here"),Vector("3"),None,true,List(),List())),
      KeyedStanza("3",Instruction(Phrase("Test bulletpoint list here 2nd testing here","Welsh Test bulletpoint list here 2nd testing here"),Vector("4"),None,true,List(),List())),
      KeyedStanza("4",Instruction(Phrase("Test bulletpoint list here 3rd testing here","Welsh Test bulletpoint list here 3rd testing here"),Vector("end"),None,true,List(),List())),
      KeyedStanza("end",EndStanza)
    )
    val titlePage: Page = Page("start","/bulletPoints", titlePageStanzas,List(),true)

    val questionPage: Page = Page("start","/bulletPoints",
                      List(
                        KeyedStanza("start",PageStanza("/bulletPoints",Vector("1"),false)),
                        KeyedStanza("1",Instruction(Phrase("Test bulletpoint list here 1st testing here","Welsh Test bulletpoint list here 1st testing here"),Vector("2"),None,true,List(),List())),
                        KeyedStanza("2",Instruction(Phrase("Test bulletpoint list here 1st testing here","Welsh Test bulletpoint list here 1st testing here"),Vector("3"),None,true,List(),List())),
                        KeyedStanza("3",Instruction(Phrase("Test bulletpoint list here 2nd testing here","Welsh Test bulletpoint list here 2nd testing here"),Vector("4"),None,true,List(),List())),
                        KeyedStanza("4",Question(Phrase("Which?","Which?"), Seq(Phrase("yes","yes"),Phrase("no","no")),Vector("end"),None,true)),
                        KeyedStanza("end",EndStanza)
                      ),List(),true)

    val sequencePage: Page = Page("start","/bulletPoints",
                      List(
                        KeyedStanza("start",PageStanza("/bulletPoints",Vector("1"),false)),
                        KeyedStanza("1",Instruction(Phrase("Test bulletpoint list here 1st testing here","Welsh Test bulletpoint list here 1st testing here"),Vector("2"),None,true,List(),List())),
                        KeyedStanza("2",Instruction(Phrase("Test bulletpoint list here 1st testing here","Welsh Test bulletpoint list here 1st testing here"),Vector("3"),None,true,List(),List())),
                        KeyedStanza("3",Instruction(Phrase("Test bulletpoint list here 2nd testing here","Welsh Test bulletpoint list here 2nd testing here"),Vector("4"),None,true,List(),List())),
                        KeyedStanza("4",seq),
                        KeyedStanza("end",EndStanza)
                      ),List(),true)

    val inputPage: Page = Page("start","/bulletPoints",
                      List(
                        KeyedStanza("start",PageStanza("/bulletPoints",Vector("1"),false)),
                        KeyedStanza("1",Instruction(Phrase("Test bulletpoint list here 1st testing here","Welsh Test bulletpoint list here 1st testing here"),Vector("2"),None,true,List(),List())),
                        KeyedStanza("2",Instruction(Phrase("Test bulletpoint list here 1st testing here","Welsh Test bulletpoint list here 1st testing here"),Vector("3"),None,true,List(),List())),
                        KeyedStanza("3",Instruction(Phrase("Test bulletpoint list here 2nd testing here","Welsh Test bulletpoint list here 2nd testing here"),Vector("4"),None,true,List(),List())),
                        KeyedStanza("4",CurrencyInput(Seq("end"),Phrase("Input",""), None, "PRICE", None, false)),
                        KeyedStanza("end",EndStanza)
                      ),List(),true)

    val yourCallPage: Page = Page("start","/bulletPoints",
                      List(
                        KeyedStanza("start",PageStanza("/bulletPoints",Vector("1"),false)),
                        KeyedStanza("1",YourCallCallout(Phrase("vtst Bullet Point List","vtst Welsh: Bullet Point List"),Vector("2"),false)),
                        KeyedStanza("2",Instruction(Phrase("Test bulletpoint list here 1st testing here","Welsh Test bulletpoint list here 1st testing here"),Vector("3"),None,true,List(),List())),
                        KeyedStanza("3",Instruction(Phrase("Test bulletpoint list here 2nd testing here","Welsh Test bulletpoint list here 2nd testing here"),Vector("4"),None,true,List(),List())),
                        KeyedStanza("4",Instruction(Phrase("Test bulletpoint list here 3rd testing here","Welsh Test bulletpoint list here 3rd testing here"),Vector("end"),None,true,List(),List())),
                        KeyedStanza("end",EndStanza)
                      ),List(),true)

  }

  trait ProcessTest extends MockRetrieveAndCacheService with MockDebugService with TestData {
    lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")

    lazy val target =
      new StartAdminController(
        errorHandler,
        mockRetrieveAndCacheService,
        mockDebugService,
        view,
        stubMessagesControllerComponents()
      )
  }

  "Calling the map-published endpoint with a valid process code" should {

    "redirect the caller to another page" in new ProcessTest {
      val pageMap = Map("/bulletPoints" -> models.PageNext("start",List(),List(),Some("Bullet Point List"),None))
      val processPageStructure: models.admin.ProcessPageStructure =  models.admin.ProcessPageStructure(
        "start",
        "/bulletPoints", 
        Some("Bullet Point List"),
        titlePageStanzas,
        Seq(models.admin.LinkedPage("4", "/blah", Some("Blah blah")), models.admin.LinkedPage("5", "/Otherblah", Some("Other Blah blah"))),
        Seq(),
        Seq("start")
      )

      MockDebugService.pageTitle(titlePage).returns(Some("Bullet Point List"))
      MockDebugService.mapPage(titlePage, pageMap).returns(processPageStructure)
      MockRetrieveAndCacheService
        .retrieveOnlyPublished(processCode)
        .returns(Future.successful(Right((emptyProcess, Seq(titlePage)))))

      val result = target.publishedPageMap(processCode)(fakeRequest)
      status(result) shouldBe Status.OK
    }

  }

  "Calling map-published endpoint with a invalid process code" should {

    "return a NOT_FOUND error" in new ProcessTest {
      val unknownProcessId = "ext90077"
      MockRetrieveAndCacheService
        .retrieveOnlyPublished(unknownProcessId)
        .returns(Future.successful(Left(NotFoundError)))
      val result = target.publishedPageMap(unknownProcessId)(fakeRequest)
      status(result) shouldBe Status.NOT_FOUND
    }

  }

  "Calling map-published endpoint with a valid process code to an invalid process" should {

    "return a InternalServerRrror error" in new ProcessTest {
      MockRetrieveAndCacheService
        .retrieveOnlyPublished(processCode)
        .returns(Future.successful(Left(InvalidProcessError)))
      val result = target.publishedPageMap(processCode)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

  }

  "Calling the map-approval endpoint with a valid process code" should {

    "redirect the caller to another page" in new ProcessTest {
      MockRetrieveAndCacheService
        .retrieveOnlyApproval(processCode)
        .returns(Future.successful(Right((emptyProcess, Seq()))))

      val result = target.approvalPageMap(processCode)(fakeRequest)
      status(result) shouldBe Status.OK
    }

  }

  "Calling map-approval endpoint with a invalid process code" should {

    "return a NOT_FOUND error" in new ProcessTest {
      val unknownProcessId = "ext90077"
      MockRetrieveAndCacheService
        .retrieveOnlyApproval(unknownProcessId)
        .returns(Future.successful(Left(NotFoundError)))
      val result = target.approvalPageMap(unknownProcessId)(fakeRequest)
      status(result) shouldBe Status.NOT_FOUND
    }

  }

  "Calling map-approval endpoint with a valid process code to an invalid process" should {

    "return a InternalServerRrror error" in new ProcessTest {
      MockRetrieveAndCacheService
        .retrieveOnlyApproval(processCode)
        .returns(Future.successful(Left(InvalidProcessError)))
      val result = target.approvalPageMap(processCode)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

  }

}
