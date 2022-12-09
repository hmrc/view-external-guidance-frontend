/*
 * Copyright 2022 HM Revenue & Customs
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
import mocks.{MockAppConfig, MockRetrieveAndCacheService}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc._
import play.api.mvc.{BodyParsers,AnyContentAsEmpty}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.test.Helpers.stubMessagesControllerComponents
import core.models.errors._
import scala.concurrent.{ExecutionContext, Future}
import controllers.actions.SessionIdAction
import core.models.ocelot.stanzas._
import core.models.ocelot._
import core.models.ocelot.{Meta, Process}

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
    val meta = Meta(processId, "", None, 0, "", 1L, 0, None, None, processCode)
    val emptyProcess = Process(meta, Map(), Vector(), Vector())

    val standardPagePath = "/std-page"
    val relativeStdPath = standardPagePath.drop(1)

    val fakeSessionIdAction = new SessionIdAction {
      def parser: BodyParsers.Default = app.injector.instanceOf[BodyParsers.Default]
      implicit protected def executionContext: ExecutionContext = ExecutionContext.global
      override def invokeBlock[A](request: Request[A], block: Request[A] => Future[Result]): Future[Result] = block(request)
    }

    lazy val errorHandler = app.injector.instanceOf[config.ErrorHandler]
    lazy val view = app.injector.instanceOf[views.html.process_map]

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

    val titlePage: Page = Page("start","/bulletPoints",
                      List(
                        KeyedStanza("start",PageStanza("/bulletPoints",Vector("1"),false)),
                        KeyedStanza("1",TitleCallout(Phrase("vtst Bullet Point List","vtst Welsh: Bullet Point List"),Vector("2"),false)),
                        KeyedStanza("2",Instruction(Phrase("Test bulletpoint list here 1st testing here","Welsh Test bulletpoint list here 1st testing here"),Vector("3"),None,true,List(),List())),
                        KeyedStanza("3",Instruction(Phrase("Test bulletpoint list here 2nd testing here","Welsh Test bulletpoint list here 2nd testing here"),Vector("4"),None,true,List(),List())),
                        KeyedStanza("4",Instruction(Phrase("Test bulletpoint list here 3rd testing here","Welsh Test bulletpoint list here 3rd testing here"),Vector("end"),None,true,List(),List())),
                        KeyedStanza("end",EndStanza)
                      ),List(),true)

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


    // val instruction: Instruction = Instruction(Phrase("Instruction", "Instruction"), Seq("3"), None, false)
    // val questionStanza: Question = Question(Phrase("Which?","Which?"), Seq(Phrase("yes","yes"),Phrase("no","no")), Seq("4","5"), None, false)
    // val currencyInputStanza: CurrencyInput = CurrencyInput(Seq("4"),Phrase("",""), None, "PRICE", None, false)
    // val nonExclusiveSequence: Sequence = Sequence(
    //   Phrase("Select a working day of the week", "Welsh: Select a working day of the week"),
    //   Seq("10", "20", "30", "40", "50", "end"),
    //   Seq(
    //     Phrase("Monday", "Welsh: Monday"),
    //     Phrase("Tuesday", "Welsh: Tuesday"),
    //     Phrase("Wednesday", "Welsh: Wednesday"),
    //     Phrase("Thursday", "Welsh: Thursday"),
    //     Phrase("Friday", "Welsh: Friday")
    //   ),
    //   None,
    //   None,
    //   stack = false
    // )
    // val exclusiveSequence: Sequence = Sequence(
    //   Phrase("Select a holiday destination", "Welsh: Select a holiday destination"),
    //   Seq("10", "20", "30", "40", "50", "60"),
    //   Seq(
    //     Phrase("Europe", "Welsh: Europe"),
    //     Phrase("Africa", "Welsh: Africa"),
    //     Phrase("Americas", "Welsh: Americas"),
    //     Phrase("Asia", "Welsh: Asia"),
    //   ),
    //   Some(Phrase(
    //     "Elsewhere [exclusive][hint:Selecting this checkbox will deselect the other checkboxes]",
    //     "Welsh: Elsewhere [exclusive][hint:Welsh: Selecting this checkbox will deselect the other checkboxes]"
    //   )),
    //   None,
    //   stack = false
    // )
    // val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
    //                                     KeyedStanza("1", instruction),
    //                                     KeyedStanza("3", questionStanza)
    //                                   )
    // val stanzasWithInput: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
    //                                     KeyedStanza("1", instruction),
    //                                     KeyedStanza("3", currencyInputStanza)
    //                                   )

    // val stanzasWithNonExclusiveSequence: Seq[KeyedStanza] = Seq(
    //   KeyedStanza("start", PageStanza("/start", Seq("1"), stack = false)),
    //   KeyedStanza("1", instruction),
    //   KeyedStanza("3", nonExclusiveSequence),
    //   KeyedStanza("end", EndStanza)
    // )

    // val page = Page("start", "/test-page", stanzas, Seq("4","5"))
    // val inputPage = Page("start", "/test-page", stanzasWithInput, Seq("4"))
    // val nonQuestionPage = Page("start", "/test-page", stanzas.drop(1), Seq("3"))
    // val nonExclusiveSequenceInputPage: Page = Page("start", "/test-page", stanzasWithNonExclusiveSequence, Seq("end"))
  }

  trait ProcessTest extends MockRetrieveAndCacheService with TestData {
    lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")

    lazy val target =
      new StartAdminController(
        errorHandler,
        mockRetrieveAndCacheService,
        view,
        fakeSessionIdAction,
        stubMessagesControllerComponents(),
        MockAppConfig
      )
  }

  "Calling the map-published endpoint with a valid process code" should {

    "redirect the caller to another page" in new ProcessTest {
      MockRetrieveAndCacheService
        .retrieveOnlyPublished(processCode)
        .returns(Future.successful(Right((emptyProcess, Seq()))))

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

  "buildPageRows" should {
    "Return sequence of ProcessMapRows" in new ProcessTest {
      val pageMap: Map[String, Page] = Seq(titlePage).map(p => (p.id, p)).toMap


      val rows = target.buildPageRows(Some(titlePage), pageMap)
      rows.length shouldBe 1
      rows.head.typ shouldBe models.PageEntry
      rows.head.id shouldBe "start"
      rows.head.url shouldBe "/bulletPoints"
      rows.head.title shouldBe Some("vtst Bullet Point List")
    }
  }

  "pageTitle" should {
    "Return title of page" in new ProcessTest {
      target.pageTitle(titlePage) shouldBe Some("vtst Bullet Point List")
      target.pageTitle(questionPage) shouldBe Some("Which?")
      target.pageTitle(sequencePage) shouldBe Some("Select a working day of the week")
      target.pageTitle(inputPage) shouldBe Some("Input")
      target.pageTitle(yourCallPage) shouldBe Some("vtst Bullet Point List")
    }
  }

  // "Calling the approval endpoint with a valid process ID" should {

  //   "redirect the caller to another page" in new ProcessTest {
  //     MockRetrieveAndCacheService
  //       .retrieveAndCacheApproval(processId, processId)
  //       .returns(Future.successful(Right((expectedUrl, processId))))

  //     val result = target.approval(processId)(fakeRequest)
  //     status(result) shouldBe Status.SEE_OTHER
  //   }

  //   "redirect the caller to the start page of the process" in new ProcessTest {
  //     MockRetrieveAndCacheService
  //       .retrieveAndCacheApproval(processId, processId)
  //       .returns(Future.successful(Right((expectedUrl,processId))))
  //     val result = target.approval(processId)(fakeRequest)
  //     redirectLocation(result) shouldBe Some(s"$pageViewBaseUrl/$processId$expectedUrl")
  //   }
  // }

  // "Calling approval endpoint with a invalid process ID" should {

  //   "return a NOT_FOUND error" in new ProcessTest {
  //     val unknownProcessId = "ext90077"
  //     MockRetrieveAndCacheService
  //       .retrieveAndCacheApproval(unknownProcessId, unknownProcessId)
  //       .returns(Future.successful(Left(NotFoundError)))
  //     val result = target.approval(unknownProcessId)(fakeRequest)
  //     status(result) shouldBe Status.NOT_FOUND
  //   }

  // }

  // "Calling the approvalPage endpoint with a valid process ID and Url" should {

  //   val url = "blah"

  //   "redirect the caller to another page" in new ProcessTest {
  //     MockRetrieveAndCacheService
  //       .retrieveAndCacheApprovalByPageUrl(s"/$url")(processId, processId)
  //       .returns(Future.successful(Right((s"/$url", processId))))

  //     val result = target.approvalPage(processId, url)(fakeRequest)
  //     status(result) shouldBe Status.SEE_OTHER
  //   }

  //   "redirect the caller to the start page of the process" in new ProcessTest {
  //     MockRetrieveAndCacheService
  //       .retrieveAndCacheApprovalByPageUrl(s"/$url")(processId, processId)
  //       .returns(Future.successful(Right((s"/$url", processId))))
  //     val result = target.approvalPage(processId, url)(fakeRequest)
  //     redirectLocation(result) shouldBe Some(s"$pageViewBaseUrl/$processId/$url")
  //   }

  // }

  // "Calling approvalPage endpoint with a invalid process ID" should {

  //   val url = "blah"

  //   "return a NOT_FOUND error" in new ProcessTest {
  //     val unknownProcessId = "ext90077"
  //     MockRetrieveAndCacheService
  //       .retrieveAndCacheApprovalByPageUrl(s"/$url")(unknownProcessId, unknownProcessId)
  //       .returns(Future.successful(Left(NotFoundError)))
  //     val result = target.approvalPage(unknownProcessId, url)(fakeRequest)
  //     status(result) shouldBe Status.NOT_FOUND
  //   }

  // }

}
