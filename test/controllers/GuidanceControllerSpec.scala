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

import akka.stream.Materializer
import base.{BaseSpec, ViewFns}
import config.ErrorHandler
import play.api.i18n.MessagesApi
import mocks.{MockAppConfig, MockGuidanceConnector, MockGuidanceService, MockSessionRepository}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc._
import play.api.mvc.{AnyContentAsEmpty, BodyParsers}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.test.Helpers.stubMessagesControllerComponents
import uk.gov.hmrc.http.SessionKeys
import models.{PageContext, PageEvaluationContext}
import core.models.ocelot.{KeyedStanza, Labels, Page, Phrase, Process, ProcessJson}
import core.models.ocelot.stanzas.{CurrencyInput, DateInput, ExclusiveSequence, NonExclusiveSequence, Question, _}
import models.ui
import models.ui._
import repositories.ProcessContext
import play.api.test.CSRFTokenHelper._
import play.api.data.FormError
import core.models.errors._
import core.models.ocelot.LabelCache
import core.services._

import scala.concurrent.{ExecutionContext, Future}
import controllers.actions.SessionIdAction
import play.api.inject.Injector
import views.html._
import services._
import mocks.MockPageRenderer

class GuidanceControllerSpec extends BaseSpec with ViewFns with GuiceOneAppPerSuite {

  trait TestData {

    def injector: Injector = app.injector
    val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
    implicit val mat: Materializer = injector.instanceOf[Materializer]

    val ansIndexZero = "0"
    lazy val uuid = "683d9aa0-2a0e-4e28-9ac8-65ce453d2730"
    lazy val sessionId = "session-2882605c-8e96-494a-a497-98ae90f52539"
    lazy val path = "/some-path"
    lazy val relativePath = path.drop(1)
    lazy val expectedUrl = "/start-url"
    lazy val processId = "ext90002"
    lazy val processCode = "testExample"

    lazy val ans1 = Answer(Text("ANS1"), Some(Text("")))
    lazy val ans2 = Answer(Text("ANS2"), Some(Text("")))

    lazy val expectedPage: ui.Page = FormPage(
      path,
      ui.Question(Text("QUESTION"), None, Seq(Paragraph(Text("QUESTION"))), Seq(ans1, ans2))
    )

    val standardPagePath = "/std-page"
    val relativeStdPath = standardPagePath.drop(1)

    val standardPage: ui.Page = ui.Page(
      standardPagePath,
      Seq(H1(Text("hello")))
    )

    val fakeSessionIdAction = new SessionIdAction {
      def parser: BodyParsers.Default = app.injector.instanceOf[BodyParsers.Default]
      implicit protected def executionContext: ExecutionContext = ExecutionContext.global
      override def invokeBlock[A](request: Request[A], block: Request[A] => Future[Result]): Future[Result] = block(request)
    }

    lazy val errorHandler: ErrorHandler = app.injector.instanceOf[config.ErrorHandler]
    lazy val view: standard_page = app.injector.instanceOf[views.html.standard_page]
    lazy val formView: form_page = app.injector.instanceOf[views.html.form_page]

    val instructionStanza: InstructionStanza = InstructionStanza(3, Seq("3"), None, false)
    val questionStanza: Question = Question(Phrase("Which?","Which?"), Seq(Phrase("yes","yes"),Phrase("no","no")), Seq("4","5"), None, false)
    val currencyInputStanza: CurrencyInput = CurrencyInput(Seq("4"),Phrase("",""), None, "PRICE", None, false)
    val dateInputStanza: DateInput = DateInput(Seq("4"),Phrase("",""), None, "Date of birth?", None, false)
    val nonExclusiveSequence: NonExclusiveSequence = NonExclusiveSequence(
      Phrase("Select a working day of the week", "Welsh, Select a working day of the week"),
      Seq("10", "20", "30", "40", "50", "60"),
      Seq(
        Phrase("Monday", "Welsh, Monday"),
        Phrase("Tuesday", "Welsh, Tuesday"),
        Phrase("Wednesday", "Welsh, Wednesday"),
        Phrase("Thursday", "Welsh, Thursday"),
        Phrase("Friday", "Welsh, Friday")
      ),
      None,
      stack = false
    )
    val exclusiveSequence: ExclusiveSequence = ExclusiveSequence(
      Phrase("Select a holiday destination", "Welsh, Select a holiday destination"),
      Seq("10", "20", "30", "40", "50", "60"),
      Seq(
        Phrase("Europe", "Welsh, Europe"),
        Phrase("Africa", "Welsh, Africa"),
        Phrase("Americas", "Welsh, Americas"),
        Phrase("Asia", "Welsh, Asia"),
        Phrase("Elsewhere [exclusive]", "Welsh, Elsewhere")
      ),
      None,
      stack = false
    )
    val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                                        KeyedStanza("1", instructionStanza),
                                        KeyedStanza("3", questionStanza)
                                      )
    val stanzasWithInput: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                                        KeyedStanza("1", instructionStanza),
                                        KeyedStanza("3", currencyInputStanza)
                                      )
    val stanzasWithDateInput: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
      KeyedStanza("1", instructionStanza),
      KeyedStanza("3", dateInputStanza)
    )

    val stanzasWithNonExclusiveSequence: Seq[KeyedStanza] = Seq(
      KeyedStanza("start", PageStanza("/start", Seq("1"), stack = false)),
      KeyedStanza("1", instructionStanza),
      KeyedStanza("3", nonExclusiveSequence)
    )

    val stanzasWithExclusiveSequence: Seq[KeyedStanza] = Seq(
      KeyedStanza("start", PageStanza("/start", Seq("1"), stack = false)),
      KeyedStanza("1", instructionStanza),
      KeyedStanza("3", exclusiveSequence)
    )

    val page = Page("start", "/test-page", stanzas, Seq("4","5"))
    val inputPage = Page("start", "/test-page", stanzasWithInput, Seq("4"))
    val nonQuestionPage = Page("start", "/test-page", stanzas.drop(1), Seq("3"))
    val dateInputPage = Page("start", "/test-page", stanzasWithDateInput, Seq("4"))
    val nonExclusiveSequenceInputPage: Page = Page("start", "/test-page", stanzasWithNonExclusiveSequence, Seq("4"))
    val exclusiveSequenceInputPage: Page = Page("start", "/test-page", stanzasWithExclusiveSequence, Seq("4"))
  }

  trait QuestionTest extends MockGuidanceService with TestData {
    val fakeRequest = FakeRequest("GET", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken

    val formError = new FormError(relativePath, List("error.required"))

    val target = new GuidanceController(
      MockAppConfig,
      fakeSessionIdAction,
      errorHandler,
      view,
      formView,
      mockGuidanceService,
      stubMessagesControllerComponents()
    )

    val initialLabels = LabelCache()
    val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = new PageRenderer().renderPage(page, initialLabels)
    val pec = PageEvaluationContext(
                page,
                vStanzas,
                di,
                sessionId,
                Map("4" -> "/somewhere-else"),
                Some("/hello"),
                Text(),
                processId,
                "hello",
                initialLabels,
                None,
                None
              )
  }

  "Calling sessionRestart" should {
    "Return a SEE_OTHER" in new QuestionTest {
      MockGuidanceService
        .sessionRestart(processId, processId)
        .returns(Future.successful(Right("/start")))

      val result = target.sessionRestart(processId)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "Return INTERNAL_SERVER_ERROR after service failure" in new QuestionTest {
      MockGuidanceService
        .sessionRestart(processId, processId)
        .returns(Future.successful(Left(InternalServerError)))

      val result = target.sessionRestart(processId)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }
  }

  "Calling a valid URL path to a Question page in a process" should {

    "return an OK response" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode))))

      val result = target.getPage(processId, relativePath, None)(fakeRequest)
      status(result) shouldBe Status.OK
    }

    "be a HTML response" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode))))
      val result = target.getPage(processId, relativePath, None)(fakeRequest)
      contentType(result) shouldBe Some("text/html")
    }
  }

  "Calling a valid URL with a previous page link flag to a Question page ina a process" should {

    "return an Ok response" in new QuestionTest {

      override val fakeRequest = FakeRequest("GET", s"$path?$PreviousPageLinkQuery")
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken

      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = true, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode))))

      val result = target.getPage(processId, relativePath, Some("1"))(fakeRequest)

      status(result) shouldBe Status.OK
    }

  }

  "Returning to a previously answered Question page in a process" should {

    "Show the original answer selected" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, processId, Some("/"), Text(Nil), processId, processCode, LabelCache(), None, Some(ansIndexZero)))))

      val result = target.getPage(processId, relativePath, None)(fakeRequest)

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      // Probably not the right place to test this
      contentAsString(result).contains("checked") shouldBe true
    }
  }

  trait QuestionSubmissionTest extends MockSessionRepository with MockGuidanceConnector with TestData  with ProcessJson {
    val fakeRequest = FakeRequest("GET", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken
    val formError = new FormError(relativePath, List("error.required"))
    val guidanceService = new GuidanceService(
      MockAppConfig,
      mockGuidanceConnector,
      mockSessionRepository,
      new PageBuilder(new Placeholders(new DefaultTodayProvider)),
      new PageRenderer,
      new SecuredProcessBuilder(messagesApi),
      new UIBuilder())

    val target = new GuidanceController(
      MockAppConfig,
      fakeSessionIdAction,
      errorHandler,
      view,
      formView,
      guidanceService,
      stubMessagesControllerComponents()
    )
    val process = prototypeJson.as[Process]
  }

  "Submitting a blank Question page form" should {

    "return a BadRequest response" in new QuestionSubmissionTest {
      MockSessionRepository
        .get(processId, Some(s"tell-hmrc$path"), previousPageByLink = false)
        .returns(Future.successful(Right(ProcessContext(process, Map(), Map(), Nil, Map(), Map(), None))))

      override val fakeRequest = FakeRequest("POST", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken
      val result = target.submitPage("tell-hmrc", relativePath)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }
  }

  "Submitting an answered Question page form" should {

    "return a SeeOther response" in new QuestionTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "0")
        .returns(Some("0"))

      MockGuidanceService
        .getPageContext(pec, NoError)
        .returns(PageContext(expectedPage, Seq.empty, None, sessionId, Some("/hello"), Text(Nil), processId, processCode))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "return a InternalServerError when saving of answer and labels fails" in new QuestionTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "0")
        .returns(Some("0"))

      MockGuidanceService
        .getPageContext(pec, NoError)
        .returns(PageContext(expectedPage, Seq.empty, None, sessionId, Some("/hello"), Text(Nil), processId, processCode))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
        .returns(Future.successful(Left(DatabaseError)))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return a BAD_REQUEST when submitting page and guidance determines invalid data" in new QuestionTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "0")
        .returns(Some("0"))

      MockGuidanceService
        .getPageContext(pec, NoError)
        .returns(PageContext(expectedPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
        .returns(Future.successful(Right((None, initialLabels))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return a SeeOther response whether the saving of the question succeeds or not" in new QuestionTest {

      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "0")
        .returns(Some("0"))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      MockGuidanceService
        .getPageContext(pec, NoError)
        .returns(PageContext(expectedPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "return a BAD_REQUEST response if trying to submit a page where url not found in process" in new QuestionTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, "/unknown", previousPageByLink = false, processId)
        .returns(Future.successful(Left(BadRequestError)))

      override val fakeRequest = FakeRequest("POST", "/unknown")
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, "unknown")(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return a BAD_REQUEST response if trying to submit a page which is not a question" in new QuestionTest {
      override val pec = PageEvaluationContext(
            nonQuestionPage,
            Seq.empty,
            None,
            sessionId,
            Map(),
            Some("/hello"),
            Text(),
            processId,
            "hello",
            LabelCache(),
            None,
            None
          )

      MockGuidanceService
        .getPageEvaluationContext(processId, standardPagePath, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getPageContext(pec, ValueMissingError)
        .returns(PageContext(standardPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels))

      override val fakeRequest = FakeRequest("POST", standardPagePath)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, relativeStdPath)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return a NOT_FOUND response if trying to submit to a non-existent page" in new QuestionTest {
      val unknownPath = "/non-existent"
      val unknownRelativePath = unknownPath.drop(1)
      MockGuidanceService
        .getPageEvaluationContext(processId, unknownPath, previousPageByLink = false, processId)
        .returns(Future.successful(Left(NotFoundError)))

      override val fakeRequest = FakeRequest("POST", unknownPath)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, unknownRelativePath)(fakeRequest)
      status(result) shouldBe Status.NOT_FOUND
    }

    "return a INTERNAL_SERVER_ERROR response if submitting to a Process containing errors is referenced" in new QuestionTest {

      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Left(InvalidProcessError)))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return a INTERNAL_SERVER_ERROR response if encountering a database error when submitting a page" in new QuestionTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Left(DatabaseError)))
      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

  }

  trait InputTest extends MockGuidanceService with MockPageRenderer with TestData {

    override lazy val expectedPage: ui.Page = FormPage(
      path,
      ui.CurrencyInput(Text("Input"), Some(Text("hint")), Seq(Paragraph(Text("para"))))
    )
    val enteredValue = "12000"
    val fakeRequest = FakeRequest("GET", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken

    val formError = new FormError(relativePath, List("error.required"))

    val invalidDataFormError = new FormError("", List("error.required"))


    val target = new GuidanceController(
      MockAppConfig,
      fakeSessionIdAction,
      errorHandler,
      view,
      formView,
      mockGuidanceService,
      stubMessagesControllerComponents()
    )

    val initialLabels = LabelCache()
    val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = new PageRenderer().renderPage(inputPage, initialLabels)
    val pec = PageEvaluationContext(
                inputPage,
                vStanzas,
                di,
                sessionId,
                Map("4" -> "/somewhere-else"),
                Some("/hello"),
                Text(),
                processId,
                "hello",
                initialLabels,
                None,
                None
              )

    val validCurrencyInput: String = "50.00"
  }

  "Calling a valid URL path to an Input page in a process" should {

    "return an OK response" in new InputTest {

      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, LabelCache()))))

      val result = target.getPage(processId, relativePath, None)(fakeRequest)
      status(result) shouldBe Status.OK
    }

    "be a HTML response" in new InputTest {
      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, Seq.empty, None, sessionId, Some("/"), Text(Nil), processId, processCode))))
      val result = target.getPage(processId, relativePath, None)(fakeRequest)
      contentType(result) shouldBe Some("text/html")
    }
  }


  "Calling a valid URL path to an Input page where the input has already been input" should {

    "return an OK response" in new InputTest {

      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(
          Future.successful(
            Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, LabelCache(), None, Some(validCurrencyInput)))
          )
        )

      val result = target.getPage(processId, relativePath, None)(fakeRequest)

      status(result) shouldBe Status.OK
    }
  }

  "Returning to an input page in a process" should {

    "Show the original value entered" in new InputTest {
      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels, Some(enteredValue)))))

      val result = target.getPage(processId, relativePath, None)(fakeRequest)

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      // Probably not the right place to test this
      contentAsString(result).contains(enteredValue) shouldBe true
    }
  }

  "Submitting a blank Input page form" should {

    "return a BadRequest response" in new InputTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getPageContext(pec, ValueMissingError)
        .returns(PageContext(expectedPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels))

      MockGuidanceService
        .submitPage(pec, path, "/guidance/hello", "/guidance/hello")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      override val fakeRequest = FakeRequest("POST", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }
  }

  "Submitting an Input page with an invalid value" should {

    "return a BadRequest to the current page" in new InputTest {
      override val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = new PageRenderer().renderPage(inputPage, initialLabels)
      override val pec = PageEvaluationContext(
                inputPage,
                vStanzas,
                di,
                sessionId,
                Map("4" -> "/somewhere-else"),
                Some("/hello"),
                Text(),
                processId,
                "hello",
                initialLabels,
                None,
                None
              )

      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "invalid input")
        .returns(None)

      MockGuidanceService
        .getPageContext(pec, ValueTypeError)
        .returns(PageContext(expectedPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels))

      override val fakeRequest = FakeRequest("POST", path).withSession(SessionKeys.sessionId -> processId)
                                                          .withFormUrlEncodedBody(relativePath -> "invalid input").withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)

      status(result) shouldBe Status.BAD_REQUEST

      val doc = asDocument(contentAsString(result))

      val inputElement = doc.getElementById(s"$relativePath-0")

      elementAttrs(inputElement)("name") shouldBe relativePath

    }
  }

  "Submitting an Input page with a guidance detected invalid value" should {

    "return a BadRequest to the current page and retain input data" in new InputTest {
      override val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = new PageRenderer().renderPage(inputPage, initialLabels)
      override val pec = PageEvaluationContext(
                inputPage,
                vStanzas,
                di,
                sessionId,
                Map("4" -> "/somewhere-else"),
                Some("/hello"),
                Text(),
                processId,
                "hello",
                initialLabels,
                None,
                None
              )

      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "150AA")
        .returns(None)

      MockGuidanceService
        .submitPage(pec, path, "150AA", "150AA")
        .returns(Future.successful(Right((None, pec.labels))))

      MockGuidanceService
        .getPageContext(pec, ValueTypeError)
        .returns(PageContext(expectedPage, Seq.empty, None, sessionId, Some("/hello"), Text(Nil), processId, processCode))

      override val fakeRequest = FakeRequest("POST", path).withSession(SessionKeys.sessionId -> processId)
                                                          .withFormUrlEncodedBody(relativePath -> "150AA").withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)

      status(result) shouldBe Status.BAD_REQUEST

      val doc = asDocument(contentAsString(result))

      val inputElement = doc.getElementById(s"$relativePath-0")

      elementAttrs(inputElement)("value") shouldBe "150AA"

    }
  }

  "Submitting an Input page form with a value" should {

    "return a SeeOther response" in new InputTest {

      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "0")
        .returns(Some("0"))

      MockGuidanceService
        .getPageContext(pec, NoError)
        .returns(PageContext(expectedPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "redirect to new page with out query string after valid submission" in new InputTest {
      override val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = new PageRenderer().renderPage(inputPage, initialLabels)
      override val pec = PageEvaluationContext(
        inputPage,
        vStanzas,
        di,
        sessionId,
        Map("4" -> "/guidance/ext90002/somewhere-else"),
        Some("/hello"),
        Text(),
        processId,
        "hello",
        initialLabels,
        Some("/guidance/ext90002/another-place"),
        None
      )

      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "0")
        .returns(Some("0"))

      MockGuidanceService
        .getPageContext(pec, NoError)
        .returns(PageContext(expectedPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken

      val result = target.submitPage(processId, relativePath)(fakeRequest)

      redirectLocation(result) shouldBe Some("/guidance/ext90002/somewhere-else")
    }

    "redirect to previously visited page with query string after valid submission" in new InputTest {
      override val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = new PageRenderer().renderPage(inputPage, initialLabels)
      override val pec = PageEvaluationContext(
        inputPage,
        vStanzas,
        di,
        sessionId,
        Map("4" -> "/guidance/ext90002/somewhere-else"),
        Some("/hello"),
        Text(),
        processId,
        "hello",
        initialLabels,
        Some("/guidance/ext90002/somewhere-else"),
        None
      )

      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "0")
        .returns(Some("0"))

      MockGuidanceService
        .getPageContext(pec, NoError)
        .returns(PageContext(expectedPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken

      val result = target.submitPage(processId, relativePath)(fakeRequest)

      redirectLocation(result) shouldBe Some("/guidance/ext90002/somewhere-else?p=1")
    }

    "return a SeeOther response whether the saving of the input succeeds or not" in new InputTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "0")
        .returns(Some("0"))

      MockGuidanceService
        .getPageContext(pec, NoError)
        .returns(PageContext(expectedPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "return a INTERNAL_SERVER_ERROR response if submitting to a Process containing errors is referenced" in new InputTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Left(InvalidProcessError)))
      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return a NOT_FOUND response if trying to submit to a non-existent page" in new QuestionTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Left(NotFoundError)))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "/guidance/hello")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.NOT_FOUND
    }

    "return a InternalServerError response when an unexpected error returned from service call" in new QuestionTest {

      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Left(DatabaseError)))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "/guidance/hello")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return a BAD_REQUEST response when a bad request error returned from service call" in new QuestionTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Left(BadRequestError)))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "/guidance/hello")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return a BAD_REQUEST response when submitted page is not an input or question" in new QuestionTest {
      override val pec = PageEvaluationContext(
            nonQuestionPage,
            Seq.empty,
            None,
            sessionId,
            Map(),
            Some("/hello"),
            Text(),
            processId,
            "hello",
            LabelCache(),
            None,
            None
          )

      MockGuidanceService
        .getPageEvaluationContext(processId, standardPagePath, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getPageContext(pec, ValueMissingError)
        .returns(PageContext(standardPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, relativeStdPath)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }
  }

  trait ProcessTest extends MockGuidanceService with TestData {
    lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")

    lazy val target =
      new GuidanceController(
        MockAppConfig,
        fakeSessionIdAction,
        errorHandler,
        view,
        formView,
        mockGuidanceService,
        stubMessagesControllerComponents()
      )
  }

  "Accessing a page from a passphrase process" should {

    trait Test extends MockSessionRepository with MockGuidanceConnector with TestData {
      lazy val fakeRequest = FakeRequest(GET, path).withSession(SessionKeys.sessionId -> processId).withCSRFToken

      val guidanceService = new GuidanceService(
        MockAppConfig,
        mockGuidanceConnector,
        mockSessionRepository,
        new PageBuilder(new Placeholders(new DefaultTodayProvider)),
        new PageRenderer,
        new SecuredProcessBuilder(messagesApi),
        new UIBuilder())

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          guidanceService,
          stubMessagesControllerComponents()
        )
    }

    "Return SEE_OTHER from a getPage() as a result of an Authentication error when non authenticated" in new Test {
      MockSessionRepository
        .get(processId, Some(s"${processId}$path"), false)
        .returns(Future.successful(Left(AuthenticationError)))

      lazy val result = target.getPage(processId, relativePath, None)(fakeRequest)

      status(result) shouldBe Status.SEE_OTHER
    }

    "Return SEE_OTHER from a submit()) as a result of an Authentication error when non authenticated" in new Test {
      MockSessionRepository
        .get(processId, Some(s"${processId}$path"), false)
        .returns(Future.successful(Left(AuthenticationError)))

      lazy val result = target.submitPage(processId, relativePath)(fakeRequest)

      status(result) shouldBe Status.SEE_OTHER
    }
  }

  "Calling a valid URL path for a page in a process" should {

    trait Test extends MockGuidanceService with TestData {
      lazy val fakeRequest = FakeRequest(GET, path).withSession(SessionKeys.sessionId -> processId).withCSRFToken

      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(standardPage, Seq.empty, None, sessionId, Some("/hello"), Text(Nil), processId, processCode))))

      MockGuidanceService
        .savePageState(sessionId, LabelCache())
        .returns(Future.successful(Right({})))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents()
        )
      lazy val result = target.getPage(processId, relativePath, None)(fakeRequest)
    }

    "return a success response" in new Test {
      status(result) shouldBe Status.OK
    }

    "be a HTML response" in new Test {
      contentType(result) shouldBe Some("text/html")
    }

  }

  "Calling a valid URL path for a page and encountering a database error" should {

    trait Test extends MockGuidanceService with TestData {
      lazy val fakeRequest = FakeRequest(GET, path).withSession(SessionKeys.sessionId -> processId).withCSRFToken

      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Left(DatabaseError)))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents()
        )
      lazy val result = target.getPage(processId, relativePath, None)(fakeRequest)
    }

    "return an INTERNAL_SERVER_ERROR response" in new Test {
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "be a HTML response" in new Test {
      contentType(result) shouldBe Some("text/html")
    }

  }

  "Calling a valid URL path for a page and encountering a database error when saving labels" should {

    trait Test extends MockGuidanceService with TestData {
      lazy val fakeRequest = FakeRequest(GET, path).withSession(SessionKeys.sessionId -> processId).withCSRFToken

      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(standardPage, Seq.empty, None, sessionId, Some("/hello"), Text(Nil), processId, processCode))))

      MockGuidanceService
        .savePageState(sessionId, LabelCache())
        .returns(Future.successful(Left(DatabaseError)))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents()
        )
      lazy val result = target.getPage(processId, relativePath, None)(fakeRequest)
    }

    "return an INTERNAL_SERVER_ERROR response" in new Test {
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "be a HTML response" in new Test {
      contentType(result) shouldBe Some("text/html")
    }

  }

  "Calling unknown URL path for a page in a process" should {

    trait Test extends MockGuidanceService with TestData {
      val unknownPath = "/BlahBlah"
      lazy val fakeRequest = FakeRequest(GET, unknownPath).withSession(SessionKeys.sessionId -> processId).withCSRFToken

      MockGuidanceService
        .getPageContext(processId, unknownPath, previousPageByLink = false, processId)
        .returns(Future.successful(Left(NotFoundError)))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents()
        )
      lazy val result = target.getPage(processId, unknownPath.drop(1), None)(fakeRequest)
    }

    "return a success response" in new Test {
      status(result) shouldBe Status.NOT_FOUND
    }

    "be a HTML response" in new Test {
      contentType(result) shouldBe Some("text/html")
    }

  }

  "Calling any URL path for a page in a process with an invalid session" should {

    trait Test extends MockGuidanceService with TestData {
      lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, Seq.empty, None, processId, Some("/hello"), Text(Nil), processId, processCode))))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents()
        )
      lazy val result = target.getPage(processId, path, None)(fakeRequest)
    }

    "return a bad request response" in new Test {

      status(result) shouldBe Status.BAD_REQUEST
    }

    "be a HTML response" in new Test {
      contentType(result) shouldBe Some("text/html")
    }

  }

  "Calling a non-existing URL path for a page in a process" should {

    trait Test extends MockGuidanceService with TestData {
      val unknownPath = "unknown/route"
      lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] =
        FakeRequest().withSession(SessionKeys.sessionId -> processId)

      MockGuidanceService
        .getPageContext(processId, "/" + unknownPath, previousPageByLink = false, processId)
        .returns(Future.successful(Left(NotFoundError)))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents()
        )
      lazy val result = target.getPage(processId, unknownPath, None)(fakeRequest)
    }

    "return not found response" in new Test {
      status(result) shouldBe Status.NOT_FOUND
    }

    "be a HTML response" in new Test {
      contentType(result) shouldBe Some("text/html")
    }

  }

  "Date Input processing" should {
    trait DateInputTest extends MockGuidanceService with TestData {

      override lazy val expectedPage: ui.Page = FormPage(
        path,
        ui.DateInput(Text("Input"), Some(Text("hint")), Seq(Paragraph(Text("para"))))
      )
      val enteredDate = "1/1/2020"
      val fakeRequest = FakeRequest("GET", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken

      val target = new GuidanceController(
        MockAppConfig,
        fakeSessionIdAction,
        errorHandler,
        view,
        formView,
        mockGuidanceService,
        stubMessagesControllerComponents()
      )

      val initialLabels = LabelCache()
      val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = new PageRenderer().renderPage(dateInputPage, initialLabels)
      val pec = PageEvaluationContext(
        dateInputPage,
        vStanzas,
        di,
        sessionId,
        Map("4" -> "/somewhere-else"),
        Some("/hello"),
        Text(),
        processId,
        "hello",
        initialLabels,
        None,
        None
      )

      val validSubmittedDateAnswer: String = "10/10/2020"
      val invalidSubmittedDateAnswer: String = "xx/10/2012"
    }

    "Calling a valid URL path to an Date Input page in a process" should {

      "return an OK response" in new DateInputTest {

        MockGuidanceService
          .getPageContext(processId, path, previousPageByLink = false, processId)
          .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels))))

        val result = target.getPage(processId, relativePath, None)(fakeRequest)
        status(result) shouldBe Status.OK
      }

      "be a HTML response" in new DateInputTest {
        MockGuidanceService
          .getPageContext(processId, path, previousPageByLink = false, processId)
          .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels))))
        val result = target.getPage(processId, relativePath, None)(fakeRequest)
        contentType(result) shouldBe Some("text/html")
      }
    }

    "Returning to a date input page in a process" should {

      "Show the original value entered" in new DateInputTest {
        MockGuidanceService
          .getPageContext(processId, path, previousPageByLink = false, processId)
          .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels, Some(enteredDate)))))

        val result = target.getPage(processId, relativePath, None)(fakeRequest)

        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
      }
    }

    "Calling a date input page where the date has already been entered" should {

      "return an Ok response" in new DateInputTest {

        MockGuidanceService
          .getPageContext(processId, path, previousPageByLink = false, processId)
          .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels, None, Some(validSubmittedDateAnswer)))))

        val result = target.getPage(processId, relativePath, None)(fakeRequest)

        status(result) shouldBe Status.OK
      }
    }

    "Calling a page with a missing date input component" should {

      "return a bad request response" in new DateInputTest {

        MockGuidanceService
          .getPageContext(processId, path, previousPageByLink = false, processId)
          .returns(
            Future.successful(Right(PageContext(expectedPage, Seq.empty, None, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels)))
          )

        val result = target.getPage(processId, relativePath, None)(fakeRequest)

        status(result) shouldBe Status.BAD_REQUEST

      }
    }

    "Submitting a valid answer to a date input page" should {

      "return a see other response" in new DateInputTest {

        MockGuidanceService
          .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
          .returns(Future.successful(Right(pec)))

        MockGuidanceService
          .validateUserResponse(pec, validSubmittedDateAnswer)
          .returns(Some(validSubmittedDateAnswer))

        MockGuidanceService
          .submitPage(pec, path, validSubmittedDateAnswer, validSubmittedDateAnswer)
          .returns(Future.successful(Right((Some("4"), LabelCache()))))

        override val fakeRequest = FakeRequest("POST", path)
          .withSession(SessionKeys.sessionId -> processId)
          .withFormUrlEncodedBody(
            "day" -> "10",
            "month" -> "10",
            "year" -> "2020"
          )
          .withCSRFToken

        val result = target.submitPage(processId, relativePath)(fakeRequest)

        status(result) shouldBe Status.SEE_OTHER
      }

      "Submitting an incomplete date" should {

        "returns a bad request response" in new DateInputTest {

          MockGuidanceService
            .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
            .returns(Future.successful(Right(pec)))

          MockGuidanceService
            .getPageContext(pec, ValueMissingGroupError(List("label.year"))) // Use message key as message substitution isn't working
            .returns(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels))

          override val fakeRequest = FakeRequest("POST", path)
            .withSession(SessionKeys.sessionId -> processId)
            .withFormUrlEncodedBody(
              "day" -> "10",
              "month" -> "10"
            )
            .withCSRFToken

          val result = target.submitPage(processId, relativePath)(fakeRequest)

          status(result) shouldBe Status.BAD_REQUEST
        }

      }

      "Submitting an invalid date" should {

        "return a bad request response" in new DateInputTest {

          MockGuidanceService
            .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
            .returns(Future.successful(Right(pec)))

          MockGuidanceService
            .validateUserResponse(pec, invalidSubmittedDateAnswer)
            .returns(None)

          MockGuidanceService
            .getPageContext(pec, ValueTypeError)
            .returns(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels))

          override val fakeRequest = FakeRequest("POST", path)
            .withSession(SessionKeys.sessionId -> processId)
            .withFormUrlEncodedBody(
              "day" -> "xx",
              "month" -> "10",
              "year" -> "2012"
            )
            .withCSRFToken

          val result = target.submitPage(processId, relativePath)(fakeRequest)

          status(result) shouldBe Status.BAD_REQUEST
        }
      }

      "Submitting to a page without a data input component" should {

        "return a bad request response" in new DateInputTest {

          override val pec = PageEvaluationContext(
            dateInputPage,
            vStanzas,
            None,
            sessionId,
            Map("4" -> "/somewhere-else"),
            Some("/hello"),
            Text(),
            processId,
            "hello",
            initialLabels,
            None,
            None
          )

          MockGuidanceService
            .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
            .returns(Future.successful(Right(pec)))

          override val fakeRequest = FakeRequest("POST", path)
            .withSession(SessionKeys.sessionId -> processId)
            .withFormUrlEncodedBody(
              "day" -> "10",
              "month" -> "10",
              "year" -> "2020"
            )
            .withCSRFToken

          val result = target.submitPage(processId, relativePath)(fakeRequest)

          status(result) shouldBe Status.BAD_REQUEST

        }
      }
    }
  }

  "Non-exclusive sequence input processing" should {

    trait SequenceInputTest extends MockGuidanceService with TestData {

      override lazy val expectedPage: ui.Page = FormPage(
        path,
        ui.NonExclusiveSequence(
          Text("Select a working day of the week"),
          None,
          Seq(
            Text("Monday"),
            Text("Tuesday"),
            Text("Wednesday"),
            Text("Thursday"),
            Text("Friday")
          ),
          Seq(Paragraph(Text("When did you go into work?"))),
          Seq.empty
        )
      )

      val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody().withCSRFToken

      val target: GuidanceController = new GuidanceController(
        MockAppConfig,
        fakeSessionIdAction,
        errorHandler,
        view,
        formView,
        mockGuidanceService,
        stubMessagesControllerComponents()
      )

      val initialLabels: Labels = LabelCache()
      val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = new PageRenderer()
        .renderPage(nonExclusiveSequenceInputPage, initialLabels)

      val pec: PageEvaluationContext = PageEvaluationContext(
        nonExclusiveSequenceInputPage,
        vStanzas,
        di,
        sessionId,
        Map("4" -> "somewhere-else"),
        Some("Hello"),
        Text(),
        processId,
        processCode,
        labels
      )

      val validSequenceAnswer: String = "0,2,4"
      val invalidSequenceAnswer: String = "0,3,6"
    }

    "Calling a valid Url path to a sequence input page" should {

      "return an OK response" in new SequenceInputTest {

        MockGuidanceService
          .getPageContext(processId, path, previousPageByLink = false, processId)
          .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels))))

        val result = target.getPage(processId, relativePath, None)(fakeRequest)

        status(result) shouldBe Status.OK
      }

      "be a HTML response" in new SequenceInputTest {

        MockGuidanceService
          .getPageContext(processId, path, previousPageByLink = false, processId)
          .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels))))

        val result = target.getPage(processId, relativePath, None)(fakeRequest)

        contentType(result) shouldBe Some("text/html")
      }

    }

    "Calling a non-exclusive sequence input page when a selection has been made previously" should {

      "return an Ok response" in new SequenceInputTest {

        MockGuidanceService
          .getPageContext(processId, path, previousPageByLink = false, processId)
          .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels, None, Some(validSequenceAnswer)))))

        val result = target.getPage(processId, relativePath, None)(fakeRequest)

        status(result) shouldBe Status.OK
      }
    }

    "Calling a page with a missing sequence input component" should {

      "return a bad request response" in new SequenceInputTest {

        MockGuidanceService
          .getPageContext(processId, path, previousPageByLink = false, processId)
          .returns(
            Future.successful(Right(PageContext(expectedPage, Seq.empty, None, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels)))
          )

        val result = target.getPage(processId, relativePath, None)(fakeRequest)

        status(result) shouldBe Status.BAD_REQUEST

      }

    }

    "Submitting a valid answer to a sequence input page" should {

      "return a see other response" in new SequenceInputTest {

        MockGuidanceService
          .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
          .returns(Future.successful(Right(pec)))

        MockGuidanceService
          .validateUserResponse(pec, validSequenceAnswer)
          .returns(Some(validSequenceAnswer))

        MockGuidanceService
          .submitPage(pec, path, validSequenceAnswer, validSequenceAnswer)
          .returns(Future.successful(Right((Some("4"), LabelCache()))))

        override val fakeRequest = FakeRequest("POST", path)
          .withSession(SessionKeys.sessionId -> processId)
          .withFormUrlEncodedBody(
            s"$relativePath[0]" -> "0",
            s"$relativePath[2]" -> "2",
            s"$relativePath[4]" -> "4"
          )
          .withCSRFToken

        val result = target.submitPage(processId, relativePath)(fakeRequest)

        status(result) shouldBe Status.SEE_OTHER
      }

      "submitting a form with no options selected" should {

        "returns a bad request response" in new SequenceInputTest {

          MockGuidanceService
            .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
            .returns(Future.successful(Right(pec)))

          MockGuidanceService
            .getPageContext(pec, ValueMissingError)
            .returns(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels))

          override val fakeRequest = FakeRequest("POST", path)
            .withSession(SessionKeys.sessionId -> processId)
            .withFormUrlEncodedBody()
            .withCSRFToken

          val result = target.submitPage(processId, relativePath)(fakeRequest)

          status(result) shouldBe Status.BAD_REQUEST
        }

      }

      "Submitting an invalid selection" should {

        "return a bad request response" in new SequenceInputTest {

          MockGuidanceService
            .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
            .returns(Future.successful(Right(pec)))

          MockGuidanceService
            .validateUserResponse(pec, invalidSequenceAnswer)
            .returns(None)

          MockGuidanceService
            .getPageContext(pec, ValueTypeError)
            .returns(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels))

          override val fakeRequest = FakeRequest("POST", path)
            .withSession(SessionKeys.sessionId -> processId)
            .withFormUrlEncodedBody(
              s"$relativePath[0]" -> "0",
              s"$relativePath[3]" -> "3",
              s"$relativePath[4]" -> "6"
            )
            .withCSRFToken

          val result = target.submitPage(processId, relativePath)(fakeRequest)

          status(result) shouldBe Status.BAD_REQUEST
        }
      }
    }

  }

  "Exclusive sequence input processing" should {

    trait ExclusiveSequenceInputTest extends MockGuidanceService with TestData {

      override lazy val expectedPage: ui.Page = FormPage(
        path,
        ui.ExclusiveSequence(
          Text("Select a holiday destination"),
          None,
          Seq(
            Text("Europe"),
            Text("Africa"),
            Text("Americas"),
            Text("Asia")
          ),
          Text("Elsewhere"),
          Seq(Paragraph(Text("When did you go into work?"))),
          Seq.empty
        )
      )

      val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody().withCSRFToken

      val target: GuidanceController = new GuidanceController(
        MockAppConfig,
        fakeSessionIdAction,
        errorHandler,
        view,
        formView,
        mockGuidanceService,
        stubMessagesControllerComponents()
      )

      val initialLabels: Labels = LabelCache()
      val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = new PageRenderer()
        .renderPage(exclusiveSequenceInputPage, initialLabels)

      val pec: PageEvaluationContext = PageEvaluationContext(
        exclusiveSequenceInputPage,
        vStanzas,
        di,
        sessionId,
        Map("4" -> "somewhere-else"),
        Some("Hello"),
        Text(),
        processId,
        processCode,
        labels
      )

      val validExclusiveSequenceAnswer: String = "0,3"
      val invalidExclusiveSequenceAnswer: String = "0,3,6"
    }

    "Calling a valid Url path to a sequence input page" should {

      "return an OK response" in new ExclusiveSequenceInputTest {

        MockGuidanceService
          .getPageContext(processId, path, previousPageByLink = false, processId)
          .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels))))

        val result = target.getPage(processId, relativePath, None)(fakeRequest)

        status(result) shouldBe Status.OK
      }

      "be a HTML response" in new ExclusiveSequenceInputTest {

        MockGuidanceService
          .getPageContext(processId, path, previousPageByLink = false, processId)
          .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels))))

        val result = target.getPage(processId, relativePath, None)(fakeRequest)

        contentType(result) shouldBe Some("text/html")
      }

    }

    "Calling a non-exclusive sequence input page when a selection has been made previously" should {

      "return an Ok response" in new ExclusiveSequenceInputTest {

        MockGuidanceService
          .getPageContext(processId, path, previousPageByLink = false, processId)
          .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels, None, Some(validExclusiveSequenceAnswer)))))

        val result = target.getPage(processId, relativePath, None)(fakeRequest)

        status(result) shouldBe Status.OK
      }
    }

    "Calling a page with a missing exclusive sequence input component" should {

      "return a bad request response" in new ExclusiveSequenceInputTest {

        MockGuidanceService
          .getPageContext(processId, path, previousPageByLink = false, processId)
          .returns(
            Future.successful(Right(PageContext(expectedPage, Seq.empty, None, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels)))
          )

        val result = target.getPage(processId, relativePath, None)(fakeRequest)

        status(result) shouldBe Status.BAD_REQUEST
      }

    }

    "Submitting a valid answer to a sequence input page" should {

      "return a see other response" in new ExclusiveSequenceInputTest {

        MockGuidanceService
          .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
          .returns(Future.successful(Right(pec)))

        MockGuidanceService
          .validateUserResponse(pec, validExclusiveSequenceAnswer)
          .returns(Some(validExclusiveSequenceAnswer))

        MockGuidanceService
          .submitPage(pec, path, validExclusiveSequenceAnswer, validExclusiveSequenceAnswer)
          .returns(Future.successful(Right((Some("4"), LabelCache()))))

        override val fakeRequest = FakeRequest("POST", path)
          .withSession(SessionKeys.sessionId -> processId)
          .withFormUrlEncodedBody(
            s"$relativePath[0]" -> "0",
            s"$relativePath[2]" -> "3"
          )
          .withCSRFToken

        val result = target.submitPage(processId, relativePath)(fakeRequest)

        status(result) shouldBe Status.SEE_OTHER
      }

      "submitting a form with no options selected" should {

        "returns a bad request response" in new ExclusiveSequenceInputTest {

          MockGuidanceService
            .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
            .returns(Future.successful(Right(pec)))

          MockGuidanceService
            .getPageContext(pec, ValueMissingError)
            .returns(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels))

          override val fakeRequest = FakeRequest("POST", path)
            .withSession(SessionKeys.sessionId -> processId)
            .withFormUrlEncodedBody()
            .withCSRFToken

          val result = target.submitPage(processId, relativePath)(fakeRequest)

          status(result) shouldBe Status.BAD_REQUEST
        }

      }

      "Submitting an invalid selection" should {

        "return a bad request response" in new ExclusiveSequenceInputTest {

          MockGuidanceService
            .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
            .returns(Future.successful(Right(pec)))

          MockGuidanceService
            .validateUserResponse(pec, invalidExclusiveSequenceAnswer)
            .returns(None)

          MockGuidanceService
            .getPageContext(pec, ValueTypeError)
            .returns(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels))

          override val fakeRequest = FakeRequest("POST", path)
            .withSession(SessionKeys.sessionId -> processId)
            .withFormUrlEncodedBody(
              s"$relativePath[0]" -> "0",
              s"$relativePath[3]" -> "3",
              s"$relativePath[4]" -> "6"
            )
            .withCSRFToken

          val result = target.submitPage(processId, relativePath)(fakeRequest)

          status(result) shouldBe Status.BAD_REQUEST
        }
      }

    }

  }

}
