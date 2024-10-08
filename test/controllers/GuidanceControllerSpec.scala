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

import org.apache.pekko.stream.Materializer
import base.{BaseSpec, ViewFns}
import config.ErrorHandler
import controllers.actions.SessionIdAction
import core.models.errors._
import core.models.ocelot.errors._
import core.models.ocelot.stanzas.{CurrencyInput, DateInput, Question, Sequence, _}
import core.models.ocelot.{KeyedStanza, LabelCache, Labels, Meta, Page, Phrase, Process, ProcessJson, Published, Scratch}
import core.services._
import forms.FormProviderFactory
import forms.providers._
import mocks._
import core.models.errors.Error
import models.ui._
import models._
import play.api.data.FormError
import play.api.http.Status
import play.api.i18n.{Messages, MessagesApi}
import play.api.mvc._
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import models.{PageHistory, Session, SessionKey}
import repositories.SessionFSM
import services._
import uk.gov.hmrc.http.{HeaderCarrier, HeaderNames, RequestId, SessionKeys}
import views.html._
import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

class GuidanceControllerSpec extends BaseSpec with ViewFns {

  trait TestBase {

    val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
    implicit val messages: Messages = messagesApi.preferred(Seq())

    val requestIdValue: String = "71dcc4a3-9d19-47f5-ad97-74bb6c2a15c4"
    implicit val mat: Materializer = injector.instanceOf[Materializer]
    implicit val headerCarrier: HeaderCarrier = HeaderCarrier(requestId = Some(RequestId(requestIdValue)))
    val requestId: Option[String] = Some(requestIdValue)
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

    val instruction: Instruction = Instruction(Phrase("Instruction", "Instruction"), Seq("3"), None, false)
    val questionStanza: Question = Question(Phrase("Which?","Which?"), Seq(Phrase("yes","yes"),Phrase("no","no")), Seq("4","5"), None, false)
    val currencyInputStanza: CurrencyInput = CurrencyInput(Seq("4"),Phrase("",""), None, "PRICE", None, false)
    val dateInputStanza: DateInput = DateInput(Seq("4"),Phrase("",""), None, "Date of birth?", None, false)
    val nonExclusiveSequence: Sequence = Sequence(
      Phrase("Select a working day of the week", "Welsh: Select a working day of the week"),
      Seq("10", "20", "30", "40", "50", "60"),
      Seq(
        Phrase("Monday", "Welsh: Monday"),
        Phrase("Tuesday", "Welsh: Tuesday"),
        Phrase("Wednesday", "Welsh: Wednesday"),
        Phrase("Thursday", "Welsh: Thursday"),
        Phrase("Friday", "Welsh: Friday")
      ),
      None,
      None,
      stack = false
    )
    val exclusiveSequence: Sequence = Sequence(
      Phrase("Select a holiday destination", "Welsh: Select a holiday destination"),
      Seq("10", "20", "30", "40", "50", "60"),
      Seq(
        Phrase("Europe", "Welsh: Europe"),
        Phrase("Africa", "Welsh: Africa"),
        Phrase("Americas", "Welsh: Americas"),
        Phrase("Asia", "Welsh: Asia"),
      ),
      Some(Phrase(
        "Elsewhere [exclusive][hint:Selecting this checkbox will deselect the other checkboxes]",
        "Welsh: Elsewhere [exclusive][hint:Welsh: Selecting this checkbox will deselect the other checkboxes]"
      )),
      None,
      stack = false
    )
    val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                                        KeyedStanza("1", instruction),
                                        KeyedStanza("3", questionStanza)
                                      )
    val stanzasWithInput: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                                        KeyedStanza("1", instruction),
                                        KeyedStanza("3", currencyInputStanza)
                                      )
    val stanzasWithDateInput: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
      KeyedStanza("1", instruction),
      KeyedStanza("3", dateInputStanza)
    )

    val stanzasWithNonExclusiveSequence: Seq[KeyedStanza] = Seq(
      KeyedStanza("start", PageStanza("/start", Seq("1"), stack = false)),
      KeyedStanza("1", instruction),
      KeyedStanza("3", nonExclusiveSequence)
    )

    val stanzasWithExclusiveSequence: Seq[KeyedStanza] = Seq(
      KeyedStanza("start", PageStanza("/start", Seq("1"), stack = false)),
      KeyedStanza("1", instruction),
      KeyedStanza("3", exclusiveSequence)
    )

    val page = Page("start", "/test-page", stanzas, Seq("4","5"))
    val inputPage = Page("start", "/test-page", stanzasWithInput, Seq("4"))
    val nonQuestionPage = Page("start", "/test-page", stanzas.drop(1), Seq("3"))
    val dateInputPage = Page("start", "/test-page", stanzasWithDateInput, Seq("4"))
    val nonExclusiveSequenceInputPage: Page = Page("start", "/test-page", stanzasWithNonExclusiveSequence, Seq("4"))
    val exclusiveSequenceInputPage: Page = Page("start", "/test-page", stanzasWithExclusiveSequence, Seq("4"))
    val meta = Meta(processId, "", None, None, 0, "", 1L, 0, None, None, processCode)
    val pageMap = Map("/start" -> PageNext("1", List("2", "3")), path -> PageNext("2"))
    val emptyProcess = Process(meta, Map(), Vector(), Vector())
    val formProvider: FormProviderFactory = new FormProviderFactory(new DateFormProvider, new StringFormProvider, new StringListFormProvider, new PassphraseFormProvider)

    def renderPage(page: Page, labels: Labels):(Seq[VisualStanza], Labels, Option[DataInput]) =
      new PageRenderer(MockAppConfig).renderPage(page, labels).fold(_ => fail(), result => result)
  }

  trait QuestionTest extends MockGuidanceService with TestBase {
    val fakeRequest = FakeRequest("GET", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken

    val formError = new FormError(relativePath, List("error.required"))

    val target = new GuidanceController(
      MockAppConfig,
      fakeSessionIdAction,
      errorHandler,
      view,
      formView,
      mockGuidanceService,
      stubMessagesControllerComponents(),
      formProvider
    )

    val initialLabels = LabelCache()
    val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = renderPage(page, initialLabels)
    val pec = PageEvaluationContext(
                page,
                vStanzas,
                di,
                sessionId,
                Map("4" -> PageDesc("4", "/somewhere-else")),
                Some("/hello"),
                Text(),
                processId,
                "hello",
                initialLabels,
                None,
                None
              )
  }

  trait TestWithRealGuidanceService extends MockSessionService with MockGuidanceConnector with MockDebugService with TestBase {
    val fakeRequest = FakeRequest("GET", path)
                        .withSession(SessionKeys.sessionId -> sessionId)
                        .withHeaders(HeaderNames.xRequestId -> requestId.get)
                        .withFormUrlEncodedBody()
                        .withCSRFToken

    val formError = new FormError(relativePath, List("error.required"))
    val guidanceService = new GuidanceService(
      MockAppConfig,
      mockSessionService,
      mockDebugService,
      new PageBuilder(new LabelledData(new Timescales(new DefaultTodayProvider), new Rates())),
      new PageRenderer(MockAppConfig),
      new SecuredProcessBuilder(messagesApi),
      new UIBuilder(),
      new SessionFSM,
      messagesApi,
      new EncrypterService(MockAppConfig)
    )

    val target = new GuidanceController(
      MockAppConfig,
      fakeSessionIdAction,
      errorHandler,
      view,
      formView,
      guidanceService,
      stubMessagesControllerComponents(),
      formProvider
    )

    val initialLabels = LabelCache()
    val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = renderPage(page, initialLabels)
    val pec = PageEvaluationContext(
                page,
                vStanzas,
                di,
                sessionId,
                Map("4" -> PageDesc("4", "/somewhere-else")),
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
        .sessionRestart(processCode, processId)
        .returns(Future.successful(Right("/start")))

      val result = target.sessionRestart(processCode)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "Return INTERNAL_SERVER_ERROR after service failure" in new QuestionTest {
      MockGuidanceService
        .sessionRestart(processCode, processId)
        .returns(Future.successful(Left((InternalServerError, None))))

      val result = target.sessionRestart(processCode)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "Return SEE_OTHER after no session" in new TestWithRealGuidanceService {
      MockSessionService
        .reset(sessionId, processCode, requestId)
        .returns(Future.successful(Left(NotFoundError)))

      val result = target.sessionRestart(processCode)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER

      redirectLocation(result) shouldBe Some(s"/guidance/$processCode")
    }

    "Return SEE_OTHER after wrong session found" in new TestWithRealGuidanceService {
      MockSessionService
        .reset(sessionId, processCode, requestId)
        .returns(Future.successful(Left(SessionNotFoundError)))

      val result = target.sessionRestart(processCode)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER

      redirectLocation(result) shouldBe Some(s"/guidance/$processCode")
    }

    "Return SEE_OTHER when non sessionId found" in new TestWithRealGuidanceService {
      override val fakeRequest = FakeRequest("GET", path)
                          .withSession(SessionKeys.sessionId -> processId)
                          .withHeaders(HeaderNames.xRequestId -> requestId.get)
                          .withFormUrlEncodedBody()
                          .withCSRFToken

      MockSessionService
        .reset(processId, processCode, requestId)
        .returns(Future.successful(Left(ExpectationFailedError)))

      val result = target.sessionRestart(processCode)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER

      redirectLocation(result) shouldBe Some(s"/guidance/$processCode")
    }

  }

  "Calling a valid URL path to a Question page in a process" should {

    "return an OK response" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, LabelCache()))))

      val result = target.getPage(processId, relativePath, None)(fakeRequest)
      status(result) shouldBe Status.OK
    }

    "be a HTML response" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, LabelCache()))))
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
        .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, LabelCache()))))

      val result = target.getPage(processId, relativePath, Some("1"))(fakeRequest)

      status(result) shouldBe Status.OK
    }

  }

  "Calling a URL which includes unsupported characters" should {

    "return a NotFound response" in new QuestionTest {

      override val fakeRequest = FakeRequest("GET", s"$path?$PreviousPageLinkQuery")
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken

      val invalidProcessCode = "check-what-inform%E2%80%A6n-to-give-your-new-employer"
      val result = target.getPage(invalidProcessCode, "outcome-starter-checklist-only", Some("1"))(fakeRequest)

      status(result) shouldBe Status.NOT_FOUND
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

  trait QuestionSubmissionTest extends MockGuidanceService with MockSessionService with MockGuidanceConnector with MockDebugService with TestBase  with ProcessJson {
    val fakeRequest = FakeRequest("GET", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken
    val formError = new FormError(relativePath, List("error.required"))
    val guidanceService = new GuidanceService(
      MockAppConfig,
      mockSessionService,
      mockDebugService,
      new PageBuilder(new LabelledData(new Timescales(new DefaultTodayProvider), new Rates())),
      new PageRenderer(MockAppConfig),
      new SecuredProcessBuilder(messagesApi),
      new UIBuilder(),
      new SessionFSM,
      messagesApi,
      new EncrypterService(MockAppConfig))

    val target = new GuidanceController(
      MockAppConfig,
      fakeSessionIdAction,
      errorHandler,
      view,
      formView,
      guidanceService,
      stubMessagesControllerComponents(),
      formProvider
    )
    val process = prototypeJson.as[Process]
  }

  "Submitting a blank Question page form" should {

    "return a NOT_FOUND response" in new QuestionSubmissionTest {
      val url = "/rent/less-than-1000/do-you-receive-any-income"
      val session = Session(SessionKey(processId, process.meta.processCode), Some(Published), process.meta.id, Map(), Nil, Map(), Map(), Nil, Nil, None, Instant.now, process.meta.lastUpdate, process.meta.timescalesVersion, process.meta.ratesVersion)
      val guidanceSession = GuidanceSession(session, process, Map(), List(PageHistory(s"tell-hmrc$url", Nil, Nil)))

      MockSessionService
        .get(processId, process.meta.processCode, requestId)
        .returns(Future.successful(Right(
          guidanceSession
        )))

      MockSessionService
        .getNoUpdate(processId, process.meta.processCode)
        .returns(Future.successful(Right(
          guidanceSession
        )))

      override val fakeRequest = FakeRequest("POST", path)
                                    .withSession(SessionKeys.sessionId -> processId)
                                    .withHeaders(HeaderNames.xRequestId -> requestId.get)
                                    .withFormUrlEncodedBody()
                                    .withCSRFToken
      val result = target.submitPage("tell-hmrc", url.drop(1))(fakeRequest)
      status(result) shouldBe Status.NOT_FOUND
    }
  }

  "Submitting to page out of sequence" should {
    val url = "/rent/less-than-1000/do-you-receive-any-income"

    "Force redirect to current page" in new QuestionSubmissionTest with MockGuidanceService {
      val outOfSequence = "/rent/1000-or-more/was-your-income-more-than-3750"
      val guidanceSession = GuidanceSession(process, Map(), Map(), Nil, Map(), Map(), Nil, Some(url), None, Published, Nil)
      MockGuidanceService
        .getSubmitGuidanceSession(processId, process.meta.processCode, Some(s"tell-hmrc$outOfSequence"))
        .returns(Future.successful(Left(IllegalPageSubmissionError)))

      MockSessionService
        .get(processId, process.meta.processCode, requestId)
        .returns(Future.successful(Right(
          GuidanceSession(Session(SessionKey(processId, process.meta.processCode), Some(Published), process.meta.id, Map(), Nil, Map(), Map(),
            List(RawPageHistory("start", Nil, Nil)), Nil, None, Instant.now, process.meta.lastUpdate, process.meta.timescalesVersion, process.meta.ratesVersion), process, Map(url -> PageNext("36", Nil, Nil), outOfSequence -> PageNext("80", Nil, Nil)), List(PageHistory(s"tell-hmrc$url", Nil, Nil)))
        )))

      MockSessionService
        .getNoUpdate(processId, process.meta.processCode)
        .returns(Future.successful(Right(
          GuidanceSession(Session(SessionKey(processId, process.meta.processCode), Some(Published), process.meta.id,Map(), Nil, Map(), Map(),
                  List(RawPageHistory("start", Nil, Nil)), Nil, None, Instant.now, process.meta.lastUpdate, process.meta.timescalesVersion, process.meta.ratesVersion), process, Map(url -> PageNext("36", Nil, Nil), outOfSequence -> PageNext("80", Nil, Nil)), List(PageHistory(s"tell-hmrc$url", Nil, Nil)))
        )))

      override val fakeRequest = FakeRequest("POST", outOfSequence)
                                  .withSession(SessionKeys.sessionId -> processId)
                                  .withHeaders(HeaderNames.xRequestId -> requestId.get)
                                  .withFormUrlEncodedBody()
                                  .withCSRFToken
      val result = target.submitPage("tell-hmrc", outOfSequence.drop(1))(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe guidanceSession.currentPageUrl.map(url => s"/guidance/tell-hmrc$url")
    }

    "Restart process when no current page available" in new QuestionSubmissionTest with MockGuidanceService {

      MockGuidanceService
        .getSubmitGuidanceSession(processId, process.meta.processCode, Some(s"tell-hmrc$path"))
        .returns(Future.successful(Left(IllegalPageSubmissionError)))

      MockSessionService
        .get(processId, process.meta.processCode, requestId)
        .returns(Future.successful(Right(
          GuidanceSession(Session(SessionKey(processId, process.meta.processCode), Some(Published), process.meta.id, Map(), Nil, Map(), Map(),
                  List(), Nil, None, Instant.now, process.meta.lastUpdate, process.meta.timescalesVersion, process.meta.ratesVersion), process, Map(), List())
        )))

      MockSessionService
        .getNoUpdate(processId, process.meta.processCode)
        .returns(Future.successful(Right(
          GuidanceSession(Session(SessionKey(processId, process.meta.processCode), Some(Published), process.meta.id, Map(), Nil, Map(), Map(),
                  List(), Nil, None, Instant.now, process.meta.lastUpdate, process.meta.timescalesVersion, process.meta.ratesVersion), process, Map(), List())
        )))

      override val fakeRequest = FakeRequest("POST", "some-other-url")
                                  .withSession(SessionKeys.sessionId -> processId)
                                  .withHeaders(HeaderNames.xRequestId -> requestId.get)
                                  .withFormUrlEncodedBody()
                                  .withCSRFToken
      val result = target.submitPage("tell-hmrc", "some-other-url")(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe Some("/guidance/tell-hmrc")
    }

    "Sync process when process code doesnt match current session" in new QuestionSubmissionTest with MockGuidanceService {
      val session = Session(SessionKey(processId, process.meta.processCode), Some(Published), process.meta.id,Map(), Nil, Map(),
                            Map(),
                            List(RawPageHistory("start", Nil, Nil)), Nil, None, Instant.now, process.meta.lastUpdate, process.meta.timescalesVersion, process.meta.ratesVersion)

      val guidanceSession = GuidanceSession(process, Map(), Map(), Nil, Map(), Map(), Nil, Some("/current-page-url"), None, Published, Nil)
      MockGuidanceService
        .getSubmitGuidanceSession(processId, "blah", Some(s"blah$path"))
        .returns(Future.successful(Left(SessionNotFoundError)))

      MockSessionService
        .get(processId, "blah", requestId)
        .returns(Future.successful(Left(SessionNotFoundError)))

      MockSessionService
        .getNoUpdate(processId, process.meta.processCode)
        .returns(Future.successful(Right(GuidanceSession(session, process, Map(url -> PageNext("36", Nil, Nil)), List()))))

      override val fakeRequest = FakeRequest("POST", path)
                                  .withSession(SessionKeys.sessionId -> processId)
                                  .withHeaders(HeaderNames.xRequestId -> requestId.get)
                                  .withFormUrlEncodedBody()
                                  .withCSRFToken
      val result = target.submitPage("blah", relativePath)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
      redirectLocation(result) shouldBe guidanceSession.currentPageUrl.map(url => s"/guidance/blah")
    }

    "Return Internal server error when a Database error occurs" in new QuestionSubmissionTest with MockGuidanceService {
      MockGuidanceService
        .getSubmitGuidanceSession(processId, "blah", Some(s"blah$path"))
        .returns(Future.successful(Left(IllegalPageSubmissionError)))

      MockSessionService
        .get(processId, "blah", requestId)
        .returns(Future.successful(Left(DatabaseError)))

      MockSessionService
        .getNoUpdate(processId, process.meta.processCode)
        .returns(Future.successful(Left(DatabaseError)))

      override val fakeRequest = FakeRequest("POST", path)
                                  .withSession(SessionKeys.sessionId -> processId)
                                  .withHeaders(HeaderNames.xRequestId -> requestId.get)
                                  .withFormUrlEncodedBody()
                                  .withCSRFToken
      val result = target.submitPage("blah", relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

  }

  "Submitting an answered Question page form" should {

    "return a SeeOther response" in new QuestionTest {
      MockGuidanceService
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getSubmitPageContext(pec, NoError)
        .returns(Right(PageContext(expectedPage, Seq.empty, None, sessionId, Some("/hello"), Text(Nil), processId, processCode, LabelCache())))

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
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getSubmitPageContext(pec, NoError)
        .returns(Right(PageContext(expectedPage, Seq.empty, None, sessionId, Some("/hello"), Text(Nil), processId, processCode, LabelCache())))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
        .returns(Future.successful(Left((DatabaseError, None))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return a BAD_REQUEST when submitting page and guidance determines invalid data" in new QuestionTest {
      MockGuidanceService
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getSubmitPageContext(pec, NoError)
        .returns(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels)))

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
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      MockGuidanceService
        .getSubmitPageContext(pec, NoError)
        .returns(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels)))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "return a BAD_REQUEST response if trying to submit a page where url not found in process" in new QuestionTest {
      MockGuidanceService
        .getSubmitEvaluationContext(processId, "/unknown", processId)
        .returns(Future.successful(Left((BadRequestError, None))))

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
        .getSubmitEvaluationContext(processId, standardPagePath, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getSubmitPageContext(pec, ValueMissingError)
        .returns(Right(PageContext(standardPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels)))

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
        .getSubmitEvaluationContext(processId, unknownPath, processId)
        .returns(Future.successful(Left((NotFoundError, None))))

      override val fakeRequest = FakeRequest("POST", unknownPath)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, unknownRelativePath)(fakeRequest)
      status(result) shouldBe Status.NOT_FOUND
    }

    "return a INTERNAL_SERVER_ERROR response if submitting to a Process containing errors is referenced" in new QuestionTest {

      MockGuidanceService
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Left((InvalidProcessError, None))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return a INTERNAL_SERVER_ERROR response if encountering a database error when submitting a page" in new QuestionTest {
      MockGuidanceService
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Left((DatabaseError, None))))
      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return a INTERNAL_SERVER_ERROR response if encountering non-terminating page when submitting to page" in new QuestionTest {
      val NtpError = Error(Error.ExecutionError, List(NonTerminatingPageError), Some(Scratch), Some("1"))
      MockGuidanceService
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Left((NtpError, None))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken

      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

  }

  trait InputTest extends MockGuidanceService with MockPageRenderer with TestBase {

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
      stubMessagesControllerComponents(),
      formProvider
    )

    val initialLabels = LabelCache()
    val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = renderPage(inputPage, initialLabels)
    val pec = PageEvaluationContext(
                inputPage,
                vStanzas,
                di,
                sessionId,
                Map("4" -> PageDesc("4", "/somewhere-else")),
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
        .returns(Future.successful(Right(PageContext(expectedPage, Seq.empty, None, sessionId, Some("/"), Text(Nil), processId, processCode, LabelCache()))))
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
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getSubmitPageContext(pec, ValueMissingError)
        .returns(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels)))

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
      override val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = renderPage(inputPage, initialLabels)
      override val pec = PageEvaluationContext(
                inputPage,
                vStanzas,
                di,
                sessionId,
                Map("4" -> PageDesc("4", "/somewhere-else")),
                Some("/hello"),
                Text(),
                processId,
                "hello",
                initialLabels,
                None,
                None
              )

      MockGuidanceService
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getSubmitPageContext(pec, ValueTypeError)
        .returns(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels)))

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
      override val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = renderPage(inputPage, initialLabels)
      override val pec = PageEvaluationContext(
                inputPage,
                vStanzas,
                di,
                sessionId,
                Map("4" -> PageDesc("4", "/somewhere-else")),
                Some("/hello"),
                Text(),
                processId,
                "hello",
                initialLabels,
                None,
                None
              )

      MockGuidanceService
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .submitPage(pec, path, "150AA", "150AA")
        .returns(Future.successful(Right((None, pec.labels))))

      MockGuidanceService
        .getSubmitPageContext(pec, ValueTypeError)
        .returns(Right(PageContext(expectedPage, Seq.empty, None, sessionId, Some("/hello"), Text(Nil), processId, processCode, LabelCache())))

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
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getSubmitPageContext(pec, NoError)
        .returns(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels)))

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
      override val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = renderPage(inputPage, initialLabels)
      override val pec = PageEvaluationContext(
        inputPage,
        vStanzas,
        di,
        sessionId,
        Map("4" -> PageDesc("4", "/guidance/ext90002/somewhere-else")),
        Some("/hello"),
        Text(),
        processId,
        "hello",
        initialLabels,
        Some("/guidance/ext90002/another-place"),
        None
      )

      MockGuidanceService
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getSubmitPageContext(pec, NoError)
        .returns(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels)))

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
      override val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = renderPage(inputPage, initialLabels)
      override val pec = PageEvaluationContext(
        inputPage,
        vStanzas,
        di,
        sessionId,
        Map("4" -> PageDesc("4", "/guidance/ext90002/somewhere-else")),
        Some("/hello"),
        Text(),
        processId,
        "hello",
        initialLabels,
        Some("/guidance/ext90002/somewhere-else"),
        None
      )

      MockGuidanceService
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getSubmitPageContext(pec, NoError)
        .returns(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels)))

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
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getSubmitPageContext(pec, NoError)
        .returns(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels)))

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
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Left((InvalidProcessError, None))))
      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return a NOT_FOUND response if trying to submit to a non-existent page" in new QuestionTest {
      MockGuidanceService
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Left((NotFoundError, None))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "/guidance/hello")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.NOT_FOUND
    }

    "return a InternalServerError response when an unexpected error returned from service call" in new QuestionTest {

      MockGuidanceService
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Left((DatabaseError, None))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "/guidance/hello")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return a BAD_REQUEST response when a bad request error returned from service call" in new QuestionTest {
      MockGuidanceService
        .getSubmitEvaluationContext(processId, path, processId)
        .returns(Future.successful(Left((BadRequestError, None))))

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
        .getSubmitEvaluationContext(processId, standardPagePath, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getSubmitPageContext(pec, ValueMissingError)
        .returns(Right(PageContext(standardPage, vStanzas, di, sessionId, Some("/hello"), Text(Nil), processId, processCode, initialLabels)))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, relativeStdPath)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }
  }

  trait ProcessTest extends MockGuidanceService with TestBase {
    lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")

    lazy val target =
      new GuidanceController(
        MockAppConfig,
        fakeSessionIdAction,
        errorHandler,
        view,
        formView,
        mockGuidanceService,
        stubMessagesControllerComponents(),
        formProvider
      )
  }

  "Accessing a page from a passphrase process" should {
    trait Test extends MockGuidanceService with MockSessionService with MockGuidanceConnector with TestBase {
      lazy val fakeRequest = FakeRequest(GET, path).withSession(SessionKeys.sessionId -> processId).withCSRFToken

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents(),
          formProvider
        )

      val session: GuidanceSession =
        GuidanceSession(emptyProcess,Map("/start" -> "0"),Map(),Nil,Map(),pageMap,List("1","2"), None,None, Published, Nil)
    }

    "Return SEE_OTHER from getPage as a result trying to access valid page illegal in the current context, redirect to start" in new Test {
      override val session: GuidanceSession = GuidanceSession(emptyProcess,Map("/start" -> "0"),Map(),Nil,Map(),pageMap,Nil, None,None, Published, Nil)

      MockGuidanceService
        .getPageContext(processCode, path, false, processId)
        .returns(Future.successful(Left((ForbiddenError, None))))

      MockGuidanceService
        .getCurrentGuidanceSession(processCode)(processId)
        .returns(Future.successful(Right(session)))

      lazy val result = target.getPage(processCode, relativePath, None)(fakeRequest)

      status(result) shouldBe Status.SEE_OTHER
    }

    "Return SEE_OTHER from getPage as a result trying to access valid page illegal in the current context, redirect to current page" in new Test {
      override val session: GuidanceSession = GuidanceSession(emptyProcess,Map("/start" -> "0"),Map(),Nil,Map(),pageMap,List("1"), None,None, Published, Nil)

      MockGuidanceService
        .getPageContext(processCode, path, false, processId)
        .returns(Future.successful(Left((ForbiddenError, None))))

      MockGuidanceService
        .getCurrentGuidanceSession(processCode)(processId)
        .returns(Future.successful(Right(session)))

      lazy val result = target.getPage(processCode, relativePath, None)(fakeRequest)

      status(result) shouldBe Status.SEE_OTHER
    }

    "Return SEE_OTHER from getPage as a result trying to access valid page illegal in the current context when session not found" in new Test {
      MockGuidanceService
        .getPageContext(processCode, path, false, processId)
        .returns(Future.successful(Left((ForbiddenError, None))))

      MockGuidanceService
        .getCurrentGuidanceSession(processCode)(processId)
        .returns(Future.successful(Left((NotFoundError, None))))

      lazy val result = target.getPage(processCode, relativePath, None)(fakeRequest)

      status(result) shouldBe Status.SEE_OTHER
    }

    "Return SEE_OTHER from a getPage() as a result of an Authentication error when non authenticated" in new Test {
      MockGuidanceService
        .getPageContext(processCode, path, false, processId)
        .returns(Future.successful(Left((AuthenticationError, None))))

      lazy val result = target.getPage(processCode, relativePath, None)(fakeRequest)

      status(result) shouldBe Status.SEE_OTHER
    }

    "Return SEE_OTHER from a submit()) as a result of an Authentication error when non authenticated" in new Test {
      MockGuidanceService
        .getSubmitEvaluationContext(processCode, path, processId)
        .returns(Future.successful(Left((AuthenticationError, None))))

      lazy val result = target.submitPage(processCode, relativePath)(fakeRequest)

      status(result) shouldBe Status.SEE_OTHER
    }

    "Redirect to the start of a process when an expectation failed error is returned" in new Test {
      lazy val fakeRequestNoSessionId = FakeRequest(GET, path).withCSRFToken

      lazy val result = target.submitPage(processId, relativePath)(fakeRequestNoSessionId)

      status(result) shouldBe Status.SEE_OTHER
    }
  }

  "Calling a valid URL path for a page in a process" should {

    trait Test extends MockGuidanceService with TestBase {
      lazy val fakeRequest = FakeRequest(GET, path).withSession(SessionKeys.sessionId -> processId).withCSRFToken
      val ctx = PageContext(standardPage, Seq.empty, None, sessionId, Some("/hello"), Text(Nil), processId, processCode, LabelCache())
      MockGuidanceService
        .getPageContext(processCode, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(ctx)))

      MockGuidanceService
        .savePageState(ctx)
        .returns(Future.successful(Right(())))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents(),
          formProvider
        )
      lazy val result = target.getPage(processCode, relativePath, None)(fakeRequest)
    }

    "return a success response" in new Test {
      status(result) shouldBe Status.OK
    }

    "be a HTML response" in new Test {
      contentType(result) shouldBe Some("text/html")
    }

  }

  "Calling a valid URL path for a non-terminating page in a process" should {

    trait Test extends MockGuidanceService with TestBase {
      lazy val fakeRequest = FakeRequest(GET, path).withSession(SessionKeys.sessionId -> processId).withCSRFToken
      val NtpError = Error(Error.ExecutionError, List(NonTerminatingPageError), Some(Scratch), Some("1"))
      MockGuidanceService
        .getPageContext(processCode, path, previousPageByLink = false, processId)
        .returns(Future.successful(Left((NtpError, None))))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents(),
          formProvider
        )
      lazy val result = target.getPage(processCode, relativePath, None)(fakeRequest)
    }

    "return a success response" in new Test {
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "be a HTML response" in new Test {
      contentType(result) shouldBe Some("text/html")
    }

  }

  "Calling a valid URL path for a page and encountering a database error" should {

    trait Test extends MockGuidanceService with TestBase {
      lazy val fakeRequest = FakeRequest(GET, path).withSession(SessionKeys.sessionId -> processId).withCSRFToken

      MockGuidanceService
        .getPageContext(processCode, path, previousPageByLink = false, processId)
        .returns(Future.successful(Left((DatabaseError, None))))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents(),
          formProvider
        )
      lazy val result = target.getPage(processCode, relativePath, None)(fakeRequest)
    }

    "return an INTERNAL_SERVER_ERROR response" in new Test {
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "be a HTML response" in new Test {
      contentType(result) shouldBe Some("text/html")
    }

  }

  "Calling a valid URL path for a page with no sessionId" should {

    trait Test extends MockGuidanceService with TestBase {
      lazy val fakeRequest = FakeRequest(GET, path)

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents(),
          formProvider
        )

    }

    "return a SEE_OTHER to SessionBlocked controller response" in new Test {
      lazy val result = target.getPage(processCode, relativePath, None, Some("1"))(fakeRequest)

      status(result) shouldBe Status.SEE_OTHER

      redirectLocation(result) shouldBe Some("/guidance/testExample/session-blocked")
    }

    "return a SEE_OTHER to SessionBlocked controller with Cy language url param" in new Test {
      lazy val result = target.getPage(processCode, relativePath, None, Some("1"), Some("cy"))(fakeRequest)

      status(result) shouldBe Status.SEE_OTHER

      redirectLocation(result) shouldBe Some("/guidance/testExample/session-blocked?lang=cy")
    }

  }

  "Calling a valid URL path for a page and encountering a TransactionFault error" should {
    trait Test extends MockGuidanceService with TestBase {
      lazy val fakeRequest = FakeRequest(GET, path).withSession(SessionKeys.sessionId -> processId).withCSRFToken
      val ctx = PageContext(standardPage, Seq.empty, None, sessionId, Some("/hello"), Text(Nil), processId, processCode, LabelCache())

      MockGuidanceService
        .getPageContext(processCode, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(ctx)))

      MockGuidanceService
        .savePageState(ctx)
        .returns(Future.successful(Left(TransactionFaultError)))

      MockGuidanceService
        .getCurrentGuidanceSession(processCode)(processId)
        .returns(Future.successful(Right(GuidanceSession(emptyProcess,Map("/start" -> "0"),Map(),Nil,Map(),Map(),List("1"), None,None, Published, Nil))))


      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents(),
          formProvider
        )
      lazy val result = target.getPage(processCode, relativePath, None)(fakeRequest)
    }

    "return an redirect response" in new Test {
      redirectLocation(result).isDefined shouldBe true
    }

  }


  "Calling a valid URL path for a page and encountering a database error when saving labels" should {

    trait Test extends MockGuidanceService with TestBase {
      lazy val fakeRequest = FakeRequest(GET, path).withSession(SessionKeys.sessionId -> processId).withCSRFToken

      val ctx = PageContext(standardPage, Seq.empty, None, sessionId, Some("/hello"), Text(Nil), processId, processCode, LabelCache())

      MockGuidanceService
        .getPageContext(processCode, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(ctx)))

      MockGuidanceService
        .savePageState(ctx)
        .returns(Future.successful(Left(DatabaseError)))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents(),
          formProvider
        )
      lazy val result = target.getPage(processCode, relativePath, None)(fakeRequest)
    }

    "return an INTERNAL_SERVER_ERROR response" in new Test {
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "be a HTML response" in new Test {
      contentType(result) shouldBe Some("text/html")
    }

  }

  "Calling unknown URL path for a page in a process" should {

    trait Test extends MockGuidanceService with TestBase {
      val unknownPath = "/BlahBlah"
      lazy val fakeRequest = FakeRequest(GET, unknownPath).withSession(SessionKeys.sessionId -> processId).withCSRFToken

      MockGuidanceService
        .getPageContext(processCode, unknownPath, previousPageByLink = false, processId)
        .returns(Future.successful(Left((NotFoundError, None))))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents(),
          formProvider
        )
      lazy val result = target.getPage(processCode, unknownPath.drop(1), None)(fakeRequest)
    }

    "return a success response" in new Test {
      status(result) shouldBe Status.NOT_FOUND
    }

    "be a HTML response" in new Test {
      contentType(result) shouldBe Some("text/html")
    }

  }

  "Calling any valid process URL in a process with an invalid session" should {

    trait Test extends MockGuidanceService with TestBase {
      lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

      MockGuidanceService
        .getPageContext(processCode, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, Seq.empty, None, processId, Some("/hello"), Text(Nil), processId, processCode, LabelCache()))))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents(),
          formProvider
        )
      lazy val result = target.getPage("otherProcessCode", "/path", None)(fakeRequest)
    }

    "return a redirect response" in new Test {

      status(result) shouldBe Status.SEE_OTHER

      redirectLocation(result).fold(fail("Should redirect to guidance entry point")){url =>
        url shouldBe s"/guidance/otherProcessCode?$RedirectWhenNoSessionUrlParam"
      }
    }

  }

  "Calling getPage and encountering a transaction fault irrespective of request processCode" should {

    "Redirect to the currrent page of current session when processCode is common" in new TestWithRealGuidanceService with ProcessJson {

      val process = prototypeJson.as[Process]
      val session = GuidanceSession(Session(SessionKey(processId, process.meta.processCode), Some(Published), process.meta.id, Map(), Nil, Map(), Map(),
        List(RawPageHistory("start", Nil, Nil)), Nil, None, Instant.now, process.meta.lastUpdate, process.meta.timescalesVersion, process.meta.ratesVersion), process, Map(), List(PageHistory(s"${process.meta.processCode}$path",Nil,Nil)))

      MockSessionService
        .get(sessionId, process.meta.processCode, requestId)
        .returns(Future.successful(Left(TransactionFaultError)))
      MockSessionService
        .getNoUpdate(sessionId, process.meta.processCode)
        .returns(Future.successful(Right(session)))

      val result = target.getPage(process.meta.processCode, path.drop(1), None)(fakeRequest)

      status(result) shouldBe Status.SEE_OTHER

      redirectLocation(result).fold(fail("Should redirect to guidance entry point")){url =>
        url shouldBe s"/guidance/${process.meta.processCode}$path"
      }
    }

    "Redirect to the start of current session when no pageUrl in session" in new TestWithRealGuidanceService with ProcessJson {

      val process = prototypeJson.as[Process]

      MockSessionService
        .get(sessionId, processCode, requestId)
        .returns(Future.successful(Left(TransactionFaultError)))

      MockSessionService
        .getNoUpdate(sessionId, processCode)
        .returns(Future.successful(Right(
          GuidanceSession(Session(SessionKey(processId, process.meta.processCode), Some(Published), process.meta.id, Map(), Nil, Map(), Map(),
                  List(), Nil, None, Instant.now, process.meta.lastUpdate, process.meta.timescalesVersion, process.meta.ratesVersion), process, Map(), List())
        )))

      val result = target.getPage(processCode, path.drop(1), None)(fakeRequest)

      status(result) shouldBe Status.SEE_OTHER

      redirectLocation(result).fold(fail("Should redirect to guidance entry point")){url =>
        url shouldBe s"/guidance/${process.meta.processCode}"
      }
    }

  }

  "Accessing a valid process URL in a process when session id exists, but either no session exists or belongs to another process" should {

    trait Test extends MockGuidanceService with MockSessionService with MockGuidanceConnector with TestBase {
      override lazy val sessionId = s"session-${java.util.UUID.randomUUID().toString}"
      lazy val fakeRequest = FakeRequest(GET, "/start").withSession(SessionKeys.sessionId -> sessionId).withFormUrlEncodedBody().withCSRFToken

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents(),
          formProvider
        )
    }

    "return a redirect response to beginning of process" in new Test {
      MockGuidanceService
        .getPageContext("otherProcessCode", path, previousPageByLink = false, sessionId)
        .returns(Future.successful(Left((SessionNotFoundError, None))))

      lazy val result = target.getPage("otherProcessCode", path.drop(1), None)(fakeRequest)

      status(result) shouldBe Status.SEE_OTHER

      redirectLocation(result).fold(fail("Should redirect to guidance entry point")){url =>
        url shouldBe s"/guidance/otherProcessCode"
      }
    }

  }

  "Calling a non-existing URL path for a page in a process" should {

    trait Test extends MockGuidanceService with TestBase {
      val unknownPath = "unknown/route"
      lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] =
        FakeRequest().withSession(SessionKeys.sessionId -> processId)

      MockGuidanceService
        .getPageContext(processId, "/" + unknownPath, previousPageByLink = false, processId)
        .returns(Future.successful(Left((NotFoundError, None))))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents(),
          formProvider
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
    trait DateInputTest extends MockGuidanceService with TestBase {

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
        stubMessagesControllerComponents(),
        formProvider
      )

      val initialLabels = LabelCache()
      val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = renderPage(dateInputPage, initialLabels)
      val pec = PageEvaluationContext(
        dateInputPage,
        vStanzas,
        di,
        sessionId,
        Map("4" -> PageDesc("4", "/somewhere-else")),
        Some("/hello"),
        Text(),
        processId,
        "hello",
        initialLabels,
        None,
        None
      )

      val validDateAnswer: String = "10/10/2020"
      val invalidDateAnswer: String = "xx/10/2012"
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
          .returns(Future.successful(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels, None, Some(validDateAnswer)))))

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
          .getSubmitEvaluationContext(processId, path, processId)
          .returns(Future.successful(Right(pec)))

        MockGuidanceService
          .submitPage(pec, path, validDateAnswer, validDateAnswer)
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
            .getSubmitEvaluationContext(processId, path, processId)
            .returns(Future.successful(Right(pec)))

          MockGuidanceService
            .getSubmitPageContext(pec, ValueMissingGroupError(List("label.year"))) // Use message key as message substitution isn't working
            .returns(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels)))

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
            .getSubmitEvaluationContext(processId, path, processId)
            .returns(Future.successful(Right(pec)))

          MockGuidanceService
            .getSubmitPageContext(pec, ValueTypeGroupError(List("label.day"),List("day")))
            .returns(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels)))

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
            Map("4" -> PageDesc("4", "/somewhere-else")),
            Some("/hello"),
            Text(),
            processId,
            "hello",
            initialLabels,
            None,
            None
          )

          MockGuidanceService
            .getSubmitEvaluationContext(processId, path, processId)
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

    trait SequenceInputTest extends MockGuidanceService with TestBase {

      override lazy val expectedPage: ui.Page = FormPage(
        path,
        ui.Sequence(
          Text("Select a working day of the week"),
          None,
          Seq(
            SequenceAnswer(Text("Monday"), None),
            SequenceAnswer(Text("Tuesday"), None),
            SequenceAnswer(Text("Wednesday"), None),
            SequenceAnswer(Text("Thursday"), None),
            SequenceAnswer(Text("Friday"), None)
          ),
          None,
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
        stubMessagesControllerComponents(),
        formProvider
      )

      val initialLabels: Labels = LabelCache()
      val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = renderPage(nonExclusiveSequenceInputPage, initialLabels)

      val pec: PageEvaluationContext = PageEvaluationContext(
        nonExclusiveSequenceInputPage,
        vStanzas,
        di,
        sessionId,
        Map("4" -> PageDesc("4", "/somewhere-else")),
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
          .getSubmitEvaluationContext(processId, path, processId)
          .returns(Future.successful(Right(pec)))

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
            .getSubmitEvaluationContext(processId, path, processId)
            .returns(Future.successful(Right(pec)))

          MockGuidanceService
            .getSubmitPageContext(pec, ValueMissingError)
            .returns(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels)))

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
            .getSubmitEvaluationContext(processId, path, processId)
            .returns(Future.successful(Right(pec)))

          MockGuidanceService
            .getSubmitPageContext(pec, ValueTypeError)
            .returns(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels)))

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

    trait ExclusiveSequenceInputTest extends MockGuidanceService with TestBase {

      override lazy val expectedPage: ui.Page = FormPage(
        path,
        ui.Sequence(
          Text("Select a holiday destination"),
          None,
          Seq(
            SequenceAnswer(Text("Europe"), None),
            SequenceAnswer(Text("Africa"), None),
            SequenceAnswer(Text("Americas"), None),
            SequenceAnswer(Text("Asia"), None)
          ),
          Some(SequenceAnswer(Text("Elsewhere"), Some(Text("Selecting this option will deselect all the other checkboxes")))),
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
        stubMessagesControllerComponents(),
        formProvider
      )

      val initialLabels: Labels = LabelCache()
      val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = renderPage(exclusiveSequenceInputPage, initialLabels)

      val pec: PageEvaluationContext = PageEvaluationContext(
        exclusiveSequenceInputPage,
        vStanzas,
        di,
        sessionId,
        Map("4" -> PageDesc("4", "/somewhere-else")),
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
          .getSubmitEvaluationContext(processId, path, processId)
          .returns(Future.successful(Right(pec)))

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
            .getSubmitEvaluationContext(processId, path, processId)
            .returns(Future.successful(Right(pec)))

          MockGuidanceService
            .getSubmitPageContext(pec, ValueMissingError)
            .returns(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels)))

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
            .getSubmitEvaluationContext(processId, path, processId)
            .returns(Future.successful(Right(pec)))

          MockGuidanceService
            .getSubmitPageContext(pec, ValueTypeError)
            .returns(Right(PageContext(expectedPage, vStanzas, di, sessionId, Some("/"), Text(Nil), processId, processCode, initialLabels)))

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

  "fromRuntimeError" should {
    "translate UnsupportedOperationError" in new TestBase {
      val report = fromRuntimeError(UnsupportedOperationError("AddOperation", Some("lvalue"), Some("rvalue"), "left", "right"), "stanzaId")
      report shouldBe "UnsupportedOperationError: Calculation stanza 'stanzaId' contains operation 'AddOperation' with arguments 'left' = lvalue, 'right' = rvalue."
    }

    "translate NonTerminatingPageError" in new TestBase {
      val report = fromRuntimeError(NonTerminatingPageError, "stanzaId")
      report shouldBe "NonTerminatingPageError: Infinite loop found on page containing stanza 'stanzaId'"
    }

    "translate UnsupportedUiPatternError" in new TestBase {
      val report = fromRuntimeError(UnsupportedUiPatternError, "stanzaId")
      report shouldBe "UnsupportedUiPatternError: Unrecognised RowStanza UI pattern including stanza \'stanzaId\'."

    }

    "translate ProgrammingError" in new TestBase {
      val report = fromRuntimeError(ProgrammingError("Something went wrong"), "stanzaId")
      report shouldBe "ProgrammingError: \'Something went wrong\' on page containing stanza \'stanzaId\'"

    }

  }

}
