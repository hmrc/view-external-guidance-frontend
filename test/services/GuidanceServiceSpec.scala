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

package services

import base.BaseSpec

import mocks.{MockAppConfig, MockGuidanceConnector, MockPageBuilder, MockPageRenderer, MockSessionRepository, MockUIBuilder}
import core.models.errors.{DatabaseError, NotFoundError, NonTerminatingPageError}
import core.models.ocelot.stanzas._
import core.models.ocelot.{Page, KeyedStanza, Process, SecuredProcess, ProcessJson, LabelCache, Labels, Phrase, Published}
import models.ui
import models.{PageDesc, PageNext, PageEvaluationContext}
import uk.gov.hmrc.http.{RequestId, HeaderCarrier}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import models.{GuidanceSession, PageContext}
import play.api.i18n.Lang
import play.api.i18n.MessagesApi
import play.api.inject.Injector
import repositories.{Session, SessionFSM, SessionKey, PageHistory}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import java.time.Instant

class GuidanceServiceSpec extends BaseSpec  with GuiceOneAppPerSuite {

  def injector: Injector = app.injector
  val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val rId: String = "71dcc4a3-9d19-47f5-ad97-74bb6c2a15c4"

  trait Test extends MockGuidanceConnector with MockSessionRepository with MockPageBuilder with MockPageRenderer with MockUIBuilder with ProcessJson {
    implicit val lang: Lang = Lang("en")
    implicit val headerCarrier: HeaderCarrier = HeaderCarrier(requestId = Some(RequestId(rId)))
    implicit val stanzaIdToUrl: Map[String, String] = Map[String, String]()

    def pageWithUrl(id: String, url: String) = Page(id, url, Seq(KeyedStanza("1", EndStanza)), Seq())

    val process: Process = validOnePageJson.as[Process]
    val processWithProcessCode = validOnePageProcessWithProcessCodeJson.as[Process]
    val fullProcess: Process = prototypeJson.as[Process]

    val firstPageUrl = "/first-page"
    val firstUiPage: ui.Page = ui.Page(firstPageUrl, Seq())

    val lastPageUrl = "/last-page"
    val lastUiPage: ui.Page = ui.Page(lastPageUrl, Seq())

    val pages: Seq[Page] = Seq(
      pageWithUrl(Process.StartStanzaId, firstPageUrl),
      pageWithUrl("1", "/page-1"),
      pageWithUrl("2", lastPageUrl)
    )

    val lastPage = pageWithUrl("2", lastPageUrl)

    val processId = "oct90001"
    val processCode = "CupOfTea"
    val uuid = "683d9aa0-2a0e-4e28-9ac8-65ce453d2730"
    val sessionRepoId = "683d9aa0-2a0e-4e28-9ac8-65ce453d2731"
    val requestId: Option[String] = Some(rId)

    val instructionStanza = InstructionStanza(3, Seq("3"), None, false)
    val questionStanza = Question(Phrase("Which?","Which?"), Seq(Phrase("yes","yes"),Phrase("no","no")), Seq("4","5"), None, false)
    val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                                        KeyedStanza("1", instructionStanza),
                                        KeyedStanza("3", questionStanza)
                                      )
    val page = Page("start", "/test-page", stanzas, Seq("4","5"))

    val standardPage = Page("start", "/test-page", stanzas.dropRight(1), Seq("4","5"))

    val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = renderPage(page, LabelCache())
    val pec = PageEvaluationContext(
                page,
                vStanzas,
                di,
                processId,
                Map("start" -> PageDesc("start", "/first-page"), "1" ->  PageDesc("1", "/page-1"), "2" ->  PageDesc("2", "/last-page")),
                Some("/hello"),
                ui.Text(),
                processId,
                "hello",
                labels,
                None,
                None
              )

    val standardPagePec = pec.copy(page = standardPage, dataInput = None, visualStanzas = Seq.empty)

    lazy val target = new GuidanceService(
      MockAppConfig,
      mockSessionRepository,
      mockPageBuilder,
      mockPageRenderer,
      new SecuredProcessBuilder(messagesApi),
      mockUIBuilder,
      new SessionFSM,
      messagesApi)

    def renderPage(p: Page, l: Labels): (Seq[VisualStanza], Labels, Option[DataInput]) = new PageRenderer(MockAppConfig).renderPage(p, l).fold(_ => fail, res => res)
  }

  "Calling saveLabels when there labels to save" should {

    "save updated labels" in new Test {
      val changedLabels = labels.update("LabelName", "New value")

      MockSessionRepository
        .updateAfterStandardPage(sessionRepoId, processCode, changedLabels, requestId)
        .returns(Future.successful(Right({})))

      private val result = target.savePageState(sessionRepoId, processCode, changedLabels)

      whenReady(result) {
        case Right(pc) => succeed
        case Left(err) => fail(s"sabveLabels returned error $err")
      }
    }
  }

  "Calling getPageContext with a PageEvaluationContext and ErrorStrategy" should {

    "retrieve a page for the process" in new Test {

      override val processCode = "cup-of-tea"

      MockSessionRepository
        .get(sessionRepoId, processCode, requestId)
        .returns(Future.successful(Right(
          Session(SessionKey(sessionRepoId, processCode), Some(Published), process.meta.id, process, Map(), Nil, Map(), Map(page.url -> PageNext("2", Nil)), Map(), Nil, Nil, None, Instant.now)
        )))

      MockPageBuilder
        .buildPage("2", process)
        .returns(Right(page))

      MockPageRenderer
        .renderPage(page, labels)
        .returns(Right((page.stanzas.collect{case s: VisualStanza => s}, labels, None)))

      MockSessionRepository
        .updateAfterStandardPage(sessionRepoId, processCode, labels, requestId)
        .returns(Future.successful(Right({})))

      MockUIBuilder
        .buildPage(page.url, page.stanzas.collect{case s: VisualStanza => s}, NoError)
        .returns(ui.Page(page.url, Seq()))

      target.getSubmitPageContext(pec, NoError) match {
        case Right(pageCtx) => pageCtx.page.urlPath shouldBe page.url
        case Left(_) => fail
      }
    }

    "return an error when retrieving a non-terminating page" in new Test {

      override val processCode = "cup-of-tea"

      MockSessionRepository
        .get(sessionRepoId, processCode, requestId)
        .returns(Future.successful(Right(
          Session(SessionKey(sessionRepoId, processCode), Some(Published), process.meta.id, process, Map(), Nil, Map(), Map(page.url -> PageNext("2", Nil)), Map(), Nil, Nil, None, Instant.now)
        )))

      MockPageBuilder
        .buildPage("2", process)
        .returns(Right(page))

      MockPageRenderer
        .renderPage(page, labels)
        .returns(Left(NonTerminatingPageError))

      target.getSubmitPageContext(pec, NoError) match {
        case Left(err) if err == NonTerminatingPageError => succeed
        case Right(_) => fail
      }
    }
  }


  "Calling getPageContext with a valid URL" should {

    "retrieve a page for the process" in new Test {

      override val processCode = "cup-of-tea"

      MockSessionRepository
        .get(sessionRepoId, processCode, requestId)
        .returns(Future.successful(Right(
          Session(SessionKey(sessionRepoId, processCode), Some(Published), process.meta.id, process, Map(), Nil, Map(), Map(lastPageUrl -> PageNext("2", Nil)), Map(), Nil, Nil, None, Instant.now)
        )))

      MockPageBuilder
        .buildPage("2", process)
        .returns(Right(lastPage))

      MockPageRenderer
        .renderPage(lastPage, labels)
        .returns(Right((lastPage.stanzas.collect{case s: VisualStanza => s}, labels, None)))

      MockSessionRepository
        .updateAfterStandardPage(sessionRepoId, processCode, labels, requestId)
        .returns(Future.successful(Right({})))

      MockUIBuilder
        .buildPage(lastPageUrl, lastPage.stanzas.collect{case s: VisualStanza => s}, NoError)
        .returns(lastUiPage)

      MockSessionRepository
        .updateForNewPage(sessionRepoId, processCode, Some(List(PageHistory("cup-of-tea/last-page",Nil))), None, Nil, List("2", "start"), requestId)
        .returns(Future.successful(Right(())))

      private val result = target.getPageContext(processCode, lastPageUrl, previousPageByLink = false, sessionRepoId)

      whenReady(result) {
        case Right(pc) => pc.page.urlPath shouldBe lastPageUrl
        case Left(err) => fail(s"no PageContext found with error $err")
      }
    }
  }

    "Calling getPageContext when page is non-terminating" should {

    "retrieve a page for the process" in new Test {

      override val processCode = "cup-of-tea"

       MockSessionRepository
        .get(sessionRepoId, processCode, requestId)
        .returns(Future.successful(Right(
          Session(SessionKey(sessionRepoId, processCode), Some(Published), process.meta.id, process, Map(), Nil, Map(), Map(lastPageUrl -> PageNext("2", Nil)), Map(), Nil, Nil, None, Instant.now)
        )))

      MockSessionRepository
        .updateForNewPage(sessionRepoId, processCode, Some(List(PageHistory("cup-of-tea/last-page",Nil))), None, Nil, List("2", "start"), requestId)
        .returns(Future.successful(Right(())))

      MockPageBuilder
        .buildPage("2", process)
        .returns(Right(lastPage))

      MockPageRenderer
        .renderPage(lastPage, labels)
        .returns(Left(NonTerminatingPageError))

      private val result = target.getPageContext(processCode, lastPageUrl, previousPageByLink = false, sessionRepoId)

      whenReady(result) {
        case Left(err) if err == NonTerminatingPageError => succeed
        case Right(_) => fail
      }
    }
  }


  "Calling getPageContext against a previously answered Question page url" should {

    "retrieve a PageContext which includes the relevant answer" in new Test {
      override val processId: String = "ext90002"

      override val processCode = "tell-hmrc"

      MockSessionRepository
        .get(sessionRepoId, processCode, requestId)
        .returns(Future.successful(Right(
          Session(SessionKey(sessionRepoId, processCode), Some(Published), fullProcess.meta.id, fullProcess, Map(), Nil, Map(), Map(lastPageUrl -> PageNext("2")), Map(lastPageUrl -> "answer"), Nil, Nil, None, Instant.now)
        )))

      MockSessionRepository
        .updateForNewPage(sessionRepoId, processCode, Some(List(PageHistory("tell-hmrc/last-page",Nil))), None, Nil, List("2", "start"), requestId)
        .returns(Future.successful(Right(())))

      MockPageBuilder
        .buildPage("2", fullProcess)
        .returns(Right(lastPage))

      MockPageRenderer
        .renderPage(lastPage, labels)
        .returns(Right((lastPage.stanzas.collect{case s: VisualStanza => s}, labels, None)))

      MockSessionRepository
        .updateAfterStandardPage(sessionRepoId, processCode, labels, requestId)
        .returns(Future.successful(Right({})))

      MockUIBuilder
        .buildPage(lastPageUrl, lastPage.stanzas.collect{case s: VisualStanza => s}, NoError)
        .returns(lastUiPage)

      private val result = target.getPageContext(processCode, lastPageUrl, previousPageByLink = false, sessionRepoId)

      whenReady(result) { pageCtx =>
        pageCtx match {
          case Right(PageContext(_, _, _, _, _, _, _, _, _, _, Some(answer), _)) => succeed
          case Right(wrongContext) => fail(s"Previous answer missing from PageContext, $wrongContext")
          case Left(err) => fail(s"Previous answer missing from PageContext, $err")
        }
      }
    }
  }

  "Calling getPageContext with an invalid URL" should {

    "not retrieve a page from the process" in new Test {

      val url = "/scooby"
      override val processCode = "cup-of-tea"

      MockSessionRepository
        .get(processId, processCode, requestId)
        .returns(Future.successful(Right(
          Session(SessionKey(sessionRepoId, processCode), Some(Published), process.meta.id, process, Map(), Nil, Map(), Map(), Map(), Nil, Nil, None, Instant.now)
        )))

      MockPageBuilder
        .buildPage("2", process)
        .returns(Right(lastPage))

      private val result = target.getPageContext(processCode, url, previousPageByLink = false, processId)

      whenReady(result) {
        _ shouldBe Left(NotFoundError)
      }
    }
  }

  "Calling getById" should {

    "successfully retrieve a process context when the session data contains a single process" in new Test {

      val expectedGuidanceSession: GuidanceSession = GuidanceSession(process, Map(), Map(), Nil, Map(), Map(), Nil, None, None, Published)

      MockSessionRepository
        .getById(sessionRepoId, process.meta.processCode)
        .returns(Future.successful(Right(expectedGuidanceSession)))

      private val result = target.getCurrentGuidanceSession(process.meta.processCode)(sessionRepoId)

      whenReady(result) { session =>
        session shouldBe Right(expectedGuidanceSession)
      }
    }

    "return a not found error if the session data does not exist" in new Test {

      MockSessionRepository
        .getById(sessionRepoId, process.meta.processCode)
        .returns(Future.successful(Left(NotFoundError)))

      private val result = target.getCurrentGuidanceSession(process.meta.processCode)(sessionRepoId)

      whenReady(result) { err =>
        err shouldBe Left(NotFoundError)
      }
    }

    "return a database error if an error occurs retrieving the session data" in new Test {

      MockSessionRepository
        .getById(sessionRepoId, process.meta.processCode)
        .returns(Future.successful(Left(DatabaseError)))

      private val result = target.getCurrentGuidanceSession(process.meta.processCode)(sessionRepoId)

      whenReady(result) { err =>
        err shouldBe Left(DatabaseError)
      }

    }
  }

  "Calling getPageGuidanceSession" should {

    "When passed a url to the passphrase page should send no pageHistory url to the repository" in new Test {
      val expectedSession: Session = Session(SessionKey(sessionRepoId, process.meta.processCode), Some(Published), process.meta.id, process, Map(), Nil, Map(), Map(), Map(), Nil, Nil, None, Instant.now)

      MockSessionRepository
        .get(sessionRepoId, process.meta.processCode, requestId)
        .returns(Future.successful(Right(expectedSession)))

      target.getPageEvaluationContext(process.meta.processCode, s"/${SecuredProcess.SecuredProcessStartUrl}", false, sessionRepoId)
    }

    "When passed a url to a standard page should send pageHistory url to the repository" in new Test {
      val expectedSession: Session = Session(SessionKey(sessionRepoId, process.meta.processCode), Some(Published), process.meta.id, process, Map(), Nil, Map(), Map(), Map(), Nil, Nil, None, Instant.now)

      MockSessionRepository
        .get(sessionRepoId, process.meta.processCode, requestId)
        .returns(Future.successful(Right(expectedSession)))

      target.getPageEvaluationContext(process.meta.processCode, s"/start", false, sessionRepoId)
    }

  }

  "Calling submitPage" should {
    "Return None if page submission evaluation determines no valid next page" in new Test {
      MockPageRenderer
        .renderPagePostSubmit(page, LabelCache(), "yes")
        .returns(Right((None, LabelCache())))

      MockSessionRepository
        .updateAfterFormSubmission(processId, processCode, "/test-page", "yes", labels, Nil, requestId)
        .returns(Future.successful(Right({})))

      target.submitPage(pec, "/test-page", "yes", "yes").map{
        case Left(err) => fail
        case Right((nxt, lbls)) if nxt.isEmpty => succeed
        case Right(_) => fail
      }
    }

    "Return the id of the page to follow" in new Test {
      MockPageRenderer
        .renderPagePostSubmit(page, LabelCache(), "yes")
        .returns(Right((Some("2"), LabelCache())))

      MockSessionRepository
        .updateAfterFormSubmission(processId, pec.processCode, "/last-page", "yes", labels, List("2"), requestId)
        .returns(Future.successful(Right({})))

      target.submitPage(pec, "/last-page", "yes", "yes").map{
        case Left(err) => fail
        case Right((Some("4"), _)) => succeed
        case Right(_) => fail
      }
    }

    "Return error if page submission evaluation finds a non-terminating page" in new Test {
      MockPageRenderer
        .renderPagePostSubmit(page, LabelCache(), "yes")
        .returns(Left(NonTerminatingPageError))

      MockSessionRepository
        .updateAfterFormSubmission(processId, processCode, "/test-page", "yes", labels, Nil, requestId)
        .returns(Future.successful(Right({})))

      target.submitPage(pec, "/test-page", "yes", "yes").map{
        case Left(err) if err == NonTerminatingPageError => succeed
        case _ => fail
      }
    }

  }

  "Calling saveLabels" should {
    "Success when labels saved successfully" in new Test {
      MockSessionRepository
        .updateAfterStandardPage(processId, processCode, labels, requestId)
        .returns(Future.successful(Right({})))

      target.savePageState(processId, processCode, LabelCache()).map{
        case Right(x) if x == Unit => succeed
        case Left(_) => fail()
      }
    }

    "An error when labels not saved successfully" in new Test {
      MockSessionRepository
        .updateAfterStandardPage(processId, processCode, labels, requestId)
        .returns(Future.successful(Left(DatabaseError)))

      target.savePageState(processId, processCode, LabelCache()).map{
        case Left(err) if err == DatabaseError => succeed
        case _ => fail()
      }
    }
  }

}
