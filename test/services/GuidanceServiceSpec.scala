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

package services

import base.BaseSpec

import mocks.{MockAppConfig, MockGuidanceConnector, MockPageBuilder, MockPageRenderer, MockSessionRepository, MockUIBuilder}
import core.models.errors.{DatabaseError, NotFoundError, NonTerminatingPageError}
import core.models.ocelot.stanzas._
import core.models.ocelot.{Page, KeyedStanza, Process, SecuredProcess, ProcessJson, LabelCache, Labels, Phrase}
import models.ui
import models.{PageDesc, PageNext, PageEvaluationContext}
import uk.gov.hmrc.http.HeaderCarrier
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import models.{ProcessContext, PageContext}
import play.api.i18n.Lang
import play.api.i18n.MessagesApi
import play.api.inject.Injector
import org.scalatestplus.play.guice.GuiceOneAppPerSuite

class GuidanceServiceSpec extends BaseSpec  with GuiceOneAppPerSuite {

  def injector: Injector = app.injector
  val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]

  trait Test extends MockGuidanceConnector with MockSessionRepository with MockPageBuilder with MockPageRenderer with MockUIBuilder with ProcessJson {
    implicit val lang: Lang = Lang("en")
    implicit val headerCarrier: HeaderCarrier = HeaderCarrier()
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
      mockGuidanceConnector,
      mockSessionRepository,
      mockPageBuilder,
      mockPageRenderer,
      new SecuredProcessBuilder(messagesApi),
      mockUIBuilder,
      messagesApi)

    def renderPage(p: Page, l: Labels): (Seq[VisualStanza], Labels, Option[DataInput]) = new PageRenderer(MockAppConfig).renderPage(p, l).fold(_ => fail, res => res)
  }

  "Calling saveLabels when there labels to save" should {

    "save updated labels" in new Test {
      val changedLabels = labels.update("LabelName", "New value")

      MockSessionRepository
        .savePageState(sessionRepoId, changedLabels)
        .returns(Future.successful(Right({})))

      private val result = target.savePageState(sessionRepoId, changedLabels)

      whenReady(result) {
        case Right(pc) => succeed
        case Left(err) => fail(s"sabveLabels returned error $err")
      }
    }
  }

  "Calling validateUserResponse" should {
    "Use page DataInput stanza to validate valid response" in new Test {

      MockPageRenderer
        .renderPage(pec.page, pec.labels)
        .returns(Right((pec.visualStanzas, pec.labels, pec.dataInput)))

      target.validateUserResponse(pec, "0") match {
        case Some("0") => succeed
        case _ => fail
      }
    }

    "Use page DataInput stanza to validate invalid response" in new Test {

      MockPageRenderer
        .renderPage(pec.page, pec.labels)
        .returns(Right(renderPage(pec.page, pec.labels)))

      target.validateUserResponse(pec, "hello") match {
        case None => succeed
        case _ => fail
      }
    }

    "return None of used on a page with no DataInput stanza" in new Test {

      MockPageRenderer
        .renderPage(standardPagePec.page, standardPagePec.labels)
        .returns(Right(renderPage(standardPagePec.page, standardPagePec.labels)))

      target.validateUserResponse(standardPagePec, "hello") match {
        case None => succeed
        case _ => fail
      }
    }

  }

  "Calling getPageContext with a PageEvaluationContext and ErrorStrategy" should {

    "retrieve a page for the process" in new Test {

      override val processCode = "cup-of-tea"

      MockSessionRepository
        .getUpdateForGET(sessionRepoId, processCode, Some(s"$processCode$${page.url}"), previousPageByLink = false)
        .returns(Future.successful(Right(ProcessContext(process, Map(), Map(), Nil, Map(), Map(page.url -> PageNext("2", Nil)), Nil, None, None))))

      MockPageBuilder
        .buildPage("2", process)
        .returns(Right(page))

      MockPageRenderer
        .renderPage(page, labels)
        .returns(Right((page.stanzas.collect{case s: VisualStanza => s}, labels, None)))

      MockSessionRepository
        .savePageState(sessionRepoId, labels)
        .returns(Future.successful(Right({})))

      MockUIBuilder
        .buildPage(page.url, page.stanzas.collect{case s: VisualStanza => s}, NoError)
        .returns(ui.Page(page.url, Seq()))

      target.getPageContext(pec, NoError) match {
        case Right(pageCtx) => pageCtx.page.urlPath shouldBe page.url
        case Left(_) => fail
      }
    }

    "return an error when retrieving a non-terminating page" in new Test {

      override val processCode = "cup-of-tea"

      MockSessionRepository
        .getUpdateForGET(sessionRepoId, processCode, Some(s"$processCode$${page.url}"), previousPageByLink = false)
        .returns(Future.successful(Right(ProcessContext(process, Map(), Map(), Nil, Map(), Map(page.url -> PageNext("2", Nil)), Nil, None, None))))

      MockPageBuilder
        .buildPage("2", process)
        .returns(Right(page))

      MockPageRenderer
        .renderPage(page, labels)
        .returns(Left(NonTerminatingPageError))

      target.getPageContext(pec, NoError) match {
        case Left(err) if err == NonTerminatingPageError => succeed
        case Right(_) => fail
      }
    }
  }


  "Calling getPageContext with a valid URL" should {

    "retrieve a page for the process" in new Test {

      override val processCode = "cup-of-tea"

      MockSessionRepository
        .getUpdateForGET(sessionRepoId, processCode, Some(s"$processCode$lastPageUrl"), previousPageByLink = false)
        .returns(Future.successful(Right(ProcessContext(process, Map(), Map(), Nil, Map(), Map(lastPageUrl -> PageNext("2")), Nil, None, None))))

      MockPageBuilder
        .buildPage("2", process)
        .returns(Right(lastPage))

      MockPageRenderer
        .renderPage(lastPage, labels)
        .returns(Right((lastPage.stanzas.collect{case s: VisualStanza => s}, labels, None)))

      MockSessionRepository
        .savePageState(sessionRepoId, labels)
        .returns(Future.successful(Right({})))

      MockUIBuilder
        .buildPage(lastPageUrl, lastPage.stanzas.collect{case s: VisualStanza => s}, NoError)
        .returns(lastUiPage)

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
        .getUpdateForGET(sessionRepoId, processCode, Some(s"$processCode$lastPageUrl"), previousPageByLink = false)
        .returns(Future.successful(Right(ProcessContext(process, Map(), Map(), Nil, Map(), Map(lastPageUrl -> PageNext("2")), Nil, None, None))))

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
        .getUpdateForGET(sessionRepoId, processCode, Some(s"$processCode$lastPageUrl"), previousPageByLink = false)
        .returns(Future.successful(Right(ProcessContext(fullProcess, Map(lastPageUrl -> "answer"), Map(), Nil, Map(), Map(lastPageUrl -> PageNext("2")), Nil, None, None))))

      MockPageBuilder
        .buildPage("2", fullProcess)
        .returns(Right(lastPage))

      MockPageRenderer
        .renderPage(lastPage, labels)
        .returns(Right((lastPage.stanzas.collect{case s: VisualStanza => s}, labels, None)))

      MockSessionRepository
        .savePageState(sessionRepoId, labels)
        .returns(Future.successful(Right({})))

      MockUIBuilder
        .buildPage(lastPageUrl, lastPage.stanzas.collect{case s: VisualStanza => s}, NoError)
        .returns(lastUiPage)

      private val result = target.getPageContext(processCode, lastPageUrl, previousPageByLink = false, sessionRepoId)

      whenReady(result) { pageCtx =>
        pageCtx match {
          case Right(PageContext(_, _, _, _, _, _, _, _, _, _, Some(answer))) => succeed
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
        .getUpdateForGET(processId, processCode, Some(s"$processCode$url"), previousPageByLink = false)
        .returns(Future.successful(Right(ProcessContext(process, Map(), Map(), Nil, Map(), Map(), Nil, None, None))))

      MockPageBuilder
        .buildPage("2", process)
        .returns(Right(lastPage))

      private val result = target.getPageContext(processCode, url, previousPageByLink = false, processId)

      whenReady(result) {
        _ shouldBe Left(NotFoundError)
      }
    }
  }

  "Calling retrieveAndCacheScratch" should {

    "retrieve the url of the start page for the scratch process" in new Test {

      MockGuidanceConnector
        .scratchProcess(uuid)
        .returns(Future.successful(Right(process)))

      val processWithUpdatedId = process.copy(meta = process.meta.copy( id = uuid))

      MockSessionRepository
        .set(uuid, processWithUpdatedId, Map("/first-page" -> PageNext("start"), "/page-1" -> PageNext("1"), "/last-page" -> PageNext("2")))
        .returns(Future.successful(Right(())))

      MockPageBuilder
        .pages(processWithUpdatedId)
        .returns(Right(pages))

      private val result = target.retrieveAndCacheScratch(uuid, uuid)

      whenReady(result) { url =>
        url shouldBe Right((firstPageUrl,"cup-of-tea"))
      }
    }
  }

  "Calling retrieveAndCachePublished" should {

    "retrieve the url of the start page for the nominated published process" in new Test {

      MockGuidanceConnector
        .publishedProcess(processId)
        .returns(Future.successful(Right(processWithProcessCode)))

      MockSessionRepository
        .set(sessionRepoId, processWithProcessCode,Map("/first-page" -> PageNext("start"), "/page-1" -> PageNext("1"), "/last-page" -> PageNext("2")))
        .returns(Future.successful(Right(())))

      MockPageBuilder
        .pages(processWithProcessCode)
        .returns(Right(pages))

      private val result = target.retrieveAndCachePublished(processId, sessionRepoId)

      whenReady(result) { url =>
        url shouldBe Right((firstPageUrl, processCode))
      }
    }
  }

  "Calling retrieveAndCacheApproval" should {

    "retrieve the url of the start page for the nominated published process" in new Test {

      MockGuidanceConnector
        .approvalProcess(processId)
        .returns(Future.successful(Right(processWithProcessCode)))

      MockSessionRepository
        .set(sessionRepoId, processWithProcessCode, Map("/first-page" -> PageNext("start"), "/page-1" -> PageNext("1"), "/last-page" -> PageNext("2")))
        .returns(Future.successful(Right(())))

      MockPageBuilder
        .pages(processWithProcessCode)
        .returns(Right(pages))

      private val result = target.retrieveAndCacheApproval(processId, sessionRepoId)

      whenReady(result) { url =>
        url shouldBe Right((firstPageUrl, processCode))
      }
    }
  }

  "Calling getProcessContext(key: String)" should {

    "successfully retrieve a process context when the session data contains a single process" in new Test {

      val expectedProcessContext: ProcessContext = ProcessContext(process, Map(), Map(), Nil, Map(), Map(), Nil, None, None)

      MockSessionRepository
        .getNoUpdate(sessionRepoId)
        .returns(Future.successful(Right(expectedProcessContext)))

      private val result = target.getProcessContext(sessionRepoId)

      whenReady(result) { processContext =>
        processContext shouldBe Right(expectedProcessContext)
      }
    }

    "return a not found error if the session data does not exist" in new Test {

      MockSessionRepository
        .getNoUpdate(sessionRepoId)
        .returns(Future.successful(Left(NotFoundError)))

      private val result = target.getProcessContext(sessionRepoId)

      whenReady(result) { err =>
        err shouldBe Left(NotFoundError)
      }
    }

    "return a database error if an error occurs retrieving the session data" in new Test {

      MockSessionRepository
        .getNoUpdate(sessionRepoId)
        .returns(Future.successful(Left(DatabaseError)))

      private val result = target.getProcessContext(sessionRepoId)

      whenReady(result) { err =>
        err shouldBe Left(DatabaseError)
      }

    }
  }

  "Calling getProcessContext full" should {

    "When passed a url to the passphrase page should send no pageHistory url to the repository" in new Test {
      val expectedProcessContext: ProcessContext = ProcessContext(process, Map(), Map(), Nil, Map(), Map(), Nil, None, None)

      MockSessionRepository
        .getUpdateForGET(sessionRepoId, process.meta.processCode, None, false)
        .returns(Future.successful(Right(expectedProcessContext)))

      private val result = target.getProcessContext(sessionRepoId, process.meta.processCode, s"/${SecuredProcess.SecuredProcessStartUrl}", false)

      whenReady(result) { err =>
        err shouldBe Right(expectedProcessContext)
      }

    }
  }

  "Calling submitPage" should {
    "Return None if page submission evaluation determines no valid next page" in new Test {
      MockPageRenderer
        .renderPagePostSubmit(page, LabelCache(), "yes")
        .returns(Right((None, LabelCache())))

      MockSessionRepository
        .saveFormPageState(processId,"/test-page", "yes", labels, Nil)
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
        .saveFormPageState(processId,"/last-page", "yes", labels, List("2"))
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
        .saveFormPageState(processId,"/test-page", "yes", labels, Nil)
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
        .savePageState(processId, labels)
        .returns(Future.successful(Right({})))

      target.savePageState(processId, LabelCache()).map{
        case Right(x) if x == Unit => succeed
        case Left(_) => fail()
      }
    }

    "An error when labels not saved successfully" in new Test {
      MockSessionRepository
        .savePageState(processId, labels)
        .returns(Future.successful(Left(DatabaseError)))

      target.savePageState(processId, LabelCache()).map{
        case Left(err) if err == DatabaseError => succeed
        case _ => fail()
      }
    }
  }

}
