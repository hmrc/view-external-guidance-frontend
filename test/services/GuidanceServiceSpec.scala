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

package services

import base.BaseSpec
import mocks.{MockAppConfig, MockGuidanceConnector, MockPageBuilder, MockSessionRepository, MockUIBuilder}
import models.ocelot.stanzas._
import models.ocelot.{Page, Process, ProcessJson}
import models.ui
import uk.gov.hmrc.http.HeaderCarrier
import repositories.ProcessContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import models.errors.BadRequestError
import models.ui.PageContext

class GuidanceServiceSpec extends BaseSpec {

  private trait Test extends MockGuidanceConnector with MockSessionRepository with MockPageBuilder with MockUIBuilder with ProcessJson {

    implicit val headerCarrier: HeaderCarrier = HeaderCarrier()
    implicit val stanzaIdToUrl: Map[String, String] = Map[String, String]()

    private def pageWithUrl(id: String, url: String) = Page(id, url, Seq(EndStanza), Seq())

    val process: Process = validOnePageJson.as[Process]
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

    val processId = "oct90001"
    val uuid = "683d9aa0-2a0e-4e28-9ac8-65ce453d2730"
    val sessionRepoId = "683d9aa0-2a0e-4e28-9ac8-65ce453d2731"

    lazy val target = new GuidanceService(MockAppConfig, mockGuidanceConnector, mockSessionRepository, mockPageBuilder, mockUIBuilder)
  }

  "Calling getPageContext with a valid URL" should {

    "retrieve a page for the process" in new Test {

      MockSessionRepository
        .get(sessionRepoId)
        .returns(Future.successful(Right(ProcessContext(process, Map()))))

      MockPageBuilder
        .pages(process)
        .returns(Right(pages))

      MockUIBuilder
        .fromStanzaPage(pages.last, None)
        .returns(lastUiPage)

      private val result = target.getPageContext(processId, lastPageUrl, sessionRepoId)

      whenReady(result) { pageContext =>
        pageContext match {
          case Right(pc) => pc.page.urlPath shouldBe lastPageUrl
          case Left(err) => fail(s"no PageContext found with error $err")
        }
      }
    }
  }

  "Calling getPageContext against a previously answered Question page url" should {

    "retrieve a PageContext which includes the relevant answer" in new Test {
      override val processId: String = "ext90002"
      MockSessionRepository
        .get(sessionRepoId)
        .returns(Future.successful(Right(ProcessContext(fullProcess, Map(lastPageUrl -> "answer")))))

      MockPageBuilder
        .pages(fullProcess)
        .returns(Right(pages))

      MockUIBuilder
        .fromStanzaPage(pages.last, None)
        .returns(lastUiPage)

      private val result = target.getPageContext(processId, lastPageUrl, sessionRepoId)

      whenReady(result) { pageContext =>
        pageContext match {
          case Right(PageContext(_, _, _, _, Some(answer))) => succeed
          case Right(wrongContext) => fail(s"Previous answer missing from PageContext, $wrongContext")
          case Left(err) => fail(s"Previous answer missing from PageContext, $err")
        }
      }
    }
  }

  "Calling getPageContext with an invalid URL" should {

    "not retrieve a page from the process" in new Test {

      val url = "scooby"

      MockSessionRepository
        .get(processId)
        .returns(Future.successful(Right(ProcessContext(process, Map()))))

      MockPageBuilder
        .pages(process)
        .returns(Right(pages))

      private val result = target.getPageContext(processId, url, processId)

      whenReady(result) {
        _ shouldBe Left(BadRequestError)
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
        .set(uuid, processWithUpdatedId)
        .returns(Future.successful(Right(())))

      MockPageBuilder
        .pages(processWithUpdatedId)
        .returns(Right(pages))

      private val result = target.retrieveAndCacheScratch(uuid, uuid)

      whenReady(result) { url =>
        url shouldBe Right(firstPageUrl)
      }
    }
  }

  "Calling retrieveAndCachePublished" should {

    "retrieve the url of the start page for the nominated published process" in new Test {

      MockGuidanceConnector
        .publishedProcess(processId)
        .returns(Future.successful(Right(process)))

      MockSessionRepository
        .set(sessionRepoId, process)
        .returns(Future.successful(Right(())))

      MockPageBuilder
        .pages(process)
        .returns(Right(pages))

      private val result = target.retrieveAndCachePublished(processId, sessionRepoId)

      whenReady(result) { url =>
        url shouldBe Right(firstPageUrl)
      }
    }
  }

  "Calling retrieveAndCacheApproval" should {

    "retrieve the url of the start page for the nominated published process" in new Test {

      MockGuidanceConnector
        .approvalProcess(processId)
        .returns(Future.successful(Right(process)))

      MockSessionRepository
        .set(sessionRepoId, process)
        .returns(Future.successful(Right(())))

      MockPageBuilder
        .pages(process)
        .returns(Right(pages))

      private val result = target.retrieveAndCacheApproval(processId, sessionRepoId)

      whenReady(result) { url =>
        url shouldBe Right(firstPageUrl)
      }
    }
  }

  "Calling saveAnswerToQuestion" should {

    "store the answer in the local repo" in new Test {

      MockSessionRepository
        .saveAnswerToQuestion(sessionRepoId, firstPageUrl, lastPageUrl)
        .returns(Future.successful(Right(())))

      MockPageBuilder
        .pages(process)
        .returns(Right(pages))

      private val result = target.saveAnswerToQuestion(sessionRepoId, firstPageUrl, lastPageUrl)

      whenReady(result) { ret =>
        ret shouldBe Right({})
      }
    }
  }

}
