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

package services

import base.BaseSpec
import core.models.errors.NotFoundError
import core.models.ocelot.stanzas._
import core.models.ocelot.{Approval, KeyedStanza, Page, PageReview, Process, ProcessJson, Published, Scratch}
import mocks.{MockGuidanceConnector, MockPageBuilder, MockSessionRepository}
import models.{PageNext, ui}
import play.api.i18n.MessagesApi
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

class RetrieveAndCacheServiceSpec extends BaseSpec {

  val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]

  trait Test extends MockGuidanceConnector with MockSessionRepository with MockPageBuilder with ProcessJson {
    implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

    def pageWithUrl(id: String, url: String) = Page(id, url, Seq(KeyedStanza("1", EndStanza)), Seq())

    val process: Process = validOnePageJson.as[Process]
    val processWithProcessCode = validOnePageProcessWithProcessCodeJson.as[Process]

    val firstPageUrl = "/first-page"
    val firstUiPage: ui.Page = ui.Page(firstPageUrl, Seq())

    val lastPageUrl = "/last-page"
    val lastUiPage: ui.Page = ui.Page(lastPageUrl, Seq())

    val pages: List[Page] = List(
      pageWithUrl(Process.StartStanzaId, firstPageUrl),
      pageWithUrl("1", "/page-1"),
      pageWithUrl("2", lastPageUrl)
    )

    val processId = "oct90001"
    val processCode = "CupOfTea"
    val uuid = "683d9aa0-2a0e-4e28-9ac8-65ce453d2730"
    val sessionRepoId = "683d9aa0-2a0e-4e28-9ac8-65ce453d2731"

    lazy val target = new RetrieveAndCacheService(
      mockGuidanceConnector,
      mockSessionRepository,
      mockPageBuilder,
      new SecuredProcessBuilder(messagesApi)
    )

  }

  "Calling retrieveAndCacheScratch" should {

    "retrieve the url of the start page for the scratch process" in new Test {

      MockGuidanceConnector
        .scratchProcess(uuid)
        .returns(Future.successful(Right(process)))

      val processWithUpdatedId = process.copy(meta = process.meta.copy( id = uuid))

      MockSessionRepository
        .create(uuid, Scratch, processWithUpdatedId, Map("/first-page" -> PageNext("start"), "/page-1" -> PageNext("1"), "/last-page" -> PageNext("2")), List("start"))
        .returns(Future.successful(Right(())))

      MockPageBuilder
        .pages(processWithUpdatedId)
        .returns(Right(pages))

      private val result = target.retrieveAndCacheScratch(uuid, uuid)

      whenReady(result) { url =>
        url shouldBe Right((firstPageUrl,"cup-of-tea"))
      }
    }

    "return NotFound error when scratch process not found" in new Test {

      MockGuidanceConnector
        .scratchProcess(uuid)
        .returns(Future.successful(Left(NotFoundError)))

      target.retrieveAndCacheScratch(uuid, uuid).map{
        case Left(err) => err shouldBe NotFoundError
        case Right(_) => fail()
      }

    }

  }

  "Calling retrieveAndCachePublished" should {

    "retrieve the url of the start page for the nominated published process" in new Test {

      MockGuidanceConnector
        .publishedProcess(processId)
        .returns(Future.successful(Right(processWithProcessCode)))

      MockSessionRepository
        .create(sessionRepoId, Published, processWithProcessCode,Map("/first-page" -> PageNext("start"), "/page-1" -> PageNext("1"), "/last-page" -> PageNext("2")), List("start"))
        .returns(Future.successful(Right(())))

      MockPageBuilder
        .pages(processWithProcessCode)
        .returns(Right(pages))

      private val result = target.retrieveAndCachePublished(processId, sessionRepoId)

      whenReady(result) { url =>
        url shouldBe Right((firstPageUrl, processCode))
      }
    }

    "return NotFound error when published process not found" in new Test {

      MockGuidanceConnector
        .publishedProcess(processId)
        .returns(Future.successful(Left(NotFoundError)))

      target.retrieveAndCachePublished(processId, sessionRepoId).map{
        case Left(err) => err shouldBe NotFoundError
        case Right(_) => fail()
      }

    }

  }

  "Calling retrieveAndCacheApproval" should {

    "retrieve the url of the start page for the nominated approval process" in new Test {

      MockGuidanceConnector
        .approvalProcess(processId)
        .returns(Future.successful(Right(processWithProcessCode)))

      MockSessionRepository
        .create(sessionRepoId, Approval, processWithProcessCode, Map("/first-page" -> PageNext("start"), "/page-1" -> PageNext("1"), "/last-page" -> PageNext("2")), List("start"))
        .returns(Future.successful(Right(())))

      MockPageBuilder
        .pages(processWithProcessCode)
        .returns(Right(pages))

      private val result = target.retrieveAndCacheApproval(processId, sessionRepoId)

      whenReady(result) { url =>
        url shouldBe Right((firstPageUrl, processCode))
      }
    }

    "return NotFound error when approval process not found" in new Test {

      MockGuidanceConnector
        .approvalProcess(processId)
        .returns(Future.successful(Left(NotFoundError)))

      target.retrieveAndCacheApproval(processId, sessionRepoId).map{
        case Left(err) => err shouldBe NotFoundError
        case Right(_) => fail()
      }

    }

  }

  "Calling retrieveAndCacheApprovalByPageUrl" should {

    "retrieve the url of the nominated page and approval process" in new Test {

      MockGuidanceConnector
        .approvalProcess(processId)
        .returns(Future.successful(Right(processWithProcessCode)))

      MockSessionRepository
        .create(sessionRepoId, PageReview, processWithProcessCode, Map("/first-page" -> PageNext("start"), "/page-1" -> PageNext("1"), "/last-page" -> PageNext("2")), List("1"))
        .returns(Future.successful(Right(())))

      MockPageBuilder
        .pages(processWithProcessCode)
        .returns(Right(pages))

      private val result = target.retrieveAndCacheApprovalByPageUrl("/page-1")(processId, sessionRepoId)

      whenReady(result) { url =>
        url shouldBe Right(("/page-1", processCode))
      }
    }

    "return NotFound error when approval process not found" in new Test {

      MockGuidanceConnector
        .approvalProcess(processId)
        .returns(Future.successful(Left(NotFoundError)))

      target.retrieveAndCacheApprovalByPageUrl("/page-1")(processId, sessionRepoId).map{
        case Left(err) => err shouldBe NotFoundError
        case Right(_) => fail()
      }

    }

  }

  "Calling retrieveOnlyPublished" should {

    "return process and pages" in new Test {

      MockGuidanceConnector
        .publishedProcess(processId)
        .returns(Future.successful(Right(processWithProcessCode)))

      MockPageBuilder
        .pages(processWithProcessCode)
        .returns(Right(pages))

      target.retrieveOnlyPublished(processId).map{
        case Right((process, pges)) =>
          process shouldBe processWithProcessCode
          pges shouldBe pages
        case Left(_) => fail()
      }
    }

    "return not found when process doesnt exist" in new Test {

      MockGuidanceConnector
        .publishedProcess(processId)
        .returns(Future.successful(Left(NotFoundError)))

      target.retrieveOnlyPublished(processId).map{
        case Right((process, pges)) => fail()
        case Left(err) => err shouldBe NotFoundError
      }

    }

  }

  "Calling retrieveOnlyApproval" should {

    "return process and pages" in new Test {

      MockGuidanceConnector
        .approvalProcessByProcessCode(processId)
        .returns(Future.successful(Right(processWithProcessCode)))

      MockPageBuilder
        .pages(processWithProcessCode)
        .returns(Right(pages))

      target.retrieveOnlyApproval(processId).map{
        case Right((process, pges)) =>
          process shouldBe processWithProcessCode
          pges shouldBe pages
        case Left(_) => fail()
      }

    }

    "return not found when process doesnt exist" in new Test {

      MockGuidanceConnector
        .approvalProcessByProcessCode(processId)
        .returns(Future.successful(Left(NotFoundError)))

      target.retrieveOnlyApproval(processId).map{
        case Right((process, pges)) => fail()
        case Left(err) => err shouldBe NotFoundError
      }

    }
  }

}

