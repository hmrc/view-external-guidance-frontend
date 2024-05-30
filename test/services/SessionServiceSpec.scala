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
import core.models.errors.DatabaseError
import core.models.ocelot.{Process, Published, ProcessJson}
import mocks._
import models._
import repositories.{SessionKey, Session, CachedProcess, PageHistory, RawPageHistory}
import uk.gov.hmrc.http.{HeaderCarrier, RequestId}

import scala.concurrent.Future
import java.time.Instant
import core.models.RequestOutcome

class SessionServiceSpec extends BaseSpec with MockProcessCacheRepository with MockSessionRepository {

  val rId: String = "71dcc4a3-9d19-47f5-ad97-74bb6c2a15c4"

  trait Test extends ProcessJson {
    implicit val headerCarrier: HeaderCarrier = HeaderCarrier(requestId = Some(RequestId(rId)))

    val process: Process = validOnePageJson.as[Process]
    val processWithProcessCode: Process = validOnePageProcessWithProcessCodeJson.as[Process]
    val fullProcess: Process = prototypeJson.as[Process]
    val firstPageUrl = "/first-page"
    val lastPageUrl = "/last-page"
    val processId = "oct90001"
    val processCode = "CupOfTea"
    val uuid = "683d9aa0-2a0e-4e28-9ac8-65ce453d2730"
    val sessionRepoId = "683d9aa0-2a0e-4e28-9ac8-65ce453d2731"

    lazy val target = new SessionService(
      MockAppConfig,
      mockSessionRepository,
      mockProcessCacheRepository)

  }

  "SessionService create" should {

    "Create a new session" in new Test {

      MockSessionRepository
        .create(sessionRepoId, process.meta, Published, List())
        .returns(Future.successful(Right(())))

      MockProcessCacheRepository
        .create(process, Map(), Published)
        .returns(Future.successful(Right(())))

      val result: Future[RequestOutcome[Unit]] = target.create(sessionRepoId, Published, process, Map(), List())

      whenReady(result) {
        case Right(pc) => succeed
        case Left(err) => fail()
      }
    }

    "Fail to Create a new session if session repository returns an error" in new Test {

      MockSessionRepository
        .create(sessionRepoId, process.meta, Published, List())
        .returns(Future.successful(Left(DatabaseError)))

      whenReady(target.create(sessionRepoId, Published, process, Map(), List())) {
        case Right(pc) => fail()
        case Left(err) => succeed
      }
    }

    "Fail to Create a new session if process cache repository returns an error" in new Test {

      MockSessionRepository
        .create(sessionRepoId, process.meta, Published, List())
        .returns(Future.successful(Right(())))

      MockProcessCacheRepository
        .create(process, Map(), Published)
        .returns(Future.successful(Left(DatabaseError)))

      whenReady(target.create(sessionRepoId, Published, process, Map(), List())) {
        case Right(pc) => fail()
        case Left(err) => succeed
      }

    }

  }

  "SessionService guidanceSession" should {

    "Query process cache for Sessions containg only dynamic items" in new Test {
      val expiry: Instant = Instant.now
      val newSession: Session = Session(
                        SessionKey(processId, process.meta.processCode),
                        Some(Published), process.meta.id,
                        Map(), Nil, Map(), Map(), Nil, Nil, Nil, None, Instant.now,
                        process.meta.lastUpdate,
                        process.meta.timescalesVersion,
                        process.meta.ratesVersion
                      )
      val cachedProcess: CachedProcess = CachedProcess(
                            repositories.CacheKey(processId, process.meta.lastUpdate, process.meta.timescalesVersion, process.meta.ratesVersion),
                            process,
                            Map(),
                            expiry
                          )

      MockProcessCacheRepository
        .get(processId, process.meta.lastUpdate, process.meta.timescalesVersion, process.meta.ratesVersion)
        .returns(Future.successful(Right(cachedProcess)))

      whenReady(target.guidanceSession(newSession)) {
        case Right(gSession) if gSession == GuidanceSession(newSession, process, Map()) => succeed
        case _ => fail()
      }
    }
  }

  "Session Service rawPageHistory" should {
    "Convert valid pageHistory" in new Test {
      val pageHistory = List(PageHistory(s"$processCode/start", Nil),
      PageHistory(s"$processCode/next", Nil), PageHistory(s"$processCode/somepage", Nil),
      PageHistory(s"$processCode/another", Nil))
      val pageMap = Map("/start" -> PageNext("start"), "/next" -> PageNext("1"), "/somepage" -> PageNext("2"), "/another" -> PageNext("3"))

      val result = target.toRawPageHistory(Some(pageHistory), pageMap, processCode)
      val expectedRawPageHistory = List(RawPageHistory("start",List()), RawPageHistory("1",List()), RawPageHistory("2",List()), RawPageHistory("3",List()))

      result match {
        case None => fail()
        case Some (rph) =>
          rph shouldBe expectedRawPageHistory
          rph.length shouldBe pageHistory.length
      }
    }

    "Fail to convert invalid pageHistory" in new Test {
      val pageHistory = List(PageHistory(s"$processCode/start", Nil), PageHistory(s"$processCode/unknown", Nil), PageHistory(s"$processCode/somepage", Nil), PageHistory(s"$processCode/another", Nil))
      val pageMap = Map("/start" -> PageNext("start"), "/somepage" -> PageNext("2"), "/another" -> PageNext("3"))

      val result = target.toRawPageHistory(Some(pageHistory), pageMap, processCode)

      result match {
        case None => succeed
        case Some(rph) => fail()
      }
    }
  }
}
