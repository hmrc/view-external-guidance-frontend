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
import repositories.{SessionKey, Session}
import uk.gov.hmrc.http.{HeaderCarrier, RequestId}
import scala.concurrent.Future
import java.time.Instant
import repositories.CachedProcess

class SessionServiceSpec extends BaseSpec with MockProcessCacheRepository with MockSessionRepository {

  val rId: String = "71dcc4a3-9d19-47f5-ad97-74bb6c2a15c4"

  trait Test extends ProcessJson {
    implicit val headerCarrier: HeaderCarrier = HeaderCarrier(requestId = Some(RequestId(rId)))

    val process: Process = validOnePageJson.as[Process]
    val processWithProcessCode = validOnePageProcessWithProcessCodeJson.as[Process]
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

      val result = target.create(sessionRepoId, Published, process, Map(), List())

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
      val expiry = Instant.now
      val newSession = Session(
                        SessionKey(processId, process.meta.processCode), 
                        Some(Published), process.meta.id, 
                        None, Map(), Nil, Map(), None, Map(), Nil, Nil, None, Instant.now, 
                        Some(process.meta.lastUpdate)
                      )
      val cachedProcess = CachedProcess(
                            repositories.CacheKey(processId, process.meta.lastUpdate),
                            process,
                            Map(),
                            expiry
                          )

      MockProcessCacheRepository
        .get(processId, process.meta.lastUpdate)
        .returns(Future.successful(Right(cachedProcess)))      

      whenReady(target.guidanceSession(newSession)) {
        case Right(gSession) if gSession == GuidanceSession(newSession, process, Map()) => succeed
        case _ => fail()
      }      
    }

    "Not query process cache for in inflight obsolete sessions" in new Test {      
      val obsoleteSession = Session(
                        SessionKey(processId, process.meta.processCode), 
                        Some(Published), process.meta.id, 
                        Some(process), Map(), Nil, Map(), Some(Map()), Map(), Nil, Nil, None, Instant.now, 
                        None
                      )

      whenReady(target.guidanceSession(obsoleteSession)) {
        case Right(gSession) if gSession == GuidanceSession(obsoleteSession, process, Map()) => succeed
        case _ => fail()
      }      
    }

  }

}
