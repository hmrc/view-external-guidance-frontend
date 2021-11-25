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

package mocks

import repositories.{Session, SessionRepository, PageHistory}
import models.{PageNext, GuidanceSession}
import core.models.ocelot._
import core.models.RequestOutcome
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import scala.concurrent.Future

trait MockSessionRepository extends MockFactory {

  val mockSessionRepository: SessionRepository = mock[SessionRepository]

  object MockSessionRepository {

    def set(key: String, process: Process, pageMap: Map[String, PageNext]): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionRepository
        .set(_: String, _: Process, _: Map[String, PageNext]))
        .expects(key, process, pageMap)

    def saveFormPageState(docId: String, url: String, answer: String, labels: Labels, nextLegalPageIds: List[String]): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionRepository
        .saveFormPageState(_: String, _: String, _: String, _: Labels, _: List[String]))
        .expects(docId, url, answer, *, nextLegalPageIds)

    def getGuidanceSessionById(key: String): CallHandler[Future[RequestOutcome[GuidanceSession]]] =
      (mockSessionRepository
        .getGuidanceSessionById(_: String))
        .expects(key)

    def getGuidanceSession(key: String, processCode: String): CallHandler[Future[RequestOutcome[Session]]] =
      (mockSessionRepository
        .getGuidanceSession(_: String, _: String))
        .expects(key, processCode)

    def getResetGuidanceSession(key: String, processCode: String): CallHandler[Future[RequestOutcome[GuidanceSession]]] =
      (mockSessionRepository
        .getResetGuidanceSession(_: String, _: String))
        .expects(key, processCode)

    def savePageState(key: String, labels: Labels): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionRepository
        .savePageState(_: String, _: Labels))
        .expects(key, *)

    def saveUpdates(key: String,
                    pageHistory: Option[List[PageHistory]],
                    flowStack: Option[List[FlowStage]],
                    labelUpdates: List[Label],
                    legalPageIds: List[String]): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionRepository
        .saveUpdates(_: String, _: Option[List[PageHistory]], _: Option[List[FlowStage]], _: List[Label], _: List[String]))
        .expects(key, pageHistory, flowStack, labelUpdates, legalPageIds)
  }
}
