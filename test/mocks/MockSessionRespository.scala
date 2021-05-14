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

import repositories.SessionRepository
import models.{PageNext, ProcessContext}
import core.models.ocelot.{Labels, Process}
import core.models.RequestOutcome
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import scala.concurrent.Future

trait MockSessionRepository extends MockFactory {

  val mockSessionRepository: SessionRepository = mock[SessionRepository]

  object MockSessionRepository {

    def getUpdateForGET(key: String, pageHistoryUrl: Option[String], previousPageByLink: Boolean): CallHandler[Future[RequestOutcome[ProcessContext]]] =
      (mockSessionRepository
        .getUpdateForGET(_: String, _: Option[String], _: Boolean))
        .expects(key, pageHistoryUrl, previousPageByLink)

    def getUpdateForPOST(key: String, pageHistoryUrl: Option[String]): CallHandler[Future[RequestOutcome[ProcessContext]]] =
      (mockSessionRepository
        .getUpdateForPOST(_: String, _: Option[String]))
        .expects(key, pageHistoryUrl)

    def set(key: String, process: Process, pageMap: Map[String, PageNext]): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionRepository
        .set(_: String, _: Process, _: Map[String, PageNext]))
        .expects(key, process, pageMap)

    def saveFormPageState(docId: String, url: String, answer: String, labels: Labels, nextLegalPageIds: List[String]): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionRepository
        .saveFormPageState(_: String, _: String, _: String, _: Labels, _: List[String]))
        .expects(docId, url, answer, *, nextLegalPageIds)

    def getNoUpdate(key: String): CallHandler[Future[RequestOutcome[ProcessContext]]] =
      (mockSessionRepository
        .getNoUpdate(_: String))
        .expects(key)

    def getResetSession(key: String): CallHandler[Future[RequestOutcome[ProcessContext]]] =
      (mockSessionRepository
        .getResetSession(_: String))
        .expects(key)

    def savePageState(key: String, labels: Labels): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionRepository
        .savePageState(_: String, _: Labels))
        .expects(key, *)
  }
}
