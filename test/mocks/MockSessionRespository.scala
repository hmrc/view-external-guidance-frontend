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

package mocks

import models.{RawPageHistory, Session}
import repositories.SessionRepository
import core.models.ocelot._
import core.models.RequestOutcome
import core.models.ocelot.RunMode
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import org.scalatest.TestSuite

import scala.concurrent.Future

trait MockSessionRepository extends TestSuite with MockFactory {

  val mockSessionRepository: SessionRepository = mock[SessionRepository]

  object MockSessionRepository {

    def create(id: String, meta: Meta, runMode: RunMode, legalPageIds: List[String]): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionRepository
        .create(_: String, _: Meta, _: RunMode, _: List[String]))
        .expects(id, meta, runMode, legalPageIds)

    def delete(key: String, processCode: String): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionRepository
        .delete(_: String, _: String))
        .expects(key, processCode)

    def updateAfterFormSubmission(docId: String, processCode: String, url: String, answer: String, labels: Labels, revertOps: Option[List[LabelOperation]], nextLegalPageIds: List[String], requestId: Option[String]): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionRepository
        .updateAfterFormSubmission(_: String, _: String, _: String, _: String, _: Labels, _: Option[List[LabelOperation]], _: List[String], _: Option[String]))
        .expects(docId, processCode, url, answer, *, revertOps, nextLegalPageIds, requestId)

    def getNoUpdate(key: String, processCode: String): CallHandler[Future[RequestOutcome[Session]]] =
      (mockSessionRepository
        .getNoUpdate(_: String, _: String))
        .expects(key, processCode)

    def get(key: String, processCode: String, requestId: Option[String]): CallHandler[Future[RequestOutcome[Session]]] =
      (mockSessionRepository
        .get(_: String, _: String, _: Option[String]))
        .expects(key, processCode, requestId)

    def reset(key: String, processCode: String, requestId: Option[String]): CallHandler[Future[RequestOutcome[Session]]] =
      (mockSessionRepository
        .reset(_: String, _: String, _: Option[String]))
        .expects(key, processCode, requestId)

    def updateAfterStandardPage(key: String, processCode: String, labels: Labels, revertOps: Option[List[LabelOperation]], requestId: Option[String]): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionRepository
        .updateAfterStandardPage(_: String, _: String, _: Labels, _: Option[List[LabelOperation]], _: Option[String]))
        .expects(key, processCode, *, revertOps, requestId)

    def updateForNewPage(key: String,
                         processCode: String,
                         rawPageHistory: Option[List[RawPageHistory]],
                         flowStack: Option[List[FlowStage]],
                         labelUpdates: List[Label],
                         labelDeletions: List[String],
                         legalPageIds: List[String],
                         requestId: Option[String]): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionRepository
        .updateForNewPage(_: String, _: String, _: Option[List[RawPageHistory]], _: Option[List[FlowStage]], _: List[Label], _: List[String], _: List[String], _: Option[String]))
        .expects(key, processCode, rawPageHistory, flowStack, labelUpdates, labelDeletions, legalPageIds, requestId)
  }
}
