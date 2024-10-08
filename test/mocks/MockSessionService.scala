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

import models.PageNext
import core.models.RequestOutcome
import core.models.ocelot.{FlowStage, Label, Labels, Process, RunMode}
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import services.SessionService
import models.PageHistory
import models.GuidanceSession
import org.scalatest.TestSuite

import scala.concurrent.{ExecutionContext, Future}
import core.models.ocelot.LabelOperation

trait MockSessionService extends TestSuite with MockFactory {

  val mockSessionService: SessionService = mock[SessionService]

  object MockSessionService {

    def create(key: String, runMode: RunMode, process: Process, pageMap: Map[String, PageNext], legalPageIds: List[String]): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionService
        .create(_: String, _: RunMode, _: Process, _:Map[String, PageNext], _: List[String])(_: ExecutionContext))
        .expects(key, runMode, process, pageMap, legalPageIds, *)

    def delete(key: String, processCode: String): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionService
        .delete(_: String, _: String))
        .expects(key, processCode)

    def getNoUpdate(key: String, processCode: String): CallHandler[Future[RequestOutcome[GuidanceSession]]] =
      (mockSessionService
        .getNoUpdate(_: String, _: String)( _: ExecutionContext))
        .expects(key, processCode, *)

    def get(key: String, processCode: String, requestId: Option[String]): CallHandler[Future[RequestOutcome[GuidanceSession]]] =
      (mockSessionService
        .get(_: String, _: String, _: Option[String])(_: ExecutionContext))
        .expects(key, processCode, requestId, *)

    def reset(key: String, processCode: String, requestId: Option[String]): CallHandler[Future[RequestOutcome[GuidanceSession]]] =
      (mockSessionService
        .reset(_: String, _: String, _: Option[String])(_: ExecutionContext))
        .expects(key, processCode, requestId, *)

    def updateForNewPage(key: String, processCode: String, pageMap: Map[String, PageNext], pageHistory: Option[List[PageHistory]], flowStack: Option[List[FlowStage]],
                         labelUpdates: List[Label], labelDeletions: List[String], legalPageIds: List[String], requestId: Option[String]): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionService
        .updateForNewPage(_: String, _: String, _: Map[String, PageNext], _: Option[List[PageHistory]], _: Option[List[FlowStage]], _: List[Label], _: List[String], _: List[String], _: Option[String]))
        .expects(key, processCode, pageMap, pageHistory, flowStack, labelUpdates, labelDeletions, legalPageIds, requestId)

    def updateAfterStandardPage(key: String, processCode: String, labels: Labels, revertOperations: Option[List[LabelOperation]], requestId: Option[String]): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionService
        .updateAfterStandardPage(_: String, _: String, _: Labels, _: Option[List[LabelOperation]], _: Option[String]))
        .expects(key, processCode, *, revertOperations, requestId)

    def updateAfterFormSubmission(key: String, processCode: String, answerId: String, answer: String, labels: Labels, nextLegalPageIds: List[String],
                                  revertOperations: Option[List[LabelOperation]], requestId: Option[String]): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionService
        .updateAfterFormSubmission(_: String, _: String, _: String, _: String, _: Labels, _: List[String], _: Option[List[LabelOperation]], _: Option[String]))
        .expects(key, processCode, answerId, answer, *, nextLegalPageIds, revertOperations, requestId)
  }
}
