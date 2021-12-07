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

import models.{PageEvaluationContext, PageContext}
import core.models.RequestOutcome
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import services.{ErrorStrategy, GuidanceService}
import models.GuidanceSession
import core.models.ocelot.Labels
import scala.concurrent.{ExecutionContext, Future}
import play.api.i18n.Lang
import uk.gov.hmrc.http.HeaderCarrier

trait MockGuidanceService extends MockFactory {

  val mockGuidanceService: GuidanceService = mock[GuidanceService]

  object MockGuidanceService {

    def getCurrentGuidanceSession(processCode: Option[String])(sessionId: String): CallHandler[Future[RequestOutcome[GuidanceSession]]] =
      (mockGuidanceService
        .getCurrentGuidanceSession(_: Option[String])(_: String)(_: ExecutionContext))
        .expects(processCode, sessionId, *)

    def getPageGuidanceSession(key: String, processCode: String, pageHistoryUrl: Option[String], previousPageByLink: Boolean): CallHandler[Future[RequestOutcome[GuidanceSession]]] =
      (mockGuidanceService
        .getPageGuidanceSession(_: String, _: String, _: Option[String], _: Boolean)(_: HeaderCarrier, _: ExecutionContext))
        .expects(key, processCode, pageHistoryUrl, previousPageByLink, *, *)

    def getSubmitGuidanceSession(key: String, processCode: String, pageHistoryUrl: Option[String]): CallHandler[Future[RequestOutcome[GuidanceSession]]] =
      (mockGuidanceService
        .getSubmitGuidanceSession(_: String, _: String, _: Option[String])(_: HeaderCarrier, _: ExecutionContext))
        .expects(key, processCode, pageHistoryUrl, *, *)

    def sessionRestart(processCode: String, sessionId: String): CallHandler[Future[RequestOutcome[String]]] =
      (mockGuidanceService
        .sessionRestart(_: String, _: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(processCode, sessionId, *, *)

    def getPageContext(pec: PageEvaluationContext, errStrategy: ErrorStrategy): CallHandler[RequestOutcome[PageContext]] =
      (mockGuidanceService
        .getPageContext(_: PageEvaluationContext, _: ErrorStrategy)(_: Lang))
        .expects(pec, errStrategy, *)

    def getPageContext(processId: String, url: String, previousPageByLink: Boolean, sessionId: String): CallHandler[Future[RequestOutcome[PageContext]]] =
      (mockGuidanceService
        .getPageContext(_: String, _: String, _: Boolean, _: String)(_: HeaderCarrier, _: ExecutionContext, _: Lang))
        .expects(processId, url, previousPageByLink, sessionId, *, *, *)

    def getSubmitEvaluationContext(
                                  processId: String,
                                  url: String,
                                  sessionId: String): CallHandler[Future[RequestOutcome[PageEvaluationContext]]] =
      (mockGuidanceService
        .getSubmitEvaluationContext(_: String, _: String, _: String)(_: HeaderCarrier, _: ExecutionContext, _: Lang))
        .expects(processId, url, sessionId, *, *, *)

    def getPageEvaluationContext(
                                  processId: String,
                                  url: String,
                                  previousPageByLink: Boolean,
                                  sessionId: String): CallHandler[Future[RequestOutcome[PageEvaluationContext]]] =
      (mockGuidanceService
        .getPageEvaluationContext(_: String, _: String, _: Boolean,  _: String)(_: HeaderCarrier, _: ExecutionContext, _: Lang))
        .expects(processId, url, previousPageByLink, sessionId, *, *, *)

    def submitPage(
                    ctx: PageEvaluationContext,
                    url: String,
                    validatedAnswer: String,
                    submittedAnswer: String): CallHandler[Future[RequestOutcome[(Option[String], Labels)]]] =
      (mockGuidanceService
        .submitPage(_: PageEvaluationContext, _: String, _: String, _: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(ctx, url, validatedAnswer, submittedAnswer, *, *)

    def savePageState(docId: String, labels: Labels): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockGuidanceService
        .savePageState(_: String, _: Labels)(_: HeaderCarrier))
        .expects(docId, *, *)
  }
}
