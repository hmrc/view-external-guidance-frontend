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

import models.{PageEvaluationContext, PageContext, RequestOperation, GET}
import core.models.RequestOutcome
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import services.{ErrorStrategy, GuidanceService}
import models.ProcessContext
import uk.gov.hmrc.http.HeaderCarrier
import core.models.ocelot.Labels
import scala.concurrent.{ExecutionContext, Future}
import play.api.i18n.Lang

trait MockGuidanceService extends MockFactory {

  val mockGuidanceService: GuidanceService = mock[GuidanceService]

  object MockGuidanceService {

    def retrieveAndCacheScratch(uuid: String, sessionRepoId: String): CallHandler[Future[RequestOutcome[(String,String)]]] =
      (mockGuidanceService
        .retrieveAndCacheScratch(_: String, _: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(uuid, *, *, *)

    def retrieveAndCachePublished(processId: String, sessionRepoId: String): CallHandler[Future[RequestOutcome[(String,String)]]] =
      (mockGuidanceService
        .retrieveAndCachePublished(_: String, _: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(processId, *, *, *)

    def retrieveAndCacheApproval(processId: String, sessionRepoId: String): CallHandler[Future[RequestOutcome[(String, String)]]] =
      (mockGuidanceService
        .retrieveAndCacheApproval(_: String, _: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(processId, *, *, *)

    def retrieveAndCacheApprovalByPageUrl(url: String)(processId: String, sessionRepoId: String): CallHandler[Future[RequestOutcome[(String, String)]]] =
      (mockGuidanceService
        .retrieveAndCacheApprovalByPageUrl(_: String)(_: String, _: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(url, processId, *, *, *)

    def getProcessContext(sessionId: String,
                          processCode: String,
                          url: String,
                          previousPageByLink: Boolean,
                          requestId: Option[String],
                          op: RequestOperation): CallHandler[Future[RequestOutcome[ProcessContext]]] =
      (mockGuidanceService
        .getProcessContext(_: String, _: String, _: String, _: Boolean, _: Option[String], _: RequestOperation)(_: ExecutionContext))
        .expects(sessionId, processCode, url, previousPageByLink, requestId, op, *)

    def getProcessContext(sessionId: String, requestId: Option[String]): CallHandler[Future[RequestOutcome[ProcessContext]]] =
      (mockGuidanceService
        .getProcessContext(_: String, _: Option[String]))
        .expects(sessionId, requestId)

    def sessionRestart(processCode: String, sessionId: String, requestId: Option[String]): CallHandler[Future[RequestOutcome[String]]] =
      (mockGuidanceService
        .sessionRestart(_: String, _: String, _: Option[String])(_: ExecutionContext))
        .expects(processCode, sessionId, requestId, *)

    def getFormPageContext(pec: PageEvaluationContext, errStrategy: ErrorStrategy): CallHandler[PageContext] =
      (mockGuidanceService
        .getFormPageContext(_: PageEvaluationContext, _: ErrorStrategy)(_: Lang))
        .expects(pec, errStrategy, *)

    def getPageContext(processId: String, url: String, previousPageByLink: Boolean, sessionId: String, requestId: Option[String]): CallHandler[Future[RequestOutcome[PageContext]]] =
      (mockGuidanceService
        .getPageContext(_: String, _: String, _: Boolean, _: String, _: Option[String])(_: ExecutionContext, _: Lang))
        .expects(processId, url, previousPageByLink, sessionId, requestId, *, *)

    def getPageEvaluationContext(
                                  processId: String,
                                  url: String,
                                  previousPageByLink: Boolean,
                                  sessionId: String,
                                  requestId: Option[String],
                                  op: RequestOperation = GET): CallHandler[Future[RequestOutcome[PageEvaluationContext]]] =
      (mockGuidanceService
        .getPageEvaluationContext(_: String, _: String, _: Boolean,  _: String, _: Option[String], _: RequestOperation)(_: ExecutionContext, _: Lang))
        .expects(processId, url, previousPageByLink, sessionId, requestId, op, *, *)

    def validateUserResponse(pec: PageEvaluationContext, response: String): CallHandler[Option[String]] =
      (mockGuidanceService
        .validateUserResponse(_: PageEvaluationContext, _: String))
        .expects(pec, response)

    def submitPage(ctx: PageEvaluationContext,
                   url: String,
                   validatedAnswer: String,
                   submittedAnswer: String,
                   requestId: Option[String]): CallHandler[Future[RequestOutcome[(Option[String], Labels)]]] =
      (mockGuidanceService
        .submitPage(_: PageEvaluationContext, _: String, _: String, _: String, _: Option[String])(_: ExecutionContext))
        .expects(ctx, url, validatedAnswer, submittedAnswer, requestId, *)

    def savePageState(docId: String, labels: Labels, requestId: Option[String]): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockGuidanceService
        .savePageState(_: String, _: Labels, _: Option[String]))
        .expects(docId, *, requestId)
  }
}
