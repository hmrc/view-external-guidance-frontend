/*
 * Copyright 2022 HM Revenue & Customs
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

import connectors.GuidanceConnector
import core.models.ocelot.Process
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.http.HeaderCarrier
import core.models.RequestOutcome

import scala.concurrent.{ExecutionContext, Future}

trait MockGuidanceConnector extends MockFactory {

  val mockGuidanceConnector: GuidanceConnector = mock[GuidanceConnector]

  object MockGuidanceConnector {

    def scratchProcess(uuid: String): CallHandler[Future[RequestOutcome[Process]]] = {
      (mockGuidanceConnector
        .scratchProcess(_: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(uuid, *, *)

    }

    def publishedProcess(processId: String): CallHandler[Future[RequestOutcome[Process]]] = {
      (mockGuidanceConnector
        .publishedProcess(_: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(processId, *, *)

    }

    def approvalProcess(processId: String): CallHandler[Future[RequestOutcome[Process]]] = {
      (mockGuidanceConnector
        .approvalProcess(_: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(processId, *, *)

    }

    def approvalProcessByProcessCode(processCode: String): CallHandler[Future[RequestOutcome[Process]]] = {
      (mockGuidanceConnector
        .approvalProcessByProcessCode(_: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(processCode, *, *)

    }

  }
}
