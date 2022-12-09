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

import core.models.RequestOutcome
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import services.RetrieveAndCacheService
import uk.gov.hmrc.http.HeaderCarrier
import scala.concurrent.Future
import core.models.ocelot.{Process, Page}

trait MockRetrieveAndCacheService extends MockFactory {

  val mockRetrieveAndCacheService: RetrieveAndCacheService = mock[RetrieveAndCacheService]

  object MockRetrieveAndCacheService {

    def retrieveAndCacheScratch(uuid: String, sessionRepoId: String): CallHandler[Future[RequestOutcome[(String,String)]]] =
      (mockRetrieveAndCacheService
        .retrieveAndCacheScratch(_: String, _: String)(_: HeaderCarrier))
        .expects(uuid, *, *)

    def retrieveAndCachePublished(processId: String, sessionRepoId: String): CallHandler[Future[RequestOutcome[(String,String)]]] =
      (mockRetrieveAndCacheService
        .retrieveAndCachePublished(_: String, _: String)(_: HeaderCarrier))
        .expects(processId, *, *)

    def retrieveAndCacheApproval(processId: String, sessionRepoId: String): CallHandler[Future[RequestOutcome[(String, String)]]] =
      (mockRetrieveAndCacheService
        .retrieveAndCacheApproval(_: String, _: String)(_: HeaderCarrier))
        .expects(processId, *, *)

    def retrieveAndCacheApprovalByPageUrl(url: String)(processId: String, sessionRepoId: String): CallHandler[Future[RequestOutcome[(String, String)]]] =
      (mockRetrieveAndCacheService
        .retrieveAndCacheApprovalByPageUrl(_: String)(_: String, _: String)(_: HeaderCarrier))
        .expects(url, processId, *, *)

  def retrieveOnlyPublished(processCode: String): CallHandler[Future[RequestOutcome[(Process, Seq[Page])]]] =
      (mockRetrieveAndCacheService
        .retrieveOnlyPublished(_: String)(_: HeaderCarrier))
        .expects(processCode, *)

  def retrieveOnlyApproval(processCode: String): CallHandler[Future[RequestOutcome[(Process, Seq[Page])]]] =
      (mockRetrieveAndCacheService
        .retrieveOnlyApproval(_: String)(_: HeaderCarrier))
        .expects(processCode, *)

  }
}
