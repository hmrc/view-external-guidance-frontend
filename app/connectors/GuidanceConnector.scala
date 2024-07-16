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

package connectors

import javax.inject.{Inject, Singleton}
import core.models.ocelot._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.client.HttpClientV2
import config.AppConfig
import core.models.RequestOutcome
import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.http.StringContextOps

@Singleton
class GuidanceConnector @Inject() (httpClient: HttpClientV2, appConfig: AppConfig) {

  def scratchProcess(uuid: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[Process]] =
    retrieveProcess(appConfig.externalGuidanceBaseUrl + s"/external-guidance/scratch/$uuid")

  def publishedProcess(processCode: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[Process]] =
    retrieveProcess(appConfig.externalGuidanceBaseUrl + s"/external-guidance/published/$processCode")

  def approvalProcess(processId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[Process]] =
    retrieveProcess(appConfig.externalGuidanceBaseUrl + s"/external-guidance/approval/$processId")

  def approvalProcessByProcessCode(processCode: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[Process]] =
    retrieveProcess(appConfig.externalGuidanceBaseUrl + s"/external-guidance/approval/code/$processCode")

  private def retrieveProcess(endPoint: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[Process]] = {
    import connectors.httpParsers.GetProcessHttpParser.getProcessHttpReads

    httpClient.get(url"$endPoint").execute[RequestOutcome[Process]]
  }

}
