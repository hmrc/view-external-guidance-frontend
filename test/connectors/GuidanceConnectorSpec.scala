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

import base.BaseSpec
import uk.gov.hmrc.http.HeaderCarrier
import mocks.{MockAppConfig, MockHttpClient}
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.test.{DefaultAwaitTimeout, FutureAwaits}
import play.api.libs.json.Json
import core.models.ocelot.Process
import scala.concurrent.Future
import core.models.RequestOutcome

class GuidanceConnectorSpec extends BaseSpec with MockHttpClient {

  private trait Test extends MockHttpClient with FutureAwaits with DefaultAwaitTimeout {
    val process: Process = Json.parse(core.models.ocelot.PrototypeJson.json).as[Process]
    implicit val hc: HeaderCarrier = HeaderCarrier()
    val gc: GuidanceConnector = new GuidanceConnector(mockHttpClient, MockAppConfig)
    val scratchEndPoint: String = MockAppConfig.externalGuidanceBaseUrl + "/external-guidance/scratch/"
    val publishedEndPoint: String = MockAppConfig.externalGuidanceBaseUrl + "/external-guidance/published/"
    val approvalEndPoint: String = MockAppConfig.externalGuidanceBaseUrl + "/external-guidance/approval/"
  }

  "Calling the scratchProcess with an existing scratch process UUID" should {

    "return a model representing a Scatch Process" in new Test {

      MockedHttpClient
        .get(scratchEndPoint + "683d9aa0-2a0e-4e28-9ac8-65ce453d2730")
        .returns(Future.successful(Right(process)))

      val response: RequestOutcome[Process] =
        await(gc.scratchProcess("683d9aa0-2a0e-4e28-9ac8-65ce453d2730")(hc, implicitly))

      response shouldBe Right(process)
    }
  }

  "Calling the publishedProcess with an existing published process ID" should {

    "return a model representing a published Process" in new Test {

      MockedHttpClient
        .get(publishedEndPoint + "ext90002")
        .returns(Future.successful(Right(process)))

      val response: RequestOutcome[Process] =
        await(gc.publishedProcess("ext90002")(hc, implicitly))

      response shouldBe Right(process)
    }
  }

  "Calling approvalProcess with an existing approval process ID" should {

    "return a model representing an approval Process" in new Test {

      MockedHttpClient
        .get(approvalEndPoint + "ext90002")
        .returns(Future.successful(Right(process)))

      val response: RequestOutcome[Process] =
        await(gc.approvalProcess("ext90002")(hc, implicitly))

      response shouldBe Right(process)
    }
  }

}
