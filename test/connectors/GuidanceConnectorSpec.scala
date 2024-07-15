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
import core.models.RequestOutcome
import core.models.ocelot.Process
import mocks.MockAppConfig
import play.api.libs.json.Json
import org.mockito.Mockito.when
import scala.concurrent.Future

class GuidanceConnectorSpec extends BaseSpec {

  private trait Test extends ConnectorTest {
    val process: Process = Json.parse(core.models.ocelot.PrototypeJson.json).as[Process]
    val gc: GuidanceConnector = new GuidanceConnector(mockHttpClient, MockAppConfig)
  }

  "Calling the scratchProcess with an existing scratch process UUID" should {

    "return a model representing a Scatch Process" in new Test {

      when(requestBuilderExecute[RequestOutcome[Process]]).thenReturn(Future.successful(Right(process)))

      val response: RequestOutcome[Process] =
        await(gc.scratchProcess("683d9aa0-2a0e-4e28-9ac8-65ce453d2730")(hc, implicitly))

      response shouldBe Right(process)
    }
  }

  "Calling the publishedProcess with an existing published process ID" should {

    "return a model representing a published Process" in new Test {

      when(requestBuilderExecute[RequestOutcome[Process]]).thenReturn(Future.successful(Right(process)))

      val response: RequestOutcome[Process] =
        await(gc.publishedProcess("ext90002")(hc, implicitly))

      response shouldBe Right(process)
    }
  }

  "Calling approvalProcess with an existing approval process ID" should {

    "return a model representing an approval Process" in new Test {

      when(requestBuilderExecute[RequestOutcome[Process]]).thenReturn(Future.successful(Right(process)))

      val response: RequestOutcome[Process] =
        await(gc.approvalProcess("ext90002")(hc, implicitly))

      response shouldBe Right(process)
    }
  }

}
