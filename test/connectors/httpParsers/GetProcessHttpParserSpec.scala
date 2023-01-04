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

package connectors.httpParsers

import connectors.httpParsers.GetProcessHttpParser.getProcessHttpReads
import play.api.http.{HttpVerbs, Status}
import play.api.libs.json.{Json, JsValue, JsObject, JsNull}
import uk.gov.hmrc.http.HttpResponse
import core.models.RequestOutcome
import core.models.errors._
import core.models.ocelot.Process
import base.BaseSpec

class GetProcessHttpParserSpec extends BaseSpec with HttpVerbs with Status {

  private trait Test {

    val url: String = "/test"
    val validResponse: JsObject = Json.parse(core.models.ocelot.PrototypeJson.json).as[JsObject]
    val process: Process = validResponse.as[Process]

    val invalidResponse: JsValue = Json.obj()
  }

  "Parsing a successful response" should {

    "return a valid scratch process" in new Test {

      private val httpResponse = HttpResponse(OK, validResponse, Map.empty[String, Seq[String]])
      private val result = getProcessHttpReads.read(GET, url, httpResponse)
      result shouldBe Right(process)
    }
  }

  "Parsing an error response" should {

    "returns OK but no process" in new Test {

      private val httpResponse = HttpResponse(OK, JsNull, Map.empty[String, Seq[String]])
      private val result = getProcessHttpReads.read(GET, url, httpResponse)
      result shouldBe Left(InternalServerError)
    }

    "return an invalid process error for a Not found" in new Test {

      val httpResponse: HttpResponse = HttpResponse(NOT_FOUND, JsNull, Map.empty[String, Seq[String]])

      val result: RequestOutcome[Process] =
        getProcessHttpReads.read(GET, url, httpResponse)

      result shouldBe Left(NotFoundError)
    }

    "return an invalid process error for a bad request" in new Test {

      val httpResponse: HttpResponse = HttpResponse(BAD_REQUEST, JsNull, Map.empty[String, Seq[String]])

      val result: RequestOutcome[Process] =
        getProcessHttpReads.read(GET, url, httpResponse)

      result shouldBe Left(InvalidProcessError)
    }

    "return an internal server error for an invalid response" in new Test {

      val httpResponse: HttpResponse = HttpResponse(CREATED, invalidResponse, Map.empty[String, Seq[String]])

      val result: RequestOutcome[Process] =
        getProcessHttpReads.read(GET, url, httpResponse)

      result shouldBe Left(InternalServerError)
    }

    "return an internal server error when an error distinct from invalid process occurs" in new Test {

      val httpResponse: HttpResponse = HttpResponse(SERVICE_UNAVAILABLE, JsNull, Map.empty[String, Seq[String]])

      val result: RequestOutcome[Process] =
        getProcessHttpReads.read(GET, url, httpResponse)

      result shouldBe Left(InternalServerError)
    }
  }
}
