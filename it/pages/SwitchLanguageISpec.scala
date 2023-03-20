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

package pages

import play.api.http.Status
import play.api.libs.ws.{WSRequest, WSResponse}
import stubs.AuditStub
import support.IntegrationSpec

class SwitchLanguageISpec extends IntegrationSpec {

  "calling the switch language welsh route" should {

    "return an OK response" in {

      AuditStub.audit()

      val request: WSRequest = buildRequest("/guidance/language/cymraeg")

      val response: WSResponse = await(request.get())

      response.status shouldBe Status.SEE_OTHER
    }

    "set the session request language to Welsh" in {

      AuditStub.audit()

      val request: WSRequest = buildRequest("/guidance/language/cymraeg")

      val response: WSResponse = await(request.get())

      response.cookies.find(_.name == "PLAY_LANG").map { cookie =>
        cookie.value shouldBe "cy"
      } orElse fail(s"No PLAY_LANG cookie found")

      response.status shouldBe Status.SEE_OTHER

    }

  }

  "calling the switch language english route" should {

    "return an OK response" in {

      AuditStub.audit()

      val request: WSRequest = buildRequest("/guidance/language/english")

      val response: WSResponse = await(request.get())

      response.status shouldBe Status.SEE_OTHER
    }

    "set the session request language to English" in {

      AuditStub.audit()

      val request: WSRequest = buildRequest("/guidance/language/en")

      val response: WSResponse = await(request.get())

      response.cookies.find(_.name == "PLAY_LANG").map { cookie =>
        cookie.value shouldBe "en"
      } orElse fail(s"No PLAY_LANG cookie found")

      response.status shouldBe Status.SEE_OTHER

    }

  }

}
