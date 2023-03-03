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

package controllers

import base.BaseSpec
import mocks.MockAppConfig
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.test.FakeRequest
import play.api.test.Helpers._
import config.ErrorHandler
import views.html.session_blocked
import play.api.test.Helpers.stubMessagesControllerComponents
import play.api.mvc.{AnyContentAsEmpty, Result}
import scala.concurrent.Future

class SessionBlockedControllerSpec extends BaseSpec with GuiceOneAppPerSuite {

  private trait Test {

    lazy val errorHandler: ErrorHandler = app.injector.instanceOf[ErrorHandler]
    lazy val view: session_blocked = app.injector.instanceOf[session_blocked]

    val target = new SessionBlockedController(MockAppConfig, stubMessagesControllerComponents(), view)

  }

  "SessionBlockedController method sessionBlocked" should {

    "delete the current session data if it exists" in new Test {

      val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")

      val result: Future[Result] = target.sessionBlocked("processCode")(fakeRequest)

      status(result) shouldBe Status.OK
    }

  }

}
