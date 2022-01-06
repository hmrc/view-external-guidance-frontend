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

package controllers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api._
import play.api.mvc.Cookies
import play.api.i18n.{MessagesApi, DefaultLangsProvider}
import play.api.test.Helpers.stubMessagesControllerComponents
import uk.gov.hmrc.play.language.LanguageUtils
import play.api.http.HeaderNames
import mocks.MockAppConfig

class SwitchLanguageControllerSpec extends AnyWordSpec with Matchers with GuiceOneAppPerSuite {
  private val fakeRequest = FakeRequest("GET", "/")
  private val env = Environment.simple()
  private val configuration = Configuration.load(env)
  private val appConfig = MockAppConfig
  val langsProvider = app.injector.instanceOf[DefaultLangsProvider]
  val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val langUtils = new LanguageUtils(langsProvider.get, configuration)(messagesApi)

  private val controller = new SwitchLanguageController(appConfig, langUtils, stubMessagesControllerComponents())

  val playCookieName = messagesApi.langCookieName

  def confirmLangCookie(cookies: Cookies, lang: String): Unit =
    cookies.get(playCookieName) match {
      case Some(cookie) => cookie.value shouldBe lang
      case None => fail(s"Missing $playCookieName cookie")
    }

  "GET /language/cy" should {
    "return 303 and set lang to cy" in {
      val result = controller.switchToLanguage("cymraeg")(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
      confirmLangCookie(cookies(result), "cy")
    }
  }

  "GET /language/en" should {
    "return 303 and set lang to en" in {
      val result = controller.switchToLanguage("english")(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
      confirmLangCookie(cookies(result), "en")
    }
  }

  "GET /language/xxx" should {
    "return 303 and fail to set lang unknown language, revert en" in {
      val result = controller.switchToLanguage("xx")(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
      confirmLangCookie(cookies(result), "en")
    }
  }

  "Lang switch" should {
    "redirect to referred page relatively if referrer is valid and includes the protocol and host name" in {
      val result = controller.switchToLanguage("cy")(fakeRequest.withHeaders((HeaderNames.REFERER, s"${appConfig.hostBaseUrl}/somepage")))
      redirectLocation(result) shouldBe Some(s"/guidance/somepage")

      val adminResult = controller.switchToLanguage("cy")(fakeRequest.withHeaders((HeaderNames.REFERER, s"${appConfig.adminHostBaseUrl}/somepage")))
      redirectLocation(adminResult) shouldBe Some(s"/guidance/somepage")

    }

    "redirect to accessibility page if referer is bogus" in {
      val result = controller.switchToLanguage("cy")(fakeRequest.withHeaders((HeaderNames.REFERER, "https://www.bbc.co.uk")))
      redirectLocation(result) shouldBe Some("/accessibility-statement/interactive-guidance")

    }

    "redirect to accessibility page if referrer only service host or admin host" in {
      val result = controller.switchToLanguage("cy")(fakeRequest.withHeaders((HeaderNames.REFERER, appConfig.host)))
      redirectLocation(result) shouldBe Some("/accessibility-statement/interactive-guidance")

      val adminResult = controller.switchToLanguage("cy")(fakeRequest.withHeaders((HeaderNames.REFERER, appConfig.adminHost)))
      redirectLocation(adminResult) shouldBe Some("/accessibility-statement/interactive-guidance")

    }

  }

}
