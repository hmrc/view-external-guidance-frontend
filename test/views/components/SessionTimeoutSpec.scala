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

package views.components

import base.{BaseSpec, ViewFns, ViewSpec}
import mocks.MockAppConfig
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest
import views.html.{user_deleted_session, system_timedout_session}

class SessionTimeoutSpec extends BaseSpec with ViewFns with ViewSpec with GuiceOneAppPerSuite {

  trait Test {

    private def injector: Injector = app.injector

    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    def sessionTimeout: system_timedout_session = injector.instanceOf[system_timedout_session]
    def deleteYourAnswers: user_deleted_session = injector.instanceOf[user_deleted_session]

    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))

    val fakeRequest = FakeRequest("GET", "/")

    val processTitle = "How to make a cup of tea"
    val defaultProcessTitle = "Interactive Guidance"
    val processCode = "cup-of-tea"
    val startUrl = s"${MockAppConfig.baseUrl}/$processCode"
    val buttonTarget = s"$startUrl/page-1"
  }

  "The session timeout view" should {

    "render timeout page correctly when all input arguments are defined" in new Test {

      val doc = asDocument(sessionTimeout(
        processTitle,
        Some(processCode),
        Some(startUrl),
        buttonTarget)(fakeRequest, messages))

      val pageTitleElement: Element = getSingleElementByTag(doc, "title")

      pageTitleElement.text shouldBe s"${messages("session.timeout.session.has.expired")} – ${messages("service.name")} – ${messages("service.govuk")}"

      val htmlHeaderDivElement: Element = getSingleElementByClass(doc, "govuk-header__content")

      val htmlHeaderLinks: Elements = htmlHeaderDivElement.getElementsByTag("a")

      htmlHeaderLinks.size() shouldBe 1

      htmlHeaderLinks.first.text shouldBe processTitle

      val htmlHeaderLinkAttrs: Map[String, String] = elementAttrs(htmlHeaderLinks.first)

      htmlHeaderLinkAttrs.contains("href") shouldBe true
      htmlHeaderLinkAttrs("href") shouldBe startUrl

      val pageHeaderDivElement: Element = getSingleElementByClass(doc, "govuk-!-margin-bottom-6")

      val pageHeadings: Elements = pageHeaderDivElement.getElementsByTag("h1")

      pageHeadings.size shouldBe 1

      pageHeadings.first.text shouldBe messages("session.timeout.session.has.expired")

      val startAgainLinkElement: Option[Element] = getElementById(doc, "startAgain")

      startAgainLinkElement match {
        case Some(linkElement) =>
          val attrs: Map[String, String] = elementAttrs(linkElement)

          attrs.contains("href") shouldBe true
          attrs("href") shouldBe buttonTarget

          linkElement.text shouldBe messages("session.timeout.button.text")
        case None => fail( "Unable to locate start again link")
      }

    }

  }

  "The delete your answers view" should {

    "render correctly when all input arguments are defined" in new Test {

      val doc = asDocument(deleteYourAnswers(
        processTitle,
        Some(processCode),
        Some(startUrl),
        buttonTarget)(fakeRequest, messages))

      val pageTitleElement: Element = getSingleElementByTag(doc, "title")

      pageTitleElement.text shouldBe s"${messages("session.timeout.delete.your.answers")} – ${messages("service.name")} – ${messages("service.govuk")}"

      val htmlHeaderDivElement: Element = getSingleElementByClass(doc, "govuk-header__content")

      val htmlHeaderLinks: Elements = htmlHeaderDivElement.getElementsByTag("a")

      htmlHeaderLinks.size() shouldBe 1

      htmlHeaderLinks.first.text shouldBe processTitle

      val htmlHeaderLinkAttrs: Map[String, String] = elementAttrs(htmlHeaderLinks.first)

      htmlHeaderLinkAttrs.contains("href") shouldBe true
      htmlHeaderLinkAttrs("href") shouldBe startUrl

      val pageHeaderDivElement: Element = getSingleElementByClass(doc, "govuk-!-margin-bottom-6")

      val pageHeadings: Elements = pageHeaderDivElement.getElementsByTag("h1")

      pageHeadings.size shouldBe 1

      pageHeadings.first.text shouldBe messages("session.timeout.delete.your.answers")

      val startAgainLinkElement: Option[Element] = getElementById(doc, "startAgain")

      startAgainLinkElement match {
        case Some(linkElement) =>
          val attrs: Map[String, String] = elementAttrs(linkElement)

          attrs.contains("href") shouldBe true
          attrs("href") shouldBe buttonTarget

          linkElement.text shouldBe messages("session.timeout.button.text")
        case None => fail( "Unable to locate start again link")
      }

    }

  }
}
