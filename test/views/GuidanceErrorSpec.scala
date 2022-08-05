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

package views

import base.{ViewFns, ViewSpec}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest
import play.twirl.api.Html
import views.html.runtime_error_template

class GuidanceErrorSpec extends ViewSpec with ViewFns with GuiceOneAppPerSuite {

  private trait Test {
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    val guidanceErrorPage = injector.instanceOf[runtime_error_template]

    implicit val request = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(request)

    val errorMessageList: List[String] = List("NonTerminatingPageError: Guidance contains non-terminating loop which includes stanza 4")
    val processCode = "processCode"
    val heading = Messages("guidance.error.heading", processCode)
    val pageTitle = Messages("guidance.error.title", processCode)
  }

  "Guidance Error Page" must {

    "Display the correct information" in new Test {
      val html: Html = guidanceErrorPage(heading,pageTitle, None, false, errorMessageList)
      val doc = asDocument(html)

      val errorMessageParagraph = doc.getElementsByTag("p").text
      errorMessageParagraph shouldBe "NonTerminatingPageError: Guidance contains non-terminating loop which includes stanza 4"
    }
  }

}
