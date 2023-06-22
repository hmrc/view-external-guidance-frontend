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

import org.jsoup.nodes.Node
import org.jsoup.nodes.{Attributes, Document}
import org.jsoup.select.Elements
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest
import models.ui._
import core.models.ocelot.{LabelCache, Labels}
import views.html.components.confirmation_panel
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import base.{BaseSpec, ViewFns}

import models.PageContext

class ConfirmationPanelSpec extends BaseSpec with ViewFns with GuiceOneAppPerSuite {

  private trait Test {

    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    val fakeRequest = FakeRequest("GET", "/confirmation")

    val page: StandardPage = StandardPage("/confirmation", Nil)

    implicit val labels: Labels = LabelCache()
    implicit val ctx: PageContext = PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
    implicit def messages: Messages = messagesApi.preferred(fakeRequest)


    val titleStr: String = "Title"

    val bodyStr1: String = "First line in body"
    val bodyStr2: String = "Second line in body"

    val title: Text = Text(titleStr)
    val bodyText1: Text =  Text(bodyStr1)
    val bodyText2: Text = Text(bodyStr2)
  }

  "Confirmation panel" must {

    "render a confirmation panel with a single text component" in new Test {

      val confirmationPanel: ConfirmationPanel = ConfirmationPanel(title)

      val doc: Document = asDocument(confirmation_panel(confirmationPanel))

      // Check for confirmation panel
      val panelDivs: Elements = doc.getElementsByClass("govuk-panel")

      panelDivs.size shouldBe 1

      elementAttrs(panelDivs.first)("class").contains("govuk-panel--confirmation") shouldBe true

      // Test heading
      val headings: Elements = panelDivs.first.getElementsByTag("h1")

      headings.size shouldBe 1

      headings.first.text() shouldBe titleStr

      elementAttrs(headings.first)("class").contains("govuk-panel__title") shouldBe true
    }

    "render a confirmation panel with a header and single body text component" in new Test {

      val confirmationPanel: ConfirmationPanel = ConfirmationPanel(title, Seq(bodyText1))

      val doc: Document = asDocument(confirmation_panel(confirmationPanel))

      // Check for confirmation panel
      val panelDivs: Elements = doc.getElementsByClass("govuk-panel")

      panelDivs.size shouldBe 1

      elementAttrs(panelDivs.first)("class").contains("govuk-panel--confirmation") shouldBe true

      // Test heading
      val headings: Elements = panelDivs.first.getElementsByTag("h1")

      headings.size shouldBe 1

      headings.first.text() shouldBe titleStr

      elementAttrs(headings.first)("class").contains("govuk-panel__title") shouldBe true

      // Test body
      val bodyDivs: Elements = panelDivs.first.getElementsByTag("div")

      bodyDivs.size shouldBe 2 // List includes panel division

      bodyDivs.last.text() shouldBe bodyStr1

      elementAttrs(bodyDivs.last)("class").contains("govuk-panel__body")

      val breaks: Elements = bodyDivs.last.getElementsByTag("br")

      breaks.size shouldBe 0
    }

    "render a confirmation panel with multiple body test components" in new Test {

      val confirmationPanel: ConfirmationPanel = ConfirmationPanel(title, Seq(bodyText1, bodyText2))

      val doc: Document = asDocument(confirmation_panel(confirmationPanel))

      // Check for confirmation panel
      val panelDivs: Elements = doc.getElementsByClass("govuk-panel")

      panelDivs.size shouldBe 1

      elementAttrs(panelDivs.first)("class").contains("govuk-panel--confirmation") shouldBe true

      // Test heading
      val headings: Elements = panelDivs.first.getElementsByTag("h1")

      headings.size shouldBe 1

      headings.first.text() shouldBe titleStr

      elementAttrs(headings.first)("class").contains("govuk-panel__title") shouldBe true

      // Test body
      val bodyDivs: Elements = panelDivs.first.getElementsByTag("div")

      bodyDivs.size shouldBe 2 // List includes panel division

      bodyDivs.last.childNodeSize() shouldBe 3

      // Test contents of nodes
      val firstChildNodeAttributes: Attributes = bodyDivs.last.childNode(0).attributes()

      firstChildNodeAttributes.get("#text").trim shouldBe bodyStr1

      val secondChildNode: Node = bodyDivs.last.childNode( 1)

      secondChildNode.outerHtml() shouldBe "<br>"

      val thirdChildNodeAttributes: Attributes = bodyDivs.last.childNode(2).attributes()

      thirdChildNodeAttributes.get("#text").trim shouldBe bodyStr2

      // Check number of line breaks
      val breaks: Elements = bodyDivs.last.getElementsByTag("br")

      breaks.size() shouldBe 1
    }

  }
}
