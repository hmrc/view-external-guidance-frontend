/*
 * Copyright 2021 HM Revenue & Customs
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

import base.{ViewFns, ViewSpec}
import models.PageContext
import models.ocelot.{LabelCache, Labels}
import models.ui._
import org.jsoup.nodes.{Attributes, Document, Element, Node}
import org.jsoup.select.Elements
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import views.html.components.details

class DetailsSpec extends ViewSpec with ViewFns with GuiceOneAppPerSuite {

  private trait Test {
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/confirmation")
    val page: StandardPage = StandardPage("/confirmation", Nil)
    implicit val labels: Labels = LabelCache()
    implicit val ctx: PageContext = PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
    implicit def messages: Messages = messagesApi.preferred(fakeRequest)
    val titleStr: String = "Title"
    val bodyText1Str: String = "First line in body"
    val bodyText2Str: String = "Second line in body"
    val title: Text = Text(titleStr)
    val bodyText1: Text =  Text(bodyText1Str)
    val bodyText2: Text = Text(bodyText2Str)
  }

  "Details" must {

    "render a details section with a header and single body text component in english" in new Test {

      val detailsComponent: Details = Details(title, Seq(bodyText1))
      val doc: Document = asDocument(details(detailsComponent))

      // Check for details section
      val detailsSection: Element = getSingleElementByTag(doc, "details")
      checkClassForElement(detailsSection, "govuk-details")

      // Check for summary section
      val summarySection: Element = getSingleElementByTag(doc,"summary")
      checkClassForElement(summarySection, "govuk-details__summary")

      // Test heading
      val heading: Element = getSingleElementByTag(summarySection,"span")
      heading.text() shouldBe title.asString
      checkClassForElement(heading, "govuk-details__summary-text")

      // Test body
      val bodyDiv: Element = getSingleElementByTag(detailsSection,"div")
      bodyDiv.text() shouldBe bodyText1.asString
      checkClassForElement(bodyDiv, "govuk-details__text")

      val breaks: Elements = bodyDiv.getElementsByTag("br")
      breaks.size shouldBe 0
    }

    "render a details section with a header and multiple body text components in english" in new Test {

      val detailsComponent: Details = Details(title, Seq(bodyText1, bodyText2))
      val doc: Document = asDocument(details(detailsComponent))
      // Check for details section
      val detailsSection: Element = getSingleElementByTag(doc,"details")
      checkClassForElement(detailsSection, "govuk-details")

      // Check for summary section
      val summarySection: Element = getSingleElementByTag(doc,"summary")
      checkClassForElement(summarySection, "govuk-details__summary")

      // Test heading
      val heading: Element = getSingleElementByTag(summarySection,"span")
      heading.text() shouldBe title.asString
      checkClassForElement(heading, "govuk-details__summary-text")

      // Test body
      val bodyDiv: Element = getSingleElementByTag(detailsSection,"div")
      checkClassForElement(bodyDiv, "govuk-details__text")
      bodyDiv.childNodeSize() shouldBe 3

      // Test contents of nodes
      val firstChildNodeAttributes: Attributes = bodyDiv.childNode(0).attributes()
      firstChildNodeAttributes.get("#text").trim shouldBe bodyText1.asString

      val secondChildNode: Node = bodyDiv.childNode( 1)
      secondChildNode.outerHtml() shouldBe "<br>"

      val thirdChildNodeAttributes: Attributes = bodyDiv.childNode(2).attributes()
      thirdChildNodeAttributes.get("#text").trim shouldBe bodyText2.asString

      // Check number of line breaks
      val breaks: Elements = bodyDiv.getElementsByTag("br")

      breaks.size() shouldBe 1
    }
  }
}
