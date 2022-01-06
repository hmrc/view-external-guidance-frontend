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

package views.components

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.nodes.Document
import org.jsoup.select.Elements
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest
import play.twirl.api.Html
import models.ui._
import core.models.ocelot.{Labels, LabelCache}
import views.html.components.bullet_point_list
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import base.ViewSpec
import scala.collection.JavaConverters._

class BulletPointListSpec extends ViewSpec with GuiceOneAppPerSuite {

  private trait Test {

    implicit val labels: Labels = LabelCache()
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    val fakeRequest = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(fakeRequest)
    val currencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val page = models.ui.FormPage("/url", currencyInput)
    implicit val ctx = models.PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
  }

  "Bullet point lists" must {

    "Render a simple bullet point list with text elements only" must {

      val leadText: String = "You can buy"
      val bpOne: String = "apples"
      val bpTwo: String = "oranges"
      val bpThree: String = "pears"
      val leadingText: Text = Text(leadText)
      val listItems: Seq[Text] = Seq(Text(bpOne), Text(bpTwo), Text(bpThree))
      val simpleBpList: BulletPointList = BulletPointList(leadingText, listItems)

      "Render simple bullet point list in English" in new Test {

        val markUp: Html = bullet_point_list(simpleBpList)
        val document: Document = Jsoup.parse(markUp.toString())
        // Test leading text
        val paragraph: Element = getSingleElementByTag(document, "p")

        paragraph.hasClass("govuk-body") shouldBe true
        paragraph.text() shouldBe leadText

        // Test list items
        val ul: Element = getSingleElementByTag(document, "ul")

        checkClassesForElement(ul, List("govuk-list", "govuk-list--bullet"))

        val expectedListItems = List(bpOne, bpTwo, bpThree)
        val actualListItems = getMultipleElementsByTag(document, "li", 3).asScala.toList

        assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
      }
    }

    "Render a bullet point list with a link embedded in the leading text" must {

      val leading1: String = "To view your options"
      val linkUrl: String = "http://optionsUrl"
      val linkEnglishText: String = "Click here"
      val leadTextPartTwo: String = "Or just give up"
      val bpOneText: String = "Continue to section A"
      val bpTwoText: String = "Continue to section B"
      val bpLeadingText: Text = Text(leading1) + Text(Link(linkUrl, linkEnglishText)) + Text(leadTextPartTwo)
      val bpListItems: Seq[Text] = Seq(Text(bpOneText), Text(bpTwoText))
      val bpList: BulletPointList = BulletPointList(bpLeadingText, bpListItems)

      "Render bullet point list with embedded link in English" in new Test {

        val markUp: Html = bullet_point_list(bpList)
        val document: Document = Jsoup.parse(markUp.toString())
        // Test leading text
        val paragraph: Element = getSingleElementByTag(document, "p")

        paragraph.hasClass("govuk-body") shouldBe true

        val textNodes = paragraph.textNodes().asScala

        textNodes.length shouldBe 2
        textNodes(0).text().trim shouldBe leading1
        textNodes(1).text().trim shouldBe leadTextPartTwo

        val link: Element = getSingleElementByTag(document, "a")

        checkHyperLink(link, linkUrl, linkEnglishText, false)

        // Test list items
        val ul: Element = getSingleElementByTag(document, "ul")

        checkClassesForElement(ul, List("govuk-list", "govuk-list--bullet"))

        val expectedListItems = List(bpOneText, bpTwoText)
        val actualListItems = getMultipleElementsByTag(document, "li", 2).asScala.toList

        assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
      }
    }

    "Render a bullet point list with links embedded in the list items" must {

      val leadText: String = "Leading text for list"
      val bpOneText: String = "Bullet point one text"
      val bpOneLinkUrl: String = "http://bpOneUrl"
      val bpOneLinkText: String = "Bullet point one link text"
      val bpTwoText: String = "Bullet point two text"
      val bpTwoLinkUrl: String = "http://bpTwoUrl"
      val bpTwoLinkText: String = "Bullet point two link text"
      val bpLeadingText: Text = Text(leadText)
      val bpListItems: Seq[Text] = Seq(Text(bpOneText) + Text(Link(bpOneLinkUrl, bpOneLinkText)),
                                       Text(bpTwoText) + Text(Link(bpTwoLinkUrl, bpTwoLinkText, true)))
      val bpList: BulletPointList = BulletPointList(bpLeadingText, bpListItems)

      "Render the bullet point list with embedded links in English" in new Test {

        val markUp: Html = bullet_point_list(bpList)
        val document: Document = Jsoup.parse(markUp.toString())
        // Test leading paragraph
        val paragraph: Element = getSingleElementByTag(document, "p")

        paragraph.hasClass("govuk-body") shouldBe true

        val textNode = paragraph.textNodes().asScala

        textNode.length shouldBe 1
        textNode(0).text().trim shouldBe leadText

        // Test list items
        val listItems: Elements = getMultipleElementsByTag(document, "li", 2)
        val firstListItem: Element = listItems.first()
        // First list item
        val firstListItemsTextNodes = firstListItem.textNodes().asScala

        firstListItemsTextNodes.length shouldBe 2
        firstListItemsTextNodes(0).text().trim shouldBe bpOneText

        val firstListItemLinks: Elements = firstListItem.getElementsByTag("a")

        firstListItemLinks.size() shouldBe 1
        checkHyperLink(firstListItemLinks.first, bpOneLinkUrl, bpOneLinkText, false)

        // Second link item
        val secondListItem = listItems.last
        val secondListItemTextNodes = secondListItem.textNodes().asScala

        secondListItemTextNodes.length shouldBe 2
        secondListItemTextNodes(0).text().trim shouldBe bpTwoText

        val secondListItemLinks: Elements = secondListItem.getElementsByTag("a")

        secondListItemLinks.size() shouldBe 1
        checkHyperLink(secondListItemLinks.first, bpTwoLinkUrl, bpTwoLinkText, true)
      }
    }
  }
}
