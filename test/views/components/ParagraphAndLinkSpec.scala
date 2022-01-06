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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.Injector
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.test.FakeRequest
import views.html.components.{link, link_withHint, paragraph}
import models.ui.{Link, Paragraph, PreviousPageLinkQuery, Text}
import org.jsoup.nodes.{Attributes, Document, Element}

import scala.collection.JavaConverters._
import core.models.ocelot.{LabelCache, Labels}

class ParagraphAndLinkSpec extends AnyWordSpec with Matchers with base.ViewFns with GuiceOneAppPerSuite {

  trait Test {
    implicit val labels: Labels = LabelCache()
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))
    val fakeRequest = FakeRequest("GET", "/")

    val paraText1 = Text("Hello")
    val paraText2 = Text("World")

    val ledePara = Paragraph(paraText1, lede = true)
    val para = Paragraph(paraText1)
    val dest1 = "https://www.bbc.co.uk"
    val dest2 = "https://www.bbc.co.uk/news"
    val link1En = Link(dest1, "The BBC", window = true)
    val link2En = Link(dest2, "BBC News")
    val pageLinkEn = Link(dest2, "BBC News")

    val link1 = Text(link1En)
    val link2 = Text(link2En)
    val pageLink = Text(pageLinkEn)
    val linkWithHint = Text(Link(dest2, "BBC News", false, false, Some("HINT")))

    val paraWithMultipleLinks = Paragraph(paraText1 + link1 + paraText2 + link2 + pageLink + linkWithHint)
    val currencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val page = models.ui.FormPage("/url", currencyInput)
    implicit val ctx = models.PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
  }

  "Paragraph component" should {

    "generate English html containing a lede text paragraph" in new Test {

      val doc = asDocument(paragraph(ledePara))
      val paras = doc.getElementsByTag("p")
      paras.size shouldBe 1
      paras.first.text shouldBe paraText1.asString
      paras.first.classNames.toString shouldBe "[govuk-body-l]"
    }

    "generate English html containing a normal text paragraph" in new Test {

      val doc = asDocument(paragraph(para))
      val paras = doc.getElementsByTag("p")
      paras.size shouldBe 1
      paras.first.text shouldBe paraText1.asString
      paras.first.classNames.toString shouldBe "[govuk-body]"
    }

    "generate English html containing Text and links" in new Test {

      val doc = asDocument(paragraph(paraWithMultipleLinks))
      val paras = doc.getElementsByTag("p")
      paras.size shouldBe 1

      val links = paras.first.getElementsByTag("a").asScala
      links.length shouldBe 4

      val txtNodes = paras.first.textNodes().asScala
      txtNodes.length shouldBe 3
      txtNodes(0).text().trim shouldBe paraText1.asString
      txtNodes(1).text().trim shouldBe paraText2.asString

      val link1Attrs = links(0).attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      link1Attrs.contains("href") shouldBe true
      link1Attrs("href") shouldBe dest1
      link1Attrs.contains("class") shouldBe true
      link1Attrs("class") shouldBe "govuk-link"
      link1Attrs.contains("target") shouldBe true
      link1Attrs("target") shouldBe "_blank"

      val link2Attrs = links(1).attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      link2Attrs.contains("href") shouldBe true
      link2Attrs("href") shouldBe dest2
      link2Attrs.contains("class") shouldBe true
      link2Attrs("class") shouldBe "govuk-link"
      link2Attrs.contains("target") shouldBe false

      val link3Attrs = links(2).attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      link3Attrs.contains("href") shouldBe true
      link3Attrs("href") shouldBe dest2
      link3Attrs.contains("class") shouldBe true
      link3Attrs("class") shouldBe "govuk-link"
      link3Attrs.contains("target") shouldBe false

      val spans = links(3).getElementsByTag("span").asScala.toList
      spans.length shouldBe 2
      elementAttrs(spans(1))("class") shouldBe "govuk-visually-hidden"
      spans(0).text shouldBe "BBC News"
      spans(1).text shouldBe "HINT"

      val link4Attrs = links(3).attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      elementAttrs(links(3))("href") shouldBe dest2
      elementAttrs(links(3))("class") shouldBe "govuk-link"
      elementAttrs(links(3)).get("target") shouldBe None
    }
  }

  "link component" should {

    "render link as button if requested" in new Test {

      val linkAsButton: Link = Link("/guidance/test/page-1", "See page 1", window = false, asButton = true, None)

      val doc: Document = asDocument(link(linkAsButton))

      val links = doc.getElementsByTag("a").asScala.toList

      links.size shouldBe 1

      links.head.text shouldBe "See page 1"

      elementAttrs(links.head)("href") shouldBe "/guidance/test/page-1"
      elementAttrs(links.head)("class") shouldBe "govuk-button"
      elementAttrs(links.head).contains("role") shouldBe true
      elementAttrs(links.head)("role") shouldBe "button"
      elementAttrs(links.head).contains("data-module") shouldBe true
      elementAttrs(links.head)("data-module") shouldBe "govuk-button"

    }
  }

  "link_withHint component" should {

    "render link with previous page by link marker if destination matches back link" in new Test {

      val destination: String = "/guidance/test/page-7"

      override implicit val ctx: models.PageContext = models.PageContext(
        page,
        Seq.empty,
        None,
        "sessionId",
        None,
        Text(),
        "processId",
        "processCode",
        labels,
        Some(destination)
      )

      val testLink: Link = Link(destination, "See destination", window = true)

      val doc: Document = asDocument(link_withHint(testLink, "Something useful"))

      val links = doc.getElementsByTag("a").asScala.toList

      links.size shouldBe 1

      elementAttrs(links.head)("href") shouldBe s"$destination?$PreviousPageLinkQuery"
      elementAttrs(links.head)("class") shouldBe "govuk-link"
      elementAttrs(links.head).contains("target") shouldBe true
      elementAttrs(links.head)("target") shouldBe "_blank"

      val spans = links.head.getElementsByTag("span").asScala.toList

      spans.size shouldBe 2

      spans(0).text shouldBe "See destination"
      spans(1).text shouldBe "Something useful"

      elementAttrs(spans(1))("class") shouldBe "govuk-visually-hidden"
    }

    "render link as button if requested" in new Test {

      val linkAsButton: Link = Link("/guidance/test/page-1", "See page 1", window = false, asButton = true)

      val doc: Document = asDocument(link_withHint(linkAsButton, "Something else"))

      val links: List[Element] = doc.getElementsByTag("a").asScala.toList

      links.size shouldBe 1

      elementAttrs(links.head)("href") shouldBe "/guidance/test/page-1"
      elementAttrs(links.head)("class") shouldBe "govuk-button"
      elementAttrs(links.head).contains("role") shouldBe true
      elementAttrs(links.head)("role") shouldBe "button"
      elementAttrs(links.head).contains("data-module") shouldBe true
      elementAttrs(links.head)("data-module") shouldBe "govuk-button"

      val spans: List[Element] = links.head.getElementsByTag("span").asScala.toList

      spans.size shouldBe 2

      spans(0).text shouldBe "See page 1"
      spans(1).text shouldBe "Something else"

      elementAttrs(spans(1))("class") shouldBe "govuk-visually-hidden"
    }
  }

}
