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

import models.PageContext
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.Injector
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.test.FakeRequest
import play.twirl.api.Html
import org.jsoup.Jsoup
import views.html.components.{h1_heading, h2_heading, h3_heading, paragraph}
import core.models.ocelot.{Label, ScalarLabel, LabelCache, Labels}
import models.ui.{Currency, CurrencyInput, CurrencyPoundsOnly, DateStandard, FormPage, H1, H2, H3}
import models.ui.{Link, Number, Paragraph, Text, TextItem, Txt, Words}
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements
import play.api.mvc.AnyContentAsEmpty

class RenderTextSpec extends AnyWordSpec with Matchers with GuiceOneAppPerSuite {

  def asDocument(html: Html): Document = Jsoup.parse(html.toString)

  trait Test {
    implicit val labels: Labels = LabelCache(Map("Blah" -> ScalarLabel("Blah", List("a value")),
                                                 "A-Label" -> ScalarLabel("A-Label", List("33.9")),
                                                 "Text-Label" -> ScalarLabel("Text-Label", List("text string")),
                                                 "Date-Label" -> ScalarLabel("Date-Label", List("29/2/2020")),
                                                 "BigNumber" -> ScalarLabel("BigNumber", List("12345678")),
                                                 "BigNumberDps" -> ScalarLabel("BigNumber", List("12345678.45"))))
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))
    val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")
    val boldText: Text = Text(Words("Hello", bold = true))
    val normalText: Text = Text(Words("Hello"))
    val textWithBold: Text = Text(Seq(Words("A label must have "),Words("Blah", bold = true), Words(" price")))
    val currencyInput: CurrencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val page: FormPage = models.ui.FormPage("/url", currencyInput)
    implicit val ctx: PageContext = models.PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
  }

  "render_text component" should {

    "generate English html containing bold text with default output formatting" in new Test {
      val doc: Document = asDocument(paragraph(Paragraph(textWithBold))(messages, ctx))
      val p: Element = doc.getElementsByTag("p").first
      val boldElement: Element = p.getElementsByTag("strong").first
      boldElement.text shouldBe "Blah"
    }

    "generate English html containing normal text" in new Test {
      val doc: Document = asDocument(paragraph(Paragraph(normalText)))
      val strong: Elements = doc.getElementsByTag("strong")
      strong.size shouldBe 0
    }

    "generate English html containing an H1" in new Test {
      val doc: Document = asDocument(h1_heading(H1(normalText)))
      val h1: Elements = doc.getElementsByTag("h1")
      h1.size shouldBe 1
    }

    "generate English html containing an H2" in new Test {
      val doc: Document = asDocument(h2_heading(H2(normalText)))
      val h2: Elements = doc.getElementsByTag("h2")
      h2.size shouldBe 1
    }

    "generate English html containing an H3" in new Test {
      val doc: Document = asDocument(h3_heading(H3(normalText)))
      val h3: Elements = doc.getElementsByTag("h3")
      h3.size shouldBe 1
    }

    "generate English html containing a normal text paragraph" in new Test {
      val doc: Document = asDocument(paragraph(Paragraph(boldText)))
      val strong: Elements = doc.getElementsByTag("strong")
      strong.size shouldBe 1
    }

    "test links with text to ensure correct spacing" in new Test {
      val link1: Link = Link("this/is/a/link", "Link Text")
      val words1: Words = Words("this is the first section ")
      val words2: Words = Words(", second section should follow link text.")
      val phrases1: Seq[TextItem] = Seq(words1, link1, words2)
      val text: Text = Text(phrases1)
      val doc: Document = asDocument(paragraph(Paragraph(text)))
      val pTag: Elements = doc.getElementsByTag("p")

      pTag.text() shouldBe "this is the first section Link Text, second section should follow link text."
    }
  }
}
