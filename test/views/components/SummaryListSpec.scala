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

import org.jsoup.nodes.Element

import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest

import play.twirl.api.Html

import models.ui._
import core.models.ocelot.{Labels, LabelCache}
import views.html._
import scala.collection.JavaConverters._
import org.scalatestplus.play.guice.GuiceOneAppPerSuite

import base.{ViewFns, ViewSpec}

class SummaryListSpec extends ViewSpec with ViewFns with GuiceOneAppPerSuite {

  private trait Test {
    implicit val labels: Labels = LabelCache()
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    implicit val fakeRequest = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(fakeRequest)

    val h1English: String = "Level 1 heading text"
    val h1Welsh: String = "Welsh Level 1 heading text"
    val dlRows = Seq.fill(3)(Seq(Text("HELLO"), Text("World"), Text("")))
    val expectedDl = CyaSummaryList(dlRows)
    val expectedNameValList = NameValueSummaryList(Seq.fill(3)(Seq(Text("HELLO"), Text("World"))))
    val expectedNameValNumericList = NameValueSummaryList(Seq.fill(3)(Seq(Text("HELLO"), Text("4.0"))))
    val dlRowsWithHint = Seq.fill(3)(Seq(Text("HELLO"), Text("World"), Text("Blah")))
    val expectedDlWithHint = CyaSummaryList(dlRowsWithHint)
    val sparseDlRows = Seq(dlRows(0), Seq(Text("HELLO"), Text(""), Text("")), dlRows(2))
    val expectedDlSparse = CyaSummaryList(sparseDlRows)
    val dlRowsWithLinkAndHint = Seq.fill(3)(Seq(Text("Goodbye"),
                                                Text("World"),
                                                Text.link("dummy-path",
                                                          "Change",
                                                          false,
                                                          false,
                                                          Some("Goodbye"))))

    val expectedDLWithLinkAndHint = CyaSummaryList(dlRowsWithLinkAndHint)

    val currencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val page = models.ui.FormPage("/url", currencyInput)
    implicit val ctx = models.PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
  }

  "Creating CYA Summary list with some content" must {

    "display the correct number of rows and columns" in new Test {
      val html: Html = components.summary_list(expectedDlSparse)
      val dlElement: Element = getSingleElementByTag(html, "dl")

      dlElement.hasClass("govuk-summary-list") shouldBe true
      val rows = dlElement.getElementsByTag("div").asScala.toList
      rows.size shouldBe expectedDlSparse.rows.size
      for( row <- rows ){
        row.hasClass("govuk-summary-list__row") shouldBe true
        row.getElementsByTag("dt").asScala.toList.size shouldBe 1
        row.getElementsByTag("dd").asScala.toList.size shouldBe 2
      }
    }

    "display the correct text in columns" in new Test {
      val html: Html = components.summary_list(expectedDl)
      val dlElement: Element = getSingleElementByTag(html, "dl")
      dlElement.hasClass("govuk-summary-list") shouldBe true
      val rows = dlElement.getElementsByTag("div").asScala.toList

      for( row <- rows ){
        row.hasClass("govuk-summary-list__row") shouldBe true
        row.getElementsByTag("dt").first.text() shouldBe "HELLO"
        val dds = row.getElementsByTag("dd").asScala.toList
        dds.size shouldBe 2
        dds(0).text shouldBe "World"
        dds(1).text shouldBe ""
      }
    }

    "display the correct text in columns with action cell hint as visually hidden text" in new Test {
      val html: Html = components.summary_list(expectedDLWithLinkAndHint)
      val dlElement: Element = getSingleElementByTag(html, "dl")
      dlElement.hasClass("govuk-summary-list") shouldBe true
      val rows = dlElement.getElementsByTag("div").asScala.toList

      for( row <- rows ){
        row.hasClass("govuk-summary-list__row") shouldBe true
        val dds = row.getElementsByTag("dd").asScala.toList
        dds.size shouldBe 2

        val a = dds(1).getElementsByTag("a").first
        a.text shouldBe "Change Goodbye"
      }
    }

  }

  "Creating Name Value Summary list with some content" must {

    "display the correct number of rows and columns" in new Test {
      val html: Html = components.summary_list(expectedNameValList)
      val dlElement: Element = getSingleElementByTag(html, "dl")

      dlElement.hasClass("govuk-summary-list") shouldBe true
      val rows = dlElement.getElementsByTag("div").asScala.toList
      rows.size shouldBe expectedNameValList.rows.size
      for( row <- rows ){
        row.hasClass("govuk-summary-list__row") shouldBe true
        row.getElementsByTag("dt").asScala.toList.size shouldBe 1
        row.getElementsByTag("dd").asScala.toList.size shouldBe 1
      }
    }

    "display the correct text in columns" in new Test {
      val html: Html = components.summary_list(expectedNameValList)
      val dlElement: Element = getSingleElementByTag(html, "dl")
      dlElement.hasClass("govuk-summary-list") shouldBe true
      val rows = dlElement.getElementsByTag("div").asScala.toList

      for( row <- rows ){
        row.hasClass("govuk-summary-list__row") shouldBe true
        row.getElementsByTag("dt").first.text() shouldBe "HELLO"
        val dds = row.getElementsByTag("dd").asScala.toList
        dds.size shouldBe 1
        dds(0).text shouldBe "World"
      }
    }

    "display the correct text in columns with right aligned numeric value column" in new Test {
      val html: Html = components.summary_list(expectedNameValNumericList)
      val dlElement: Element = getSingleElementByTag(html, "dl")
      dlElement.hasClass("govuk-summary-list") shouldBe true
      val rows = dlElement.getElementsByTag("div").asScala.toList

      for( row <- rows ){
        row.hasClass("govuk-summary-list__row") shouldBe true
        val dt = row.getElementsByTag("dt").first
        dt.text() shouldBe "HELLO"
        dt.hasClass("govuk-!-width-three-quarters")
        val dds = row.getElementsByTag("dd").asScala.toList
        dds.size shouldBe 1
        dds(0).hasClass("govuk-!-width-one-quarter")
        dds(0).hasClass("numeric")
      }
    }

  }

}
