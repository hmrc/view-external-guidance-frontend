/*
 * Copyright 2020 HM Revenue & Customs
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
import models.ocelot.{Labels, LabelCache}
import views.html._
import scala.collection.JavaConverters._
import org.scalatestplus.play.guice.GuiceOneAppPerSuite

import base.{ViewFns, ViewSpec}

class TableSpec extends ViewSpec with ViewFns with GuiceOneAppPerSuite {

  private trait Test {
    implicit val labels: Labels = LabelCache()
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    implicit val fakeRequest = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(fakeRequest)

    val h1English: String = "Level 1 heading text"
    val h1Welsh: String = "Welsh Level 1 heading text"
    val dlRows: Seq[Seq[Text]] = Seq(Seq(Text(Words("HELLO", true),Words("HELLO", true)), Text(Words("World", true),Words("World", true)))) ++
                                 Seq.fill(3)(Seq(Text("HELLO","HELLO"), Text("World","World")))

    val tableBody: Seq[Seq[Cell]] = Seq.fill(3)(Seq(Td(Text("HELLO","HELLO")), Td(Text("World","World"))))
    val expectedTable = Table(None,
                              Some(Seq(Th(Text("HELLO","HELLO")), Th(Text("World","World")))),
                              tableBody)
    val expectedTableWithCaption = expectedTable.copy(caption = Some(Text("Caption", "Caption")))

    val dlRowsWithHint = Seq.fill(3)(Seq(Text("HELLO","HELLO"), Text("Blah","Blah")))
    val expectedDlWithHint = SummaryList(dlRowsWithHint)
    val sparseDlRows = Seq(dlRows(0), Seq(Text("HELLO","HELLO"), Text("",""), Text("","")), dlRows(2))
    val expectedDlSparse = SummaryList(sparseDlRows)
    val dlRowsWithLinkAndHint = Seq.fill(3)(Seq(Text("Goodbye","Goodbye"),
                                                Text("World","World"),
                                                Text.link("dummy-path",
                                                          Vector("Change", "Change"),
                                                          false,
                                                          false,
                                                          Some(Vector("Goodbye", "Goodbye")))))
    val expectedDLWithLinkAndHint = SummaryList(dlRowsWithLinkAndHint)

    val currencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val page = models.ui.InputPage("/url", currencyInput)
    implicit val ctx = models.PageContext(page, "sessionId", None, Text(), "processId", "processCode", labels)
  }

  private trait WelshTest extends Test {implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))}

  "Tables" must {

    "Encode all bold intial row as table headings" in new Test {
      val html: Html = components.table(expectedTable)
      val table: Element = getSingleElementByTag(html, "Table")
      table.getElementsByTag("caption").asScala.toList.isEmpty shouldBe true
      table.hasClass("govuk-table") shouldBe true
      val head = table.getElementsByTag("thead").first
      val headings = head.getElementsByTag("th").asScala.toList
      expectedTable.headingRow.fold(succeed){row =>
        headings.size shouldBe row.size
      }

      val body = table.getElementsByTag("tbody").first
      val rows = body.getElementsByTag("tr").asScala.toList

      rows.size shouldBe expectedTable.rows.size
    }

    "Encode all bold intial row as table headings with a Caption" in new Test {
      val html: Html = components.table(expectedTableWithCaption)
      val table: Element = getSingleElementByTag(html, "Table")
      table.hasClass("govuk-table") shouldBe true
      expectedTableWithCaption.caption.fold(fail){caption =>
        val element: Element = table.getElementsByTag("caption").first
        element.hasClass("govuk-table__caption") shouldBe true
        element.text shouldBe caption.asString(messages.lang)
      }

      val head = table.getElementsByTag("thead").first
      val headings = head.getElementsByTag("th").asScala.toList
      expectedTableWithCaption.headingRow.fold(succeed){row =>
        headings.size shouldBe row.size
      }

      val body = table.getElementsByTag("tbody").first
      val rows = body.getElementsByTag("tr").asScala.toList

      rows.size shouldBe expectedTable.rows.size
    }

    // "display the correct text in columns" in new Test {
    //   val html: Html = components.summary_list(expectedDl)
    //   val dlElement: Element = getSingleElementByTag(html, "dl")
    //   dlElement.hasClass("govuk-summary-list") shouldBe true
    //   val rows = dlElement.getElementsByTag("div").asScala.toList

    //   for( row <- rows ){
    //     row.hasClass("govuk-summary-list__row") shouldBe true
    //     row.getElementsByTag("dt").first.text() shouldBe "HELLO"
    //     val dds = row.getElementsByTag("dd").asScala.toList
    //     dds.size shouldBe 2
    //     dds(0).text shouldBe "World"
    //     dds(1).text shouldBe ""
    //   }
    // }

    // "display the correct text in columns with action cell hint as visually hidden text" in new Test {
    //   val html: Html = components.summary_list(expectedDLWithLinkAndHint)
    //   val dlElement: Element = getSingleElementByTag(html, "dl")
    //   dlElement.hasClass("govuk-summary-list") shouldBe true
    //   val rows = dlElement.getElementsByTag("div").asScala.toList

    //   for( row <- rows ){
    //     row.hasClass("govuk-summary-list__row") shouldBe true
    //     val dds = row.getElementsByTag("dd").asScala.toList
    //     dds.size shouldBe 2

    //     val a = dds(1).getElementsByTag("a").first
    //     a.text shouldBe "ChangeGoodbye"
    //   }
    // }

  }

}
