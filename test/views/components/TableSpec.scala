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

class TableSpec extends ViewSpec with ViewFns with GuiceOneAppPerSuite {

  private trait Test {
    implicit val labels: Labels = LabelCache()
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    implicit val fakeRequest = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(fakeRequest)
    val currencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val page = models.ui.FormPage("/url", currencyInput)
    implicit val ctx = models.PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)

    val expectedTable = Table(Text("HELLO"),
                              Seq(Text("First"), Text("Second")),
                              Seq.fill(3)(Seq(Text("HELLO"), Text("World"))))
    val expectedTableWithNumericCells = Table(Text("HELLO"),
                                              Seq(Text("Caption"), Text("Caption")),
                                              Seq.fill(3)(Seq(Text("HELLO"), Text(LabelRef("Blah", Currency)))))
    val expectedTableWithCaption = expectedTable.copy(caption = Text("Caption"))
    val expectedTableWithTxtCells = Table(Text("HELLO"),
                                          Seq(Text("Caption"), Text("Caption")),
                                          Seq(Seq(Text("HELLO"), Text(LabelRef("Blah", Txt)))))
  }

  "English Tables" must {
    "Encode all bold initial row as table headings with a Caption" in new Test {
      val html: Html = components.table(expectedTableWithCaption)
      val table: Element = getSingleElementByTag(html, "Table")
      table.hasClass("govuk-table") shouldBe true
      val element: Element = table.getElementsByTag("caption").first
      element.hasClass("govuk-table__caption") shouldBe true
      element.text shouldBe expectedTableWithCaption.caption.asString

      val head = table.getElementsByTag("thead").first
      val headings = head.getElementsByTag("th").asScala.toList
      headings.size shouldBe expectedTableWithCaption.headingRow.size

      val body = table.getElementsByTag("tbody").first
      val rows = body.getElementsByTag("tr").asScala.toList

      rows.size shouldBe expectedTable.rows.size
    }

    "Encode table with numeric cells using govuk numeric class" in new Test {
      val html: Html = components.table(expectedTableWithNumericCells)
      val table: Element = getSingleElementByTag(html, "Table")
      table.hasClass("govuk-table") shouldBe true
      val body = table.getElementsByTag("tbody").first
      val rows = body.getElementsByTag("tr").asScala.toList

      rows.size shouldBe expectedTable.rows.size

      (rows zip expectedTableWithNumericCells.rows).foreach{
        case (rowElem, tableRow) =>
          val cells = rowElem.children.asScala.toList
          (cells zip tableRow).foreach{
            case (c, td) if td.isNumericLabelRef =>
              c.hasClass("govuk-table__cell--numeric") shouldBe true

            case (c, tc) =>
              c.hasClass("govuk-table__cell--numeric") shouldBe false
          }
      }
    }

    "Encode table with txt cells without using govuk numeric class" in new Test {
      val html: Html = components.table(expectedTableWithTxtCells)
      val table: Element = getSingleElementByTag(html, "Table")
      table.hasClass("govuk-table") shouldBe true
      val body = table.getElementsByTag("tbody").first
      val rows = body.getElementsByTag("tr").asScala.toList

      rows.size shouldBe expectedTableWithTxtCells.rows.size

      (rows zip expectedTableWithTxtCells.rows).foreach{
        case (rowElem, tableRow) =>
          val cells = rowElem.children.asScala.toList
          (cells zip tableRow).foreach{
            case (c, td) if td.isNumericLabelRef => fail()
            case (c, _) =>
              c.hasClass("govuk-table__cell--numeric") shouldBe false
          }
      }
    }
  }
}
