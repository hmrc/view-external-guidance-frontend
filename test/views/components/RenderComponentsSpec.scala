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
import org.jsoup.nodes.Element
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.twirl.api.Html
import views.html._

import scala.collection.JavaConverters._

class RenderComponentsSpec extends ViewSpec with ViewFns with GuiceOneAppPerSuite {

  private trait Test {
    implicit val labels: Labels = LabelCache()
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    implicit val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(fakeRequest)

    val expectedTable: Table = Table(Text("HELLO","HELLO"),
                              Seq(Text("First","First"), Text("Second","Second")),
                              Seq.fill(3)(Seq(Text("HELLO","HELLO"), Text("World","World"))))

    val currencyInput: CurrencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val page: FormPage = models.ui.FormPage("/url", currencyInput)
    implicit val ctx: PageContext = models.PageContext(page, None, "sessionId", None, Text(), "processId", "processCode", labels)
  }

  private trait WelshTest extends Test {implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))}

  "English render_components" must {

    "Encode table missing caption, with initial row/cell as table caption" in new Test {
      val html: Html = components.render_components(Seq(expectedTable))
      val table: Element = getSingleElementByTag(html, "Table")
      table.getElementsByTag("caption").asScala.toList.headOption.fold(fail){ caption =>
        caption.text shouldBe "HELLO"

        table.hasClass("govuk-table") shouldBe true

        val body = table.getElementsByTag("tbody").first
        val rows = body.getElementsByTag("tr").asScala.toList

        rows.size shouldBe expectedTable.rows.size
      }
    }

    "generate a Details section when required" in new Test {
      val details: Details = Details(Text("Detail header","Welsh Detail header"), Seq(Text("hidden text","Welsh hidden text")))
      val html: Html = components.render_components(Seq(details))
      val detailsSection: Element = getSingleElementByTag(html, "details")

      val header: Element = getSingleElementByTag(detailsSection, "span")
      header.text() shouldBe "Detail header"
      checkClassForElement(header, "govuk-details__summary-text")

      val bodyDiv: Element = getSingleElementByTag(detailsSection, "div")
      bodyDiv.text() shouldBe "hidden text"
      checkClassForElement(bodyDiv, "govuk-details__text")
    }
  }

  "Welsh render_components" must {

    "Encode table missing caption, with initial row/cell as table caption" in new WelshTest {
      val html: Html = components.render_components(Seq(expectedTable))
      val table: Element = getSingleElementByTag(html, "Table")
      table.getElementsByTag("caption").asScala.toList.headOption.fold(fail){ caption =>
        caption.text shouldBe "HELLO"

        table.hasClass("govuk-table") shouldBe true

        val body = table.getElementsByTag("tbody").first
        val rows = body.getElementsByTag("tr").asScala.toList

        rows.size shouldBe expectedTable.rows.size
      }
    }
    "generate a Details section when required" in new WelshTest {
      val details: Details = Details(Text("Detail header","Welsh Detail header"), Seq(Text("hidden text","Welsh hidden text")))
      val html: Html = components.render_components(Seq(details))
      val detailsSection: Element = getSingleElementByTag(html, "details")

      val header: Element = getSingleElementByTag(detailsSection, "span")
      header.text() shouldBe "Welsh Detail header"
      checkClassForElement(header, "govuk-details__summary-text")

      val bodyDiv: Element = getSingleElementByTag(detailsSection, "div")
      bodyDiv.text() shouldBe "Welsh hidden text"
      checkClassForElement(bodyDiv, "govuk-details__text")
    }
  }
}
