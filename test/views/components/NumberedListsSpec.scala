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

import base.{ViewFns, ViewSpec}
import core.models.ocelot.{LabelCache, Labels}
import models.ui._
import org.jsoup.nodes.Element
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.{Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest
import play.twirl.api.Html
import views.html.components.{numbered_circle_list, numbered_list}
import scala.jdk.CollectionConverters._

class NumberedListsSpec extends ViewSpec with ViewFns with GuiceOneAppPerSuite {

  private trait Test {
    implicit val labels: Labels = LabelCache()
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    implicit val request = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(request)

    val currencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val page = models.ui.StandardPage("/url", Seq(currencyInput))
    implicit val ctx = models.PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
    val numberedList: NumberedList = NumberedList(Seq(Text("Line1"), Text("Line2")))
    val numberedCircleList: NumberedCircleList = NumberedCircleList(Seq(Text("Line1"), Text("Line2")))
  }

  "English Numbered Lists" must {

    "be rendered as an <ol> with govuk classes and <li> elements" in new Test {
      val html: Html = numbered_list(numberedList)
      val ol: Element = getSingleElementByTag(html, "ol")

      ol.hasClass("govuk-list") shouldBe true
      ol.hasClass("govuk-list--number") shouldBe true

      val lis = ol.getElementsByTag("li").asScala.toList
      lis.length shouldBe 2
      lis(0).text shouldBe "Line1"
      lis(1).text shouldBe "Line2"
    }
  }

  "English Numbered Circle Lists" must {

    "be rendered as an <ol> with govuk classes and <li> elements" in new Test {
      val html: Html = numbered_circle_list(numberedCircleList)
      val ol: Element = getSingleElementByTag(html, "ol")

      ol.hasClass("govuk-list") shouldBe true
      ol.hasClass("govuk-list--number") shouldBe true
      ol.hasClass("steps") shouldBe true

      val lis = ol.getElementsByTag("li").asScala.toList
      lis.length shouldBe 2
      lis(0).text shouldBe "Line1"
      lis(1).text shouldBe "Line2"
    }
  }
}
