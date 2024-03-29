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

import base.ViewSpec
import core.models.ocelot.{LabelCache, Labels}
import models.ui._
import org.jsoup.nodes.Element
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.{Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest
import play.twirl.api.Html
import views.html.components.h1_heading_with_label

class H1HeadingWithLabelSpec extends ViewSpec with GuiceOneAppPerSuite {

  private trait Test {
    implicit val labels: Labels = LabelCache()
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    val fakeRequest = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(fakeRequest)

    val h1Str: String = "Level 1 heading text"

    val h1: H1 = H1(Text(h1Str))
    val currencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val summaryList = CyaSummaryList(Seq.empty)
    val page = models.ui.StandardPage("/url", Seq(currencyInput))
    val summaryPage = models.ui.StandardPage("/url", Seq(summaryList))
    val ctx = models.PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
    val ctxReduced = models.PageContext(summaryPage, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
  }

  "Creating a level 1 heading with some content" must {

    "Define the correct GDS standard class" in new Test {

      val markUp: Html = h1_heading_with_label(h1, "field")(ctx)

      val h1Element: Element = getSingleElementByTag(markUp, "h1")
      h1Element.hasClass("govuk-label-wrapper") shouldBe true

      val labelElement: Element = getSingleElementByTag(markUp, "label")
      labelElement.hasClass("govuk-label--xl") shouldBe true
    }

    "Define the correct GDS reduced class" in new Test {

      val markUp: Html = h1_heading_with_label(h1, "field")(ctxReduced)

      val h1Element: Element = getSingleElementByTag(markUp, "h1")
      h1Element.hasClass("govuk-label-wrapper") shouldBe true

      val labelElement: Element = getSingleElementByTag(markUp, "label")
      labelElement.hasClass("govuk-label--l") shouldBe true
    }

    "display text in English" in new Test {

      val markUp: Html = h1_heading_with_label(h1, "field")(ctx)

      val labelElement: Element = getSingleElementByTag(markUp, "label")
      labelElement.text() shouldBe h1Str
    }
  }
}
