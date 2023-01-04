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

import org.jsoup.nodes.Element

import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest

import play.twirl.api.Html

import models.ui._
import core.models.ocelot.{Labels, LabelCache}
import views.html.components.h1_heading

import org.scalatestplus.play.guice.GuiceOneAppPerSuite

import base.ViewSpec

class H1HeadingSpec extends ViewSpec with GuiceOneAppPerSuite {

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

      val markUp: Html = h1_heading(h1)(messages, ctx)

      val h1Element: Element = getSingleElementByTag(markUp, "h1")

      h1Element.hasClass("govuk-heading-xl") shouldBe true
    }

    "Define the correct GDS reduced class" in new Test {

      val markUp: Html = h1_heading(h1)(messages, ctxReduced)

      val h1Element: Element = getSingleElementByTag(markUp, "h1")

      h1Element.hasClass("govuk-heading-l") shouldBe true
    }

    "display text" in new Test {

      val markUp: Html = h1_heading(h1)(messages, ctx)
      val h1Element: Element = getSingleElementByTag(markUp, "h1")

      h1Element.text() shouldBe h1Str
    }
  }

}
