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

import models.ui.{H3, Text, SummaryList}
import views.html.components.h3_heading

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import models.ocelot.{Labels, LabelCache}
import base.ViewSpec

class H3HeadingSpec extends ViewSpec with GuiceOneAppPerSuite {

  private trait Test {
    implicit val labels: Labels = LabelCache()
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    val fakeRequest = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(fakeRequest)

    val h3English: String = "Level 3 heading text"
    val h3Welsh: String = "Welsh Level 3 heading text"

    val h3: H3 = H3(Text(h3English, h3Welsh))
    val currencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val summaryList = SummaryList(Seq.empty)
    val page = models.ui.StandardPage("/url", Seq(currencyInput))
    val summaryPage = models.ui.StandardPage("/url", Seq(summaryList))
    val ctx = models.PageContext(page, "sessionId", None, Text(), "processId", "processCode", labels)
    val ctxReduced = models.PageContext(summaryPage, "sessionId", None, Text(), "processId", "processCode", labels)
  }

  private trait WelshTest extends Test {

    implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))

  }

  "Creating a level 3 heading with some content" must {

    "Define the correct GDS standard class" in new Test {

      val markUp: Html = h3_heading(h3)(messages, ctx)

      val h3Element: Element = getSingleElementByTag(markUp, "h3")

      h3Element.hasClass("govuk-heading-m") shouldBe true
    }

    "Define the correct GDS reduced class" in new Test {

      val markUp: Html = h3_heading(h3)(messages, ctxReduced)

      val h3Element: Element = getSingleElementByTag(markUp, "h3")

      h3Element.hasClass("govuk-heading-s") shouldBe true
    }

    "display text in English" in new Test {

      val markUp: Html = h3_heading(h3)(messages, ctx)

      val h3Element: Element = getSingleElementByTag(markUp, "h3")

      h3Element.text() shouldBe h3English
    }

    "display text in Welsh when requested" in new WelshTest {

      val markUp: Html = h3_heading(h3)(messages, ctx)

      val h3Element: Element = getSingleElementByTag(markUp, "h3")

      h3Element.text() shouldBe h3Welsh
    }

  }

}
