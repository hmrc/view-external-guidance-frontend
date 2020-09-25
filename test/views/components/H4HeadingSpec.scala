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

import base.ViewSpec
import models.ui.{H4, Text}
import org.jsoup.nodes.Element
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.twirl.api.Html
import views.html.components.h4_heading

class H4HeadingSpec extends ViewSpec with GuiceOneAppPerSuite {

  private trait Test {
    implicit val labels: Map[String, models.ocelot.Label] = Map()
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(fakeRequest)

    val englishText: String = "English text"
    val welshText: String = "Welsh text"

    val h4: H4 = H4(Text(englishText, welshText))
  }

  private trait WelshTest extends Test {

    implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))

  }

  "Creating a level 4 heading with some content" must {

    "Define the correct GDS class" in new Test {

      val markUp: Html = h4_heading(h4)

      val element: Element = getSingleElementByTag(markUp, "h4")

      element.hasClass("govuk-heading-s") shouldBe true
    }

    "display text in English" in new Test {

      val markUp: Html = h4_heading(h4)

      val element: Element = getSingleElementByTag(markUp, "h4")

      element.text() shouldBe englishText
    }

    "display text in Welsh when requested" in new WelshTest {

      val markUp: Html = h4_heading(h4)

      val element: Element = getSingleElementByTag(markUp, "h4")

      element.text() shouldBe welshText
    }

  }

}
