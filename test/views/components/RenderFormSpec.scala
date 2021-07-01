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
import forms.binders._
import models.PageContext
import core.models.ocelot.{LabelCache, Labels}
import models.ui.{Answer, CurrencyInput, CurrencyPoundsOnlyInput, DateInput, TextInput, FormPage, Question, Text}
import org.jsoup.nodes.Element
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.data.Forms.nonEmptyText
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.twirl.api.Html
import views.html.components

class RenderFormSpec extends ViewSpec with ViewFns with GuiceOneAppPerSuite {
  private trait Test {
    implicit val labels: Labels = LabelCache()
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    val dateFormProvider: DateFormBinder = new DateFormBinder(messagesApi)
    val formProvider: StringFormBinder = new StringFormBinder()

    implicit val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(fakeRequest)
  }

  "English render_form" must {

    "render a text input for a TextInput" in new Test {
      val textInput: TextInput = models.ui.TextInput(Text("Name?"), None, Seq.empty)
      val page: FormPage = models.ui.FormPage("/url", textInput)
      implicit val ctx: PageContext = models.PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
      val html: Html = components.render_form(textInput, "text", formProvider.form("test"))
      val input: Element = getSingleElementByTag(html, "Input")
      input.id() shouldBe "text-0"
    }

    "render a text input for a CurrencyInput" in new Test {
      val currencyInput: CurrencyInput = models.ui.CurrencyInput(Text("Bank balance?"), None, Seq.empty)
      val page: FormPage = models.ui.FormPage("/url", currencyInput)
      implicit val ctx: PageContext = models.PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
      val html: Html = components.render_form(currencyInput, "currency", formProvider.form("test"))
      val input: Element = getSingleElementByTag(html, "Input")
      input.id() shouldBe "currency-0"
    }

    "render a text input for a CurrencyInputPoundsOnly" in new Test {
      val currencyInput: CurrencyPoundsOnlyInput = models.ui.CurrencyPoundsOnlyInput(Text("Bank balance?"), None, Seq.empty)
      val page: FormPage = models.ui.FormPage("/url", currencyInput)
      implicit val ctx: PageContext = models.PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
      val html: Html = components.render_form(currencyInput, "currency", formProvider.form("test"))
      val h1: Element = getSingleElementByTag(html, "H1")
      h1.text() shouldBe "Bank balance?"
      val input: Element = getSingleElementByTag(html, "Input")
      input.id() shouldBe "currency-0"
    }

    "render three text inputs for a DateInput" in new Test {
      val dateInput: DateInput = models.ui.DateInput(Text("Date of birth?"), None, Seq.empty)
      val page: FormPage = models.ui.FormPage("/url", dateInput)
      implicit val ctx: PageContext = models.PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
      val html: Html = components.render_form(dateInput, "dateOfBirth", dateFormProvider.form)
      val h1: Element = getSingleElementByTag(html, "H1")
      h1.text() shouldBe "Date of birth?"
      val day: Option[Element] = getElementById(html, "day")
      day.isDefined shouldBe true
      val month: Option[Element] = getElementById(html, "month")
      month.isDefined shouldBe true
      val year: Option[Element] = getElementById(html, "year")
      year.isDefined shouldBe true
    }

    "render a set of radio buttons for a Question" in new Test {
      val question: Question = models.ui.Question(
        Text("Have bank account?"),
        None,
        Seq.empty,
        Seq(Answer(Text("A1"), None), Answer(Text("A2"), None)))
      val page: FormPage = models.ui.FormPage("/url", question)
      implicit val ctx: PageContext = models.PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
      val html: Html = components.render_form(question, "bankAccount", formProvider.form("test"))
      val h1: Element = getSingleElementByTag(html, "H1")
      h1.text() shouldBe "Have bank account?"
      val radio1: Option[Element] = getElementById(html, "bankAccount-0")
      radio1.isDefined shouldBe true
      radio1.get.`val`() shouldBe "0"
      val radio2: Option[Element] = getElementById(html, "bankAccount-1")
      radio2.isDefined shouldBe true
      radio2.get.`val`() shouldBe "1"
    }
  }
}
