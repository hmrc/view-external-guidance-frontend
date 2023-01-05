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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.data.Form
import play.api.data.Forms._
import play.api.inject.Injector
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.mvc.Request
import play.api.test.FakeRequest
import views.html._
import forms.{SubmittedDateAnswerFormProvider, SubmittedListAnswerFormProvider, SubmittedTextAnswerFormProvider}
import models.PageContext
import models.ui.{CurrencyInput, DateInput, FormPage, RequiredErrorMsg, Sequence, SubmittedDateAnswer, SubmittedListAnswer, SubmittedTextAnswer, Text, TypeErrorMsg, ValueErrorMsg}
import base.ViewFns
import org.jsoup.nodes.{Document, Element}
import play.api.data.FormBinding.Implicits._

import scala.collection.JavaConverters._

class ErrorSpec extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with ViewFns {

  trait Test {

    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))

    val path: String = "/guidanceInput"
    val relativePath: String = "guidanceInput"

    val dayAnswer: String = "9"
    val monthAnswer: String = "11"
    val yearAnswer: String = "2017"

    val requiredSelectionError: String = "Please make a selection"
    val requiredDateComponentError: String = "Parts of the date are absent"
    val valueError: String = "Please enter a valid amount"

    val dateAnswerFormProvider: SubmittedDateAnswerFormProvider = new SubmittedDateAnswerFormProvider()
    val listAnswerFormProvider: SubmittedListAnswerFormProvider = new SubmittedListAnswerFormProvider()
    val textAnswerFormProvider: SubmittedTextAnswerFormProvider = new SubmittedTextAnswerFormProvider()
    
    val currencyInput: CurrencyInput = CurrencyInput(Text(), None, Seq.empty)
    val page: FormPage = FormPage("/url", currencyInput)
    implicit val ctx: PageContext = PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode")

    val requiredErrMsgs: Seq[RequiredErrorMsg] = Seq(RequiredErrorMsg(Text(requiredSelectionError)))
    val requiredDateComponentsErrorMsgs: Seq[RequiredErrorMsg] = Seq(RequiredErrorMsg(Text(requiredDateComponentError)))
    val valueErrorMsgs: Seq[ValueErrorMsg] = Seq(ValueErrorMsg(Text(valueError)))

    val currencyInputWithErrors: CurrencyInput = CurrencyInput(Text(), None, Seq.empty, valueErrorMsgs)
    val currencyInputWithErrorsPage: FormPage = FormPage("/currencyInput", currencyInputWithErrors)

    implicit val currencyInputWithErrorsCtx: PageContext = PageContext(
      currencyInputWithErrorsPage, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode"
    )

    val sequenceWithErrors: Sequence = Sequence(Text(), None, Seq.empty, None, Seq.empty, requiredErrMsgs)
    val sequenceWithErrorsPage: FormPage = FormPage("/sequenceInput", sequenceWithErrors)

    implicit val sequenceWithErrorsCtx: PageContext = PageContext(
      sequenceWithErrorsPage, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode"
    )

    val dateInputWithErrors: DateInput = DateInput(Text(), None, Seq.empty, requiredDateComponentsErrorMsgs)
    val dateInputWithErrorsPage: FormPage = FormPage("/dateInput", dateInputWithErrors)

    implicit val dateInputWithErrorsCtx: PageContext = PageContext(
      dateInputWithErrorsPage, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode"
    )

  }

  "error_summary" must {

    "render error header and list of messages for currency input with value error" in new Test {

      val form: Form[SubmittedTextAnswer] = textAnswerFormProvider(relativePath -> nonEmptyText)

      val doc: Document = asDocument(components.error_summary(currencyInputWithErrors, relativePath, form)
      (messages, currencyInputWithErrorsCtx))

      val head: Element = doc.getElementById("error-summary-title")

      head.text() shouldBe messages("error.summary.title")

      val lists: List[Element] = doc.getElementsByTag("ul").asScala.toList

      lists.size shouldBe 1

      val listItems: List[Element] = lists.head.getElementsByTag("li").asScala.toList

      listItems.size shouldBe 1

      listItems.head.text() shouldBe valueError

      // Check link on error message
      val anchors: List[Element] = listItems.head.getElementsByTag("a").asScala.toList

      anchors.size shouldBe 1

      elementAttrs(anchors.head)("href") shouldBe s"#$relativePath-0"
    }

    "render error message with link when invalid data input to sequence" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody()

      val form:Form[SubmittedListAnswer] = listAnswerFormProvider(relativePath)

      form.bindFromRequest().fold(
        formWithErrors => {

          val doc: Document = asDocument(components.error_summary(sequenceWithErrors, relativePath, formWithErrors)
          (messages, sequenceWithErrorsCtx))

          val lists: List[Element] = doc.getElementsByTag("ul").asScala.toList

          lists.size shouldBe 1

          val listItems: List[Element] = lists.head.getElementsByTag("li").asScala.toList

          listItems.size shouldBe 1

          listItems.head.text() shouldBe requiredSelectionError

          // Check link on error message
          val anchors: List[Element] = listItems.head.getElementsByTag("a").asScala.toList

          anchors.size shouldBe 1

          elementAttrs(anchors.head)("href") shouldBe s"#$relativePath-0"
        },
        formData => fail(s"Binding of empty request should cause form binding to fail. Returned form data : $formData")
      )
    }

    "render error message and define link for date input with missing day" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody("month" -> monthAnswer, "year" -> yearAnswer)

      val form: Form[SubmittedDateAnswer] = dateAnswerFormProvider()

      form.bindFromRequest().fold(
        formWithErrors => {

          val doc: Document = asDocument(components.error_summary(dateInputWithErrors, relativePath, formWithErrors)
          (messages, dateInputWithErrorsCtx))

          val lists: List[Element] = doc.getElementsByTag("ul").asScala.toList

          lists.size shouldBe 1

          val listItems: List[Element] = lists.head.getElementsByTag("li").asScala.toList

          listItems.size shouldBe 1

          listItems.head.text() shouldBe requiredDateComponentError

          // Check link on error message
          val anchors: List[Element] = listItems.head.getElementsByTag("a").asScala.toList

          anchors.size shouldBe 1

          elementAttrs(anchors.head)("href") shouldBe "#day"

        },
        formData => fail(s"Binding of empty request should cause form binding to fail. Returned form data : $formData")
      )

    }

    "render error message and define link for date input with missing month" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody("day" -> dayAnswer, "year" -> yearAnswer)

      val form: Form[SubmittedDateAnswer] = dateAnswerFormProvider()

      form.bindFromRequest().fold(
        formWithErrors => {

          val doc: Document = asDocument(components.error_summary(dateInputWithErrors, relativePath, formWithErrors)
          (messages, dateInputWithErrorsCtx))

          val lists: List[Element] = doc.getElementsByTag("ul").asScala.toList

          val listItems: List[Element] = lists.head.getElementsByTag("li").asScala.toList

          listItems.head.text() shouldBe requiredDateComponentError

          // Check link on error message
          val anchors: List[Element] = listItems.head.getElementsByTag("a").asScala.toList

          elementAttrs(anchors.head)("href") shouldBe "#month"

        },
        formData => fail(s"Binding of empty request should cause form binding to fail. Returned form data : $formData")
      )

    }

    "render error message and define link for date input with missing year" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody("day" -> dayAnswer, "month" -> monthAnswer)

      val form: Form[SubmittedDateAnswer] = dateAnswerFormProvider()

      form.bindFromRequest().fold(
        formWithErrors => {

          val doc: Document = asDocument(components.error_summary(dateInputWithErrors, relativePath, formWithErrors)
          (messages, dateInputWithErrorsCtx))

          val lists: List[Element] = doc.getElementsByTag("ul").asScala.toList

          val listItems: List[Element] = lists.head.getElementsByTag("li").asScala.toList

          listItems.head.text() shouldBe requiredDateComponentError

          // Check link on error message
          val anchors: List[Element] = listItems.head.getElementsByTag("a").asScala.toList

          elementAttrs(anchors.head)("href") shouldBe "#year"

        },
        formData => fail(s"Binding of empty request should cause form binding to fail. Returned form data : $formData")
      )

    }

    "render error message and define link for date input with missing day and month" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody("year" -> yearAnswer)

      val form: Form[SubmittedDateAnswer] = dateAnswerFormProvider()

      form.bindFromRequest().fold(
        formWithErrors => {

          val doc: Document = asDocument(components.error_summary(dateInputWithErrors, relativePath, formWithErrors)
          (messages, dateInputWithErrorsCtx))

          val lists: List[Element] = doc.getElementsByTag("ul").asScala.toList

          val listItems: List[Element] = lists.head.getElementsByTag("li").asScala.toList

          listItems.head.text() shouldBe requiredDateComponentError

          // Check link on error message
          val anchors: List[Element] = listItems.head.getElementsByTag("a").asScala.toList

          elementAttrs(anchors.head)("href") shouldBe "#day"

        },
        formData => fail(s"Binding of empty request should cause form binding to fail. Returned form data : $formData")
      )

    }

    "render error message and define link for date input with missing month and year" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody("day" -> dayAnswer)

      val form: Form[SubmittedDateAnswer] = dateAnswerFormProvider()

      form.bindFromRequest().fold(
        formWithErrors => {

          val doc: Document = asDocument(components.error_summary(dateInputWithErrors, relativePath, formWithErrors)
          (messages, dateInputWithErrorsCtx))

          val lists: List[Element] = doc.getElementsByTag("ul").asScala.toList

          val listItems: List[Element] = lists.head.getElementsByTag("li").asScala.toList

          listItems.head.text() shouldBe requiredDateComponentError

          // Check link on error message
          val anchors: List[Element] = listItems.head.getElementsByTag("a").asScala.toList

          elementAttrs(anchors.head)("href") shouldBe "#month"

        },
        formData => fail(s"Binding of empty request should cause form binding to fail. Returned form data : $formData")
      )

    }

    "render error message and define link for date input with missing day and year" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody("month" -> monthAnswer)

      val form: Form[SubmittedDateAnswer] = dateAnswerFormProvider()

      form.bindFromRequest().fold(
        formWithErrors => {

          val doc: Document = asDocument(components.error_summary(dateInputWithErrors, relativePath, formWithErrors)
          (messages, dateInputWithErrorsCtx))

          val lists: List[Element] = doc.getElementsByTag("ul").asScala.toList

          val listItems: List[Element] = lists.head.getElementsByTag("li").asScala.toList

          listItems.head.text() shouldBe requiredDateComponentError

          // Check link on error message
          val anchors: List[Element] = listItems.head.getElementsByTag("a").asScala.toList

          elementAttrs(anchors.head)("href") shouldBe "#day"

        },
        formData => fail(s"Binding of empty request should cause form binding to fail. Returned form data : $formData")
      )

    }

    "render error message and define link for date input with missing day month, and year" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody()

      val form: Form[SubmittedDateAnswer] = dateAnswerFormProvider()

      form.bindFromRequest().fold(
        formWithErrors => {

          val doc: Document = asDocument(components.error_summary(dateInputWithErrors, relativePath, formWithErrors)
          (messages, dateInputWithErrorsCtx))

          val lists: List[Element] = doc.getElementsByTag("ul").asScala.toList

          val listItems: List[Element] = lists.head.getElementsByTag("li").asScala.toList

          listItems.head.text() shouldBe requiredDateComponentError

          // Check link on error message
          val anchors: List[Element] = listItems.head.getElementsByTag("a").asScala.toList

          elementAttrs(anchors.head)("href") shouldBe "#day"

        },
        formData => fail(s"Binding of empty request should cause form binding to fail. Returned form data : $formData")
      )

    }

    "render error message and link for date input with invalid day" in new Test {

      val invalidDayError: String = "The value of the day is invalid"
      val invalidDayErrorMsgs: Seq[TypeErrorMsg] = Seq(TypeErrorMsg(Text(invalidDayError)))

      val dateInputWithInvalidDay: DateInput = DateInput(Text(), None, Seq.empty, invalidDayErrorMsgs)
      val dateInputWithInvalidDayPage: FormPage = FormPage("/dateInput",  dateInputWithInvalidDay)

      implicit val dateInputWithInvalidDayCtx: PageContext = PageContext(
        dateInputWithInvalidDayPage, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode"
      )

      val form: Form[SubmittedDateAnswer] = dateAnswerFormProvider()

      val doc: Document = asDocument(components.error_summary(dateInputWithInvalidDay, relativePath, form)
      (messages, dateInputWithInvalidDayCtx))

      val lists: List[Element] = doc.getElementsByTag("ul").asScala.toList

      val listItems: List[Element] = lists.head.getElementsByTag("li").asScala.toList

      listItems.head.text() shouldBe invalidDayError

      // Check link on error message
      val anchors: List[Element] = listItems.head.getElementsByTag("a").asScala.toList

      elementAttrs(anchors.head)("href") shouldBe "#day"

    }

  }

  "error_message" must {

    "render error message" in new Test {
      val doc: Document = asDocument(components.error_message(requiredErrMsgs)(messages, ctx))
      val p: List[Element] = doc.getElementsByTag("p").asScala.toList.filter(_.id == "required-error")

      p.head.text() shouldBe messages("error.browser.title.prefix") + " " + requiredSelectionError
    }

    "render hidden text with error message" in new Test {

      val doc: Document = asDocument(components.error_message(requiredErrMsgs)(messages, ctx))
      val hidden: List[Element] = doc.getElementsByClass("govuk-visually-hidden").asScala.toList

      hidden.head.text() shouldBe messages("error.browser.title.prefix")
    }

  }

}
