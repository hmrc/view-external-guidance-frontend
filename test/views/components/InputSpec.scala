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

import forms.FormProvider
import forms.providers.{DateFormProvider, StringFormProvider}
import core.models.ocelot.{Label, LabelCache, Labels, Phrase}
import models.ui.{BulletPointList, CurrencyInput, DateInput, FormPage, H2, H3, H4, Input, NumberInput}
import models.ui.{Paragraph, RequiredErrorMsg, SubmittedDateAnswer, Text, TextInput, ValueErrorMsg,SubmittedTextAnswer}
import org.jsoup._
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.data.Form
import play.api.data.Forms.nonEmptyText
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.mvc.{AnyContentAsEmpty, Request}
import play.api.test.FakeRequest
import play.twirl.api.Html
import views.html._
import play.api.data.FormBinding.Implicits._

import scala.collection.JavaConverters._
import models.PageContext

class InputSpec extends AnyWordSpec with Matchers with GuiceOneAppPerSuite {

  def asDocument(html: Html): Document = Jsoup.parse(html.toString)

  def elementAttrs(el: Element): Map[String, String] = el.attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap

  trait Test {
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    def textFormProvider: StringFormProvider = new StringFormProvider
    def dateFormProvider: DateFormProvider = new DateFormProvider
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))
    val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")
    private val para1Text = Text("This is a question")
    protected val para1 = Paragraph(para1Text)

    val i1 = "Enter main residence value"
    val i1Hint = "This is where they lived"

    val inputValue1: String = "200"
    private val leading = Text("You can buy")
    private val bp1 = Text("apples")
    private val bp2 = Text("oranges")
    private val bp3 = Text("pears")
    private val bpList: BulletPointList = BulletPointList(leading, Seq(bp1, bp2, bp3))

    protected val h2: H2 = H2(Text("h2"))
    protected val h3: H3 = H3(Text("h3"))
    protected val h4: H4 = H4(Text("h4"))
    val inputPhrase: Phrase = Phrase(Vector("Some Text", "Welsh: Some Text"))
    val helpPhrase: Phrase = Phrase(Vector("Help text", "Welsh: Help text"))

    val input: Input = CurrencyInput(Text(i1), Some(Text(i1Hint)), Seq(h2, h3, h4, bpList, para1), Nil, false, "5")

    val inputWithoutBody: Input = CurrencyInput(Text(i1), None, Seq.empty)
    val inputWithHintAndNoBody: Input = CurrencyInput(Text(i1), Some(Text(i1Hint)), Seq.empty)
    protected val errorMsg = RequiredErrorMsg(Text("An error has occurred"))
    protected val valueErrorMsg = ValueErrorMsg(Text("A value error has occurred"))
    val inputWithErrors: Input = CurrencyInput(Text(i1), None, Seq.empty, Seq(errorMsg))
    val inputWithHint: Input = CurrencyInput(Text(i1), Some(Text(i1Hint)), Seq(bpList, para1), Seq.empty, true)
    val inputWithHintAndErrors: Input = CurrencyInput(Text(i1), Some(Text(i1Hint)), Seq(bpList, para1), Seq(errorMsg))
    implicit val labels: Labels = LabelCache()
    val currencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val currencyPoundsInput = models.ui.CurrencyPoundsOnlyInput(Text(), None, Seq.empty)
    val page = models.ui.FormPage("/url", currencyInput)
    val pagePo = models.ui.FormPage("/url", currencyPoundsInput)
    val ctx = PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
    val ctxPo = PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
  }

  "English Currency Input components" must {

    "render input text as a header" in new Test {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val heading = doc.getElementsByTag("h1")
      heading.size shouldBe 1
      heading.first.text() shouldBe i1
    }

    "render contained paragraphs" in new Test {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test"))(fakeRequest, messages, ctx))

      doc.getElementsByTag("p").asScala.toList.foreach { p =>
        elementAttrs(p)("class").contains("govuk-body") shouldBe true
      }
    }

    "render input as input field" in new Test {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val inputFields = doc.getElementsByTag("input")
      inputFields.size shouldBe 1
    }

    "render input as input field of correct width" in new Test {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val inputFields = doc.getElementsByTag("input")
      inputFields.size shouldBe 1
      private val attrs = elementAttrs(inputFields.first())
      attrs("class").contains("govuk-input--width-5") shouldBe true
    }

    "render input with correct mode" in new Test {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val inputFields = doc.getElementsByTag("input")
      inputFields.size shouldBe 1
      private val attrs = elementAttrs(inputFields.first())
      attrs("inputmode").contains("decimal") shouldBe true
      private val docPo = asDocument(components.input(currencyPoundsInput, "test", textFormProvider("test"))(fakeRequest, messages, ctxPo))
      private val inputFieldsPo = docPo.getElementsByTag("input")
      inputFieldsPo.size shouldBe 1
      private val attrsPo = elementAttrs(inputFieldsPo.first())
      attrsPo("inputmode").contains("numeric") shouldBe true

    }

    "render label for input when input component body is not empty" in new Test {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val htmlLabels: Elements = doc.getElementsByTag("label")

      htmlLabels.size shouldBe 1

      htmlLabels.first.text() shouldBe input.text.asString
    }

    "render input with previous answer entered" in new Test {
      private val form = textFormProvider("test").bind(Map("test" -> inputValue1))
      private val doc = asDocument(components.input(input, "test", form)(fakeRequest, messages, ctx))
      private val inputs = doc.getElementsByTag("input").asScala.toList
      inputs.size shouldBe 1
      private val inputField = elementAttrs(inputs.head)
      inputField("value") shouldBe inputValue1
    }

    "render input with hints" in new Test {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val hints = doc.getElementsByTag("div").asScala.toList
      private val firstHint = hints(1)
      private val hint1Attrs = elementAttrs(firstHint)
      hint1Attrs("class") shouldBe "govuk-hint"
      firstHint.text() shouldBe i1Hint
    }

    "Input with no body should have a label wrapper class on H1" in new Test {
      private val doc = asDocument(components.input(inputWithoutBody, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val h1 = doc.getElementsByTag("h1").first
      private val attrs = elementAttrs(h1)
      attrs("class").contains("govuk-label-wrapper") shouldBe true
    }

    "Input modified with [norepeat] with body should have a label wrapper class on H1" in new Test {
      private val doc = asDocument(components.input(inputWithHint, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val h1 = doc.getElementsByTag("h1").first
      private val attrs = elementAttrs(h1)
      attrs("class").contains("govuk-label-wrapper") shouldBe true
    }

    "input without body should not contain an Html label element with class govuk-label--m" in new Test {
      private val doc = asDocument(components.input(inputWithoutBody, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val htmlLabels: Elements = doc.getElementsByTag("label")

      htmlLabels.size shouldBe 1

      val attrs: Map[String, String] = elementAttrs(htmlLabels.first())
      attrs("class").contains("govuk-label--m") shouldBe false
    }

    "input without body should render hint within a span without a fieldset" in new Test {
      private val doc = asDocument(components.input(inputWithHintAndNoBody, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val fieldset = doc.getElementsByTag("fieldset").first

      Option(fieldset) shouldBe None

      Option(doc.getElementById("input-hint")).fold(fail("Missing hint span")) { span =>
        val attrs = elementAttrs(span)
        attrs("id") shouldBe "input-hint"
        attrs("class").contains("govuk-hint") shouldBe true
        span.text shouldBe i1Hint
      }
    }

   "input with hint in error should include hint id and error id in aria-describedby on input" in new Test {
      private val doc = asDocument(components.input(inputWithHintAndErrors, "test", textFormProvider("test"))(fakeRequest, messages, ctx))

      doc.getElementsByTag("input").asScala.toList.foreach { inp =>
        elementAttrs(inp).get("aria-describedby").fold(fail("Missing aria-describedby")){ aria =>
          aria should include("input-hint")
          aria should include("required-error")
        }
      }
   }

    "input with errors should have error class assigned" in new Test {

      private val doc = asDocument(components.input(inputWithErrors, "test", textFormProvider("test"))(fakeRequest, messages, ctx))

      doc.getElementsByTag("input").asScala.toList.foreach { inp =>
        val attrs: Map[String, String] = elementAttrs(inp)
        attrs("class").contains("govuk-input") shouldBe true
        attrs("class").contains("govuk-input--width-10") shouldBe true
        attrs("class").contains("govuk-input--error") shouldBe true
      }
    }

  }

  "English Text Input component" must {

    trait TextTest extends Test {
      override val input: TextInput = TextInput(Text(i1), Some(Text(i1Hint)), Seq(h2, h3, h4, para1), Nil, false, "5")

      override val inputWithoutBody: TextInput = TextInput(Text(i1), None, Seq.empty)
      override val inputWithHintAndNoBody: TextInput = TextInput(Text(i1), Some(Text(i1Hint)), Seq.empty)
      override val inputWithErrors: TextInput = TextInput(Text(i1), None, Seq.empty, Seq(errorMsg))
      override val inputWithHint: TextInput = TextInput(Text(i1), Some(Text(i1Hint)), Seq(para1), Seq.empty, true)
      override val inputWithHintAndErrors: TextInput = TextInput(Text(i1), Some(Text(i1Hint)), Seq(para1), Seq(errorMsg))
      val textInput: TextInput = models.ui.TextInput(Text(), None, Seq.empty)
      val textInputPage: FormPage = models.ui.FormPage("/url", textInput)
      override val ctx = PageContext(textInputPage, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
    }

    "render input text as a header" in new TextTest {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val heading = doc.getElementsByTag("h1")
      heading.size shouldBe 1
      heading.first.text() shouldBe i1
    }

    "render contained paragraphs" in new TextTest {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test"))(fakeRequest, messages, ctx))

      doc.getElementsByTag("p").asScala.toList.foreach { p =>
        elementAttrs(p)("class").contains("govuk-body") shouldBe true
      }
    }

    "render input as input field" in new TextTest {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val inputFields = doc.getElementsByTag("input")
      inputFields.size shouldBe 1
    }

    "render input as input field of correct width" in new TextTest {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val inputFields = doc.getElementsByTag("input")
      inputFields.size shouldBe 1
      private val attrs = elementAttrs(inputFields.first())
      attrs("class").contains("govuk-input--width-5") shouldBe true
    }

    "render input with previous answer entered" in new TextTest {
      private val form = textFormProvider("test").bind(Map("test" -> inputValue1))
      private val doc = asDocument(components.input(input, "test", form)(fakeRequest, messages, ctx))
      private val inputs = doc.getElementsByTag("input").asScala.toList
      inputs.size shouldBe 1
      private val inputField = elementAttrs(inputs.head)
      inputField("value") shouldBe inputValue1
    }

    "render input with hints" in new TextTest {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val hints = doc.getElementsByTag("div").asScala.toList
      private val firstHint = hints(1)
      private val hint1Attrs = elementAttrs(firstHint)
      hint1Attrs("class") shouldBe "govuk-hint"
      firstHint.text() shouldBe i1Hint
    }

    "Input with no body should have a label wrapper class on H1" in new TextTest {
      private val doc = asDocument(components.input(inputWithoutBody, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val h1 = doc.getElementsByTag("h1").first
      private val attrs = elementAttrs(h1)
      attrs("class").contains("govuk-label-wrapper") shouldBe true
    }

    "Input modified with [norepeat] with body should have a label wrapper class on H1" in new Test {
      private val doc = asDocument(components.input(inputWithHint, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val h1 = doc.getElementsByTag("h1").first
      private val attrs = elementAttrs(h1)
      attrs("class").contains("govuk-label-wrapper") shouldBe true
    }

    "input without body should render hint within a span without a fieldset" in new TextTest {
      private val doc = asDocument(components.input(inputWithHintAndNoBody, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val fieldset = doc.getElementsByTag("fieldset").first

      Option(fieldset) shouldBe None

      Option(doc.getElementById("input-hint")).fold(fail("Missing hint span")) { span =>
        val attrs = elementAttrs(span)
        attrs("id") shouldBe "input-hint"
        attrs("class").contains("govuk-hint") shouldBe true
        span.text shouldBe i1Hint
      }
    }

   "input with hint in error should include hint id and error id in aria-describedby on input" in new TextTest {
      private val doc = asDocument(components.input(inputWithHintAndErrors, "test", textFormProvider("test"))(fakeRequest, messages, ctx))

      doc.getElementsByTag("input").asScala.toList.foreach { inp =>
        elementAttrs(inp).get("aria-describedby").fold(fail("Missing aria-describedby")){ aria =>
          aria should include("input-hint")
          aria should include("required-error")
        }
      }
   }

    "input with errors should have error class assigned" in new Test {

      private val doc = asDocument(components.input(inputWithErrors, "test", textFormProvider("test"))(fakeRequest, messages, ctx))

      doc.getElementsByTag("input").asScala.toList.foreach { inp =>
        val attrs: Map[String, String] = elementAttrs(inp)
        attrs("class").contains("govuk-input") shouldBe true
        attrs("class").contains("govuk-input--width-10") shouldBe true
        attrs("class").contains("govuk-input--error") shouldBe true
      }
    }

  }

  "English Date Input component" must {

    trait DateTest extends Test {
      override val input: DateInput = DateInput(Text(i1), Some(Text(i1Hint)), Seq(h2, h3, h4, para1))

      override val inputWithoutBody: DateInput = DateInput(Text(i1), None, Seq.empty)
      override val inputWithHintAndNoBody: DateInput = DateInput(Text(i1), Some(Text(i1Hint)), Seq.empty)
      override val inputWithErrors: DateInput = DateInput(Text(i1), None, Seq.empty, Seq(errorMsg))
      override val inputWithHint: DateInput = DateInput(Text(i1), Some(Text(i1Hint)), Seq(para1), Seq.empty, true)
      override val inputWithHintAndErrors: DateInput = DateInput(Text(i1), Some(Text(i1Hint)), Seq(para1), Seq(errorMsg))
      val inputWithValueErrors: DateInput = DateInput(Text(i1), Some(Text(i1Hint)), Seq(para1), Seq(valueErrorMsg))
      val dateInput: DateInput = models.ui.DateInput(Text(), None, Seq.empty)
      val datePage: FormPage = models.ui.FormPage("/url", dateInput)
      override val ctx = PageContext(datePage, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)

      val dayAnswer: String = "10"
      val monthAnswer: String = "7"
      val yearAnswer: String = "2016"
    }

    "render input text as a header" in new DateTest {
      private val doc = asDocument(components.input_date(input, "test", dateFormProvider())(fakeRequest, messages, ctx))
      private val heading = doc.getElementsByTag("h1")
      heading.size shouldBe 1
      heading.first.text() shouldBe i1
    }

    "render contained a fieldset" in new DateTest {
      private val doc = asDocument(components.input_date(input, "test", dateFormProvider())(fakeRequest, messages, ctx))
      private val fieldset = doc.getElementsByTag("fieldset")
      fieldset.size shouldBe 1
    }

    "render contained paragraphs" in new DateTest {
      private val doc = asDocument(components.input_date(input, "test", dateFormProvider())(fakeRequest, messages, ctx))

      doc.getElementsByTag("p").asScala.toList.foreach { p =>
        elementAttrs(p)("class").contains("govuk-body") shouldBe true
      }
    }

    "render input as input field" in new DateTest {
      private val doc = asDocument(components.input_date(input, "test", dateFormProvider())(fakeRequest, messages, ctx))
      private val inputFields = doc.getElementsByTag("input")
      inputFields.size shouldBe 3
    }

    "render input with previous answer entered" in new DateTest {
      private val form = dateFormProvider().bind(Map("day" -> "23", "month" -> "12", "year" -> "2000"))
      private val doc = asDocument(components.input_date(input, "test", form)(fakeRequest, messages, ctx))
      private val inputs = doc.getElementsByTag("input").asScala.toList
      inputs.size shouldBe 3
      private val dayInput = doc.getElementById("day")
      dayInput.`val`() shouldBe "23"
      private val monthInput = doc.getElementById("month")
      monthInput.`val`() shouldBe "12"
      private val yearInput = doc.getElementById("year")
      yearInput.`val`() shouldBe "2000"
    }

    "render input with hints" in new DateTest {
      private val doc = asDocument(components.input_date(input, "test", dateFormProvider())(fakeRequest, messages, ctx))
      private val hints = doc.getElementsByTag("div").asScala.toList
      private val firstHint = hints(1)
      private val hint1Attrs = elementAttrs(firstHint)
      hint1Attrs("class") shouldBe "govuk-hint"
      firstHint.text() shouldBe Text(i1Hint).asString
    }

    "render input with no body as a fieldset class on H1" in new DateTest {
      private val doc = asDocument(components.input_date(inputWithoutBody, "test", dateFormProvider())(fakeRequest, messages, ctx))
      private val h1 = doc.getElementsByTag("h1").first
      private val attrs = elementAttrs(h1)
      attrs("class").contains("govuk-fieldset__heading") shouldBe true
    }

    "Input modified with [norepeat] with body should have a label wrapper class on H1" in new Test {
      private val doc = asDocument(components.input(inputWithHint, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val h1 = doc.getElementsByTag("h1").first
      private val attrs = elementAttrs(h1)
      attrs("class").contains("govuk-label-wrapper") shouldBe true
    }

    "render input without body within a fieldset" in new DateTest {
      private val doc = asDocument(components.input_date(inputWithHintAndNoBody, "test", dateFormProvider())(fakeRequest, messages, ctx))
      private val fieldset = doc.getElementsByTag("fieldset").first

      Option(fieldset) shouldBe Some(fieldset)

      Option(doc.getElementById("input-hint")).fold(fail("Missing hint span")) { span =>
        val attrs = elementAttrs(span)
        attrs("id") shouldBe "input-hint"
        attrs("class").contains("govuk-hint") shouldBe true
        span.text shouldBe i1Hint
      }
    }

    "not render inputs with error class for required error messages on input component only" in new DateTest {

      private val doc: Document = asDocument(components.input_date(inputWithErrors, "test", dateFormProvider())(fakeRequest, messages, ctx))

      private val dayAttrs: Map[String, String] = getElementAttributes(doc, "day")

      dayAttrs("class").contains("govuk-input--error") shouldBe true

      private val monthAttrs: Map[String, String] = getElementAttributes(doc, "month")

      monthAttrs("class").contains("govuk-input--error") shouldBe true

      private val yearAttrs: Map[String, String] = getElementAttributes(doc, "year")

      yearAttrs("class").contains("govuk-input--error") shouldBe true
    }

    "render inputs with error class for value error messages on input component only" in new DateTest {

      private val doc: Document = asDocument(components.input_date(inputWithValueErrors, "test", dateFormProvider())(fakeRequest, messages, ctx))

      private val dayAttrs: Map[String, String] = getElementAttributes(doc, "day")

      dayAttrs("class").contains("govuk-input--error") shouldBe true

      private val monthAttrs: Map[String, String] = getElementAttributes(doc, "month")

      monthAttrs("class").contains("govuk-input--error") shouldBe true

      private val yearAttrs: Map[String, String] = getElementAttributes(doc, "year")

      yearAttrs("class").contains("govuk-input--error") shouldBe true
    }


    "render day input with error class when day is missing" in new DateTest {

      private val commonInputClasses: List[String] = List( "govuk-input", "govuk-date-input__input")

      bindDateFormData( "month" -> monthAnswer, "year" -> yearAnswer) match {
        case Right(_) => fail("Binding of data should fail owing to a missing field")
        case Left(formWithErrors) =>

          val doc: Document = asDocument(components.input_date(inputWithErrors, "test", formWithErrors)(fakeRequest, messages, ctx))

          val dayAttrs: Map[String, String] = getElementAttributes(doc, "day")

          commonInputClasses.foreach{ cic => dayAttrs("class").contains(cic) shouldBe true}

          dayAttrs("class").contains("govuk-input--error") shouldBe true

          dayAttrs("class").contains("govuk-input--width-2") shouldBe true

          val monthAttrs: Map[String, String] = getElementAttributes(doc, "month")

          commonInputClasses.foreach{ cic => monthAttrs("class").contains(cic) shouldBe true}

          monthAttrs("class").contains("govuk-input--error") shouldBe false

          monthAttrs("class").contains("govuk-input--width-2") shouldBe true

          val yearAttrs: Map[String, String] = getElementAttributes(doc, "year")

          commonInputClasses.foreach{ cic => yearAttrs("class").contains(cic) shouldBe true}

          yearAttrs("class").contains("govuk-input--error") shouldBe false

          yearAttrs("class").contains("govuk-input--width-4") shouldBe true
      }

    }

    "render day input with error class when month is missing" in new DateTest {

      bindDateFormData( "day" -> dayAnswer, "year" -> yearAnswer) match {
        case Right(_) => fail("Binding of data should fail owing to a missing field")
        case Left(formWithErrors) =>

          val doc: Document = asDocument(components.input_date(inputWithErrors, "test", formWithErrors)(fakeRequest, messages, ctx))

          val dayAttrs: Map[String, String] = getElementAttributes(doc, "day")

          dayAttrs("class").contains("govuk-input--error") shouldBe false

          val monthAttrs: Map[String, String] = getElementAttributes(doc, "month")

          monthAttrs("class").contains("govuk-input--error") shouldBe true

          val yearAttrs: Map[String, String] = getElementAttributes(doc, "year")

          yearAttrs("class").contains("govuk-input--error") shouldBe false
      }

    }

    "render day input with error class when year is missing" in new DateTest {

      bindDateFormData( "day" -> dayAnswer, "month" -> monthAnswer) match {
        case Right(_) => fail("Binding of data should fail owing to a missing field")
        case Left(formWithErrors) =>

          val doc: Document = asDocument(components.input_date(inputWithErrors, "test", formWithErrors)(fakeRequest, messages, ctx))

          val dayAttrs: Map[String, String] = getElementAttributes(doc, "day")

          dayAttrs("class").contains("govuk-input--error") shouldBe false

          val monthAttrs: Map[String, String] = getElementAttributes(doc, "month")

          monthAttrs("class").contains("govuk-input--error") shouldBe false

          val yearAttrs: Map[String, String] = getElementAttributes(doc, "year")

          yearAttrs("class").contains("govuk-input--error") shouldBe true
      }

    }

    "render day input with error class when day and month are missing" in new DateTest {

      bindDateFormData( "year" -> yearAnswer) match {
        case Right(_) => fail("Binding of data should fail owing to missing fields")
        case Left(formWithErrors) =>

          val doc: Document = asDocument(components.input_date(inputWithErrors, "test", formWithErrors)(fakeRequest, messages, ctx))

          val dayAttrs: Map[String, String] = getElementAttributes(doc, "day")

          dayAttrs("class").contains("govuk-input--error") shouldBe true

          val monthAttrs: Map[String, String] = getElementAttributes(doc, "month")

          monthAttrs("class").contains("govuk-input--error") shouldBe true

          val yearAttrs: Map[String, String] = getElementAttributes(doc, "year")

          yearAttrs("class").contains("govuk-input--error") shouldBe false
      }

    }

    "render day input with error class when all input fields are missing" in new DateTest {

      bindDateFormData() match {
        case Right(_) => fail("Binding of data should fail owing to missing fields")
        case Left(formWithErrors) =>

          val doc: Document = asDocument(components.input_date(inputWithErrors, "test", formWithErrors)(fakeRequest, messages, ctx))

          val dayAttrs: Map[String, String] = getElementAttributes(doc, "day")

          dayAttrs("class").contains("govuk-input--error") shouldBe true

          val monthAttrs: Map[String, String] = getElementAttributes(doc, "month")

          monthAttrs("class").contains("govuk-input--error") shouldBe true

          val yearAttrs: Map[String, String] = getElementAttributes(doc, "year")

          yearAttrs("class").contains("govuk-input--error") shouldBe true
      }

    }

  }

  "English Number Input component" must {

    trait NumberTest extends Test {
      override val input: NumberInput = NumberInput(Text(i1), Some(Text(i1Hint)), Seq(h2, h3, h4, para1), Nil, false, "5")

      override val inputWithoutBody: NumberInput = NumberInput(Text(i1), None, Seq.empty)
      override val inputWithHintAndNoBody: NumberInput = NumberInput(Text(i1), Some(Text(i1Hint)), Seq.empty)
      override val inputWithHint: NumberInput = NumberInput(Text(i1), Some(Text(i1Hint)), Seq(para1), Seq.empty, true)
      override val inputWithHintAndErrors: NumberInput = NumberInput(Text(i1), Some(Text(i1Hint)), Seq(para1), Seq(errorMsg))
      val NumberInputPage: FormPage = models.ui.FormPage("/url", input)
      override val ctx = PageContext(NumberInputPage, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
    }

    "render input text as a header" in new NumberTest {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val heading = doc.getElementsByTag("h1")
      heading.size shouldBe 1
      heading.first.text() shouldBe i1
    }

    "render contained paragraphs" in new NumberTest {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test"))(fakeRequest, messages, ctx))

      doc.getElementsByTag("p").asScala.toList.foreach { p =>
        elementAttrs(p)("class").contains("govuk-body") shouldBe true
      }
    }

    "render input as input field" in new NumberTest {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val inputFields = doc.getElementsByTag("input")
      inputFields.size shouldBe 1
    }

    "render input as input field of correct width" in new NumberTest {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val inputFields = doc.getElementsByTag("input")
      inputFields.size shouldBe 1
      private val attrs = elementAttrs(inputFields.first())
      attrs("class").contains("govuk-input--width-5") shouldBe true
    }

    "render input with previous answer entered" in new NumberTest {
      private val form = textFormProvider("test").bind(Map("test" -> inputValue1))
      private val doc = asDocument(components.input(input, "test", form)(fakeRequest, messages, ctx))
      private val inputs = doc.getElementsByTag("input").asScala.toList
      inputs.size shouldBe 1
      private val inputField = elementAttrs(inputs.head)
      inputField("value") shouldBe inputValue1
    }

    "render input with hints" in new NumberTest {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val hints = doc.getElementsByTag("div").asScala.toList
      private val firstHint = hints(1)
      private val hint1Attrs = elementAttrs(firstHint)
      hint1Attrs("class") shouldBe "govuk-hint"
      firstHint.text() shouldBe i1Hint
    }

    "Input with no body should have a label wrapper class on H1" in new NumberTest {
      private val doc = asDocument(components.input(inputWithoutBody, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val h1 = doc.getElementsByTag("h1").first
      private val attrs = elementAttrs(h1)
      attrs("class").contains("govuk-label-wrapper") shouldBe true
    }

    "Input modified with [norepeat] with body should have a label wrapper class on H1" in new Test {
      private val doc = asDocument(components.input(inputWithHint, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val h1 = doc.getElementsByTag("h1").first
      private val attrs = elementAttrs(h1)
      attrs("class").contains("govuk-label-wrapper") shouldBe true
    }

    "input without body should render hint within a span without a fieldset" in new NumberTest {
      private val doc = asDocument(components.input(inputWithHintAndNoBody, "test", textFormProvider("test"))(fakeRequest, messages, ctx))
      private val fieldset = doc.getElementsByTag("fieldset").first

      Option(fieldset) shouldBe None

      Option(doc.getElementById("input-hint")).fold(fail("Missing hint span")) { span =>
        val attrs = elementAttrs(span)
        attrs("id") shouldBe "input-hint"
        attrs("class").contains("govuk-hint") shouldBe true
        span.text shouldBe i1Hint
      }
    }

    "input with hint in error should include hint id and error id in aria-describedby on input" in new NumberTest {
      private val doc = asDocument(components.input(inputWithHintAndErrors, "test", textFormProvider("test"))(fakeRequest, messages, ctx))

      doc.getElementsByTag("input").asScala.toList.foreach { inp =>
        elementAttrs(inp).get("aria-describedby").fold(fail("Missing aria-describedby")){ aria =>
          aria should include("input-hint")
          aria should include("required-error")
        }
      }
    }
  }

  private def bindDateFormData(data: (String, String)*): Either[Form[_], SubmittedDateAnswer] = {

    implicit val bindRequest: Request[_] = FakeRequest("POST", "/")
      .withFormUrlEncodedBody(data: _*)

    def formProvider: DateFormProvider = new DateFormProvider

    val form: Form[SubmittedDateAnswer] = formProvider()

    form.bindFromRequest().fold(
      formWithErrors => Left(formWithErrors),
      formData => Right(formData)
    )
  }

  private def getElementAttributes(doc: Document, id: String) : Map[String, String] = {

    Option(doc.getElementById(id)) match {
      case Some(element) => elementAttrs(element)
      case None => Map()
    }

  }

}
