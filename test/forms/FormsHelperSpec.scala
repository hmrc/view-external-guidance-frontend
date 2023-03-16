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

package forms

import play.api.mvc._
import play.api.data.Form
import play.api.i18n.{Lang, Messages, MessagesApi}
import core.models.ocelot.Phrase
import core.models.ocelot.stanzas.{CurrencyInput, CurrencyPoundsOnlyInput, DateInput, Question, Sequence}
import models.ui.SubmittedAnswer
import services.{ErrorStrategy, ValueMissingError, ValueMissingGroupError}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.test.FakeRequest
import base.BaseSpec

class FormsHelperSpec extends BaseSpec with GuiceOneAppPerSuite {

  private trait Test {

    val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
    implicit val messages: Messages = messagesApi.preferred(Seq(Lang("en")))

    val processId: String = "ext90000"
    val path: String = s"/guidance/$processId/question"
    val relativePath: String = path.drop(1)
    val incorrectPath: String = "guidance/somePath"
    val questionAnswer: String = "0"
    val currencyAnswer: String = "10.00"
    val currencyPoundsOnlyAnswer: String = "10"
    val day: String = "day"
    val month: String = "month"
    val year: String = "year"
    val dayAnswer: String = "16"
    val monthAnswer: String = "5"
    val yearAnswer: String = "2019"
    val dateAnswer: String = dayAnswer + "/" + monthAnswer + "/" + yearAnswer
    val nonExclusiveSequenceAnswer: String = "0,2,4"
    val exclusiveSequenceAnswer: String = "0,2"
    val expectedErrorMessage: String = "error.required"

    val question: Question = Question(
      Phrase("Which way is the artic?", "Welsh: which way is the arctic?"),
      Seq(
        Phrase("North", "Welsh: North"),
        Phrase("South", "Welsh: South")
      ),
      Seq("2", "3"),
      None,
      stack = false
    )

    val currencyInput: CurrencyInput = CurrencyInput(
      Seq("4"),
      Phrase("How much money do you have?", "Welsh: How much money do you have?"),
      None,
      "moneyInHand",
      None,
      stack = false
    )

    val currencyPoundsOnlyInput: CurrencyPoundsOnlyInput = CurrencyPoundsOnlyInput(
      Seq("5"),
      Phrase("How much money do you have rounded to the nearest pound?", "Welsh: How much money do you have rounded to the nearest pound?"),
      None,
      "roundedMoneyInHand",
      None,
      stack = false
    )

    val dateInput: DateInput = DateInput(
      Seq("25"),
      Phrase("Enter a random date", "Welsh: Enter a random date"),
      None,
      "randomDate",
      None,
      stack = false
    )

    val nonExclusiveSequence: Sequence = Sequence(
      Phrase("Select a day in the working week", "Welsh: Select a day in the working week"),
      Seq("10", "20", "30", "40", "50", "60"),
      Seq(
        Phrase("Monday", "Welsh: Monday"),
        Phrase("Tuesday", "Welsh: Tuesday"),
        Phrase("Wednesday", "Welsh: Wednesday"),
        Phrase("Thursday", "Welsh: Thursday"),
        Phrase("Friday", "Welsh: Friday")
      ),
      None,
      None,
      stack = false
    )

    val exclusiveSequence: Sequence = Sequence(
      Phrase("Select your favorite colour", "Welsh: Select your favourite colour"),
      Seq("10", "20", "30", "40"),
      Seq(
        Phrase("Red", "Welsh: Red"),
        Phrase("Green", "Welsh: Green"),
        Phrase("Blue", "Welsh: Blue")
      ),
      Some(
        Phrase(
          "Not a primary colour [exclusive][hint:Selecting this checkbox will deselect the other checkboxes]",
          "Welsh: Not a primary colour [exclusive][hint:Welsh: Selecting this checkbox will deselect the other checkboxes]")
      ),
      None,
      stack = false
    )

  }

  private trait WelshTest extends Test {
    override implicit val messages: Messages = messagesApi.preferred(Seq(Lang("cy")))

    override val day: String = "diwrnod"
    override val month: String = "mis"
    override val year: String = "blwyddyn"
  }

  "FormsHelper's binding functionality" should {

    "be able to bind data for a question input component" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody(relativePath -> questionAnswer)

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(question, relativePath)

      result match {
        case Right((form: Form[_], submittedAnswer: SubmittedAnswer)) =>
          form(relativePath).value shouldBe Some(questionAnswer)
          submittedAnswer.text shouldBe questionAnswer
        case Left((_: Form[_], _: ErrorStrategy)) => fail("Form binding returned a form with errors")
      }
    }

    "be able to bind data for a currency input component" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody(relativePath -> currencyAnswer)

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(currencyInput, relativePath)

      result match {
        case Right((form: Form[_], submittedAnswer: SubmittedAnswer)) =>
          form(relativePath).value shouldBe Some(currencyAnswer)
          submittedAnswer.text shouldBe currencyAnswer
        case Left((_: Form[_], _: ErrorStrategy)) => fail("Form binding returned a form with errors")
      }

    }

    "be able to bind data for a currency pounds only input component" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody(relativePath -> currencyPoundsOnlyAnswer)

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(currencyPoundsOnlyInput, relativePath)

      result match {
        case Right((form: Form[_], submittedAnswer: SubmittedAnswer)) =>
          form(relativePath).value shouldBe Some(currencyPoundsOnlyAnswer)
          submittedAnswer.text shouldBe currencyPoundsOnlyAnswer
        case Left((_: Form[_], _: ErrorStrategy)) => fail("Form binding returned a form with errors")
      }

    }

    "be able to bind data for a date input component" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody("day" -> dayAnswer, "month" -> monthAnswer, "year" -> yearAnswer)

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(dateInput, relativePath)

      result match {
        case Right((form: Form[_], submittedAnswer: SubmittedAnswer)) =>
          form("day").value shouldBe Some(dayAnswer)
          form("month").value shouldBe Some(monthAnswer)
          form("year").value shouldBe Some(yearAnswer)
          submittedAnswer.text shouldBe dateAnswer
        case Left((_: Form[_], _: ErrorStrategy)) => fail("Form binding returned a form with errors")
      }

    }

    "be able to bind data for a non-exclusive sequence input component" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody(
          s"$relativePath[0]" -> "0",
          s"$relativePath[2]" -> "2",
          s"$relativePath[4]" -> "4"
        )

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(nonExclusiveSequence, relativePath)

      result match {
        case Right((form: Form[_], submittedAnswer: SubmittedAnswer)) =>
          form(s"$relativePath[0]").value shouldBe Some("0")
          form(s"$relativePath[1]").value shouldBe Some("2")
          form(s"$relativePath[2]").value shouldBe Some("4")
          submittedAnswer.text shouldBe nonExclusiveSequenceAnswer
        case Left((_: Form[_], _: ErrorStrategy)) => fail("Form binding returned a form with errors")
      }
    }

    "be able to bind data for an exclusive sequence input component" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody(
          s"$relativePath[0]" -> "0",
          s"$relativePath[2]" -> "2"
        )

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(exclusiveSequence, relativePath)

      result match {
        case Right((form: Form[_], submittedAnswer: SubmittedAnswer)) =>
          form(s"$relativePath[0]").value shouldBe Some("0")
          form(s"$relativePath[1]").value shouldBe Some("2")
          submittedAnswer.text shouldBe exclusiveSequenceAnswer
        case Left((_: Form[_], _: ErrorStrategy)) => fail("Form binding returned a form with errors")
      }

    }

    "return a form with errors if the question answer is not mapped to the correct key in the request" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody(incorrectPath -> questionAnswer)

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(question, relativePath)

      result match {
        case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
          formWithErrors.errors.head.key shouldBe relativePath
          formWithErrors.errors.head.message shouldBe expectedErrorMessage
          errorStrategy shouldBe ValueMissingError
        case _ => fail("Binding should fail for incorrect request data")
      }

    }

    "return a form with errors if the currency value is not mapped to the correct key in the request" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody(incorrectPath -> currencyAnswer)

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(currencyInput, relativePath)

      result match {
        case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
          formWithErrors.errors.head.key shouldBe relativePath
          formWithErrors.errors.head.message shouldBe expectedErrorMessage
          errorStrategy shouldBe ValueMissingError
        case _ => fail("Binding should fail for incorrect request data")
      }

    }

    "return a form with errors if the currency pounds only value is not mapped to the correct key in the request" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody(incorrectPath -> currencyPoundsOnlyAnswer)

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(currencyPoundsOnlyInput, relativePath)

      result match {
        case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
          formWithErrors.errors.head.key shouldBe relativePath
          formWithErrors.errors.head.message shouldBe expectedErrorMessage
          errorStrategy shouldBe ValueMissingError
        case _ => fail("Binding should fail for incorrect request data")
      }
    }

    "return a form in english with errors if a date is not fully defined in the request - missing day" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody("month" -> monthAnswer, "year" -> yearAnswer)

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(dateInput, relativePath)

      result match {
        case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
          formWithErrors.errors.head.key shouldBe "day"
          formWithErrors.errors.head.message shouldBe expectedErrorMessage
          errorStrategy shouldBe ValueMissingGroupError(List(day))
        case _  => fail("Binding should fail for incorrect request data")
      }

    }

    "return a form in welsh with errors if a date is not fully defined in the request - missing day" in new WelshTest {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody("month" -> monthAnswer, "year" -> yearAnswer)

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(dateInput, relativePath)

      result match {
        case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
          errorStrategy shouldBe ValueMissingGroupError(List(day))
        case _  => fail("Binding should fail for incorrect request data")
      }

    }

    "return a form in english with errors if a date is not fully defined in the request - missing month" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody("day" -> dayAnswer, "year" -> yearAnswer)

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(dateInput, relativePath)

      result match {
        case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
          formWithErrors.errors.head.key shouldBe "month"
          formWithErrors.errors.head.message shouldBe expectedErrorMessage
          errorStrategy shouldBe ValueMissingGroupError(List(month))
        case _  => fail("Binding should fail for incorrect request data")
      }

    }

    "return a form in welsh with errors if a date is not fully defined in the request - missing month" in new WelshTest {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody("day" -> dayAnswer, "year" -> yearAnswer)

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(dateInput, relativePath)

      result match {
        case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
          errorStrategy shouldBe ValueMissingGroupError(List(month))
        case _  => fail("Binding should fail for incorrect request data")
      }

    }

    "return a form in english with errors if a date is not fully defined in the request - missing year" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody("day" -> dayAnswer, "month" -> monthAnswer)

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(dateInput, relativePath)

      result match {
        case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
          formWithErrors.errors.head.key shouldBe "year"
          formWithErrors.errors.head.message shouldBe expectedErrorMessage
          errorStrategy shouldBe ValueMissingGroupError(List(year))
        case _  => fail("Binding should fail for incorrect request data")
      }

    }

    "return a form in welsh with errors if a date is not fully defined in the request - missing year" in new WelshTest {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody("day" -> dayAnswer, "month" -> monthAnswer)

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(dateInput, relativePath)

      result match {
        case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
          errorStrategy shouldBe ValueMissingGroupError(List(year))
        case _  => fail("Binding should fail for incorrect request data")
      }

    }

    "return a form in english with errors if a date is not fully defined in the request - missing day and year" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody("month" -> monthAnswer)

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(dateInput, relativePath)

      result match {
        case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
          formWithErrors.errors.head.key shouldBe "day"
          formWithErrors.errors.head.message shouldBe expectedErrorMessage
          formWithErrors.errors.last.key shouldBe "year"
          formWithErrors.errors.last.message shouldBe expectedErrorMessage
          errorStrategy shouldBe ValueMissingGroupError(List(day, year))
        case _  => fail("Binding should fail for incorrect request data")
      }

    }

    "return a form in welsh with errors if a date is not fully defined in the request - missing day and year" in new WelshTest {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody("month" -> monthAnswer)

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(dateInput, relativePath)

      result match {
        case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
          errorStrategy shouldBe ValueMissingGroupError(List(day, year))
        case _  => fail("Binding should fail for incorrect request data")
      }

    }

    "return a form with errors if a date is not fully defined in the request - all values missing" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody()

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(dateInput, relativePath)

      result match {
        case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
          formWithErrors.errors.head.key shouldBe "day"
          formWithErrors.errors.head.message shouldBe expectedErrorMessage
          formWithErrors.errors(1).key shouldBe "month"
          formWithErrors.errors(1).message shouldBe expectedErrorMessage
          formWithErrors.errors.last.key shouldBe "year"
          formWithErrors.errors.last.message shouldBe expectedErrorMessage
          errorStrategy shouldBe ValueMissingGroupError(Nil)
        case _  => fail("Binding should fail for incorrect request data")
      }

    }

    "return a form with error if the non-exclusive sequence is not mapped to the correct key in the request" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody(s"$incorrectPath[0]" -> "0")

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(nonExclusiveSequence, relativePath)

      result match {
        case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
          formWithErrors.errors.head.message shouldBe expectedErrorMessage
          errorStrategy shouldBe ValueMissingError
        case _ => fail("Binding should fail for incorrect request data")
      }
    }

    "return a form with error if the exclusive sequence is not mapped to the correct key in the request" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody(s"$incorrectPath[0]" -> "0")

      val result: Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(exclusiveSequence, relativePath)

      result match {
        case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
          formWithErrors.errors.head.message shouldBe expectedErrorMessage
          errorStrategy shouldBe ValueMissingError
        case _ => fail("Binding should fail for incorrect request data")
      }
    }

    "populate a form with the submitted answer for a question" in new Test {

      val form: Form[_] = FormsHelper.populatedForm(question, relativePath, Some(questionAnswer))

      form(relativePath).value shouldBe Some(questionAnswer)

    }

    "populate a form with the submitted answer for a currency value" in new Test {

      val form: Form[_] = FormsHelper.populatedForm(currencyInput, relativePath, Some(currencyAnswer))

      form(relativePath).value shouldBe Some(currencyAnswer)

    }

    "populate a form with the submitted answer for a currency pounds only value" in new Test {

      val form: Form[_] = FormsHelper.populatedForm(currencyPoundsOnlyInput, relativePath, Some(currencyPoundsOnlyAnswer))

      form(relativePath).value shouldBe Some(currencyPoundsOnlyAnswer)

    }

    "populate a form with the submitted answer for a date" in new Test {

      val form: Form[_] = FormsHelper.populatedForm(dateInput, relativePath, Some(dateAnswer))

      form("day").value shouldBe Some(dayAnswer)
      form("month").value shouldBe Some(monthAnswer)
      form("year").value shouldBe Some(yearAnswer)
    }

    "populate a form with the submitted answer for a non-exclusive sequence" in new Test {

      val form: Form[_] = FormsHelper.populatedForm(nonExclusiveSequence, relativePath, Some(nonExclusiveSequenceAnswer))

      form(s"$relativePath[0]").value shouldBe Some("0")
      form(s"$relativePath[1]").value shouldBe Some("2")
      form(s"$relativePath[2]").value shouldBe Some("4")
      form(s"$relativePath[3]").value shouldBe None
    }

    "populate a form with the submitted answer for an exclusive sequence" in new Test {

      val form: Form[_] = FormsHelper.populatedForm(exclusiveSequence, relativePath, Some(exclusiveSequenceAnswer))

      form(s"$relativePath[0]").value shouldBe Some("0")
      form(s"$relativePath[1]").value shouldBe Some("2")
      form(s"$relativePath[2]").value shouldBe None
    }

  }

}
