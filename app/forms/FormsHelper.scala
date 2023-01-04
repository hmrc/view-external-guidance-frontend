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
import play.api.data.{Form, Mapping}
import play.api.data.Forms.nonEmptyText
import play.api.i18n.Messages
import core.models.ocelot.splitInputDateString
import core.models.ocelot.stanzas.{Input, DateInput, DataInput, Question, Sequence}
import models.ui.{SubmittedAnswer, SubmittedDateAnswer, SubmittedListAnswer, SubmittedTextAnswer}
import services.{ErrorStrategy, ValueMissingError, ValueMissingGroupError}
import play.api.data.FormBinding.Implicits._

object FormsHelper {

  def bindFormData(inputStanza: DataInput, path: String)
                  (implicit request: Request[_], messages: Messages): Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] =
    // Define form mapping for each data input type
    inputStanza match {
      case _: DateInput => bindSubmittedDateAnswer()
      case _: Input | _: Question => bindSubmittedTextAnswer(path -> nonEmptyText)
      case _: Sequence => bindSubmittedListAnswer(path)
    }

  def populatedForm(inputStanza: DataInput, path: String, answer: Option[String]): Form[_] =
    inputStanza match {
      case _: DateInput => populateSubmittedDateAnswerForm(answer)
      case _: Input | _: Question => populateSubmittedTextAnswerForm(path -> nonEmptyText, path, answer)
      case _: Sequence => populateSubmittedListAnswerForm(path, answer)
    }

  private def bindSubmittedTextAnswer(bindData: (String, Mapping[String]))
                                  (implicit request: Request[_]): Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = {

    val formProvider: SubmittedTextAnswerFormProvider = new SubmittedTextAnswerFormProvider()

    val form = formProvider(bindData)

    form.bindFromRequest().fold(
      formWithErrors => Left((formWithErrors, ValueMissingError)),
      formData => Right((form.fill(formData), formData))
    )

  }

  private def bindSubmittedDateAnswer()
                                     (implicit request: Request[_], messages: Messages): Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = {

    val formProvider: SubmittedDateAnswerFormProvider = new SubmittedDateAnswerFormProvider()

    val form = formProvider()

    form.bindFromRequest().fold(
      formWithErrors => {
        formWithErrors.errors.size match {
          case 3 => Left((formWithErrors, ValueMissingGroupError(Nil)))
          case _ =>
            val errorItems: List[String] = formWithErrors.errors.toList.map(err => messages(s"label.${err.key}"))
            Left((formWithErrors, ValueMissingGroupError(errorItems.map(_.toLowerCase))))
        }
      },
      formData => Right((form.fill(formData), formData))
    )

  }

  private def bindSubmittedListAnswer(path: String)
                                     (implicit request: Request[_]): Either[(Form[_], ErrorStrategy), (Form[_], SubmittedAnswer)] = {

    val formProvider: SubmittedListAnswerFormProvider = new SubmittedListAnswerFormProvider()

    val form = formProvider(path)

    form.bindFromRequest().fold(
      formWithErrors => Left((formWithErrors, ValueMissingError)),
      formData => Right((form.fill(formData), formData))
    )
  }

  private def populateSubmittedTextAnswerForm(bindData: (String, Mapping[String]), path: String, answer: Option[String]): Form[SubmittedTextAnswer] = {

    val formProvider: SubmittedTextAnswerFormProvider = new SubmittedTextAnswerFormProvider()

    val form: Form[SubmittedTextAnswer] = formProvider(bindData)

    answer match {
      case Some(value) => form.bind(Map(path -> value))
      case None => form
    }

  }

  private def populateSubmittedDateAnswerForm(answer: Option[String]): Form[SubmittedDateAnswer] = {

    val formProvider: SubmittedDateAnswerFormProvider = new SubmittedDateAnswerFormProvider()

    val form: Form[SubmittedDateAnswer] = formProvider()

    answer match {
      case Some(value) =>

        val (day, month, year) = splitInputDateString(value).fold(("", "", ""))(v => v)

        form.bind(
          Map(
          "day" -> day,
          "month" -> month,
          "year" -> year
             )
        )

      case None => form
    }
  }

  private def populateSubmittedListAnswerForm(path: String, answer: Option[String]): Form[SubmittedListAnswer] = {

    val formProvider: SubmittedListAnswerFormProvider = new SubmittedListAnswerFormProvider()

    val form: Form[SubmittedListAnswer] = formProvider(path)

    answer match {
      case Some(value) if value.nonEmpty => form.fill(SubmittedListAnswer(value.split(",").map(_.trim).toList))
      case _ => form
    }
  }

}
