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

package forms.providers

import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.Forms.nonEmptyText
import play.api.i18n.Messages
import core.models.ocelot.splitInputDateString
import models.ui.DateAnswer
import services.ValueMissingGroupError
import play.api.data.FormBinding.Implicits._
import forms._

class DateFormProvider extends FormProvider[DateAnswer] {
  def bind(name: String)(implicit request: Request[_], messages: Messages): Binding =
   apply().bindFromRequest().fold(
    fe => if (fe.errors.size == 3) Left((fe, ValueMissingGroupError(Nil)))
          else Left((fe, ValueMissingGroupError(fe.errors.map(e => (messages(s"label.${e.key}")).toLowerCase).toList))),
    formData => Right(( apply().fill(formData), formData))
  )

  def populated(name: String, answer: Option[String]): Form[DateAnswer] =
    answer.fold(apply()){value =>
      val (day, month, year) = splitInputDateString(value).fold(("", "", ""))(v => v)
      apply().bind(Map("day" -> day, "month" -> month, "year" -> year))
    }

  def apply(): Form[DateAnswer] =
    Form(mapping("day" -> nonEmptyText,
                 "month" -> nonEmptyText,
                 "year" -> nonEmptyText)
        (DateAnswer.apply)(DateAnswer.unapply)
    )
}
