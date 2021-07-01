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

package forms.binders

import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.Forms.nonEmptyText
import play.api.i18n.MessagesApi
import scala.util.matching.Regex
import models.ui.SubmittedDateAnswer
import services.ValueMissingGroupError
import play.api.data.FormBinding.Implicits._

class DateFormBinder(messagesApi: MessagesApi) extends TypedFormBinder {
  val inputDateRegex: Regex = "(.+?)\\/(.+?)\\/(.+?)$".r
  val form: Form[SubmittedDateAnswer] = Form(mapping("day" -> nonEmptyText, "month" -> nonEmptyText, "year" -> nonEmptyText)
                                                    (SubmittedDateAnswer.apply)(SubmittedDateAnswer.unapply))

  def bind(name: String)(implicit request: Request[_]): Binding =
    form.bindFromRequest().fold(formWithErrors =>
      if (formWithErrors.errors.size == 3) Left((formWithErrors, ValueMissingGroupError(Nil)))
      else {
        val errorItems: List[String] = formWithErrors.errors.toList.map(err => messagesApi.preferred(request)(s"label.${err.key}"))
        Left((formWithErrors, ValueMissingGroupError(errorItems.map(_.toLowerCase))))
      },
      formData => Right((form.fill(formData), formData))
    )

  def populated(path: String, answer: Option[String]): Form[SubmittedDateAnswer] =
    answer.fold(form)(value => {
      val (day, month, year) = inputDateRegex.findFirstMatchIn(value).fold(("","","")){m => (m.group(1), m.group(2), m.group(3))}
      form.bind(Map("day" -> day, "month" -> month, "year" -> year))
    })
}