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
import models.ui.SubmittedTextAnswer
import services.ValueMissingError
import play.api.data.FormBinding.Implicits._

class StringFormBinder extends TypedFormBinder {
  def bind(name: String)(implicit request: Request[_]): Binding =
    form(name).bindFromRequest().fold(fe => Left((fe, ValueMissingError)), fd => Right((form(name).fill(fd), fd)))

  def populated(name: String, answer: Option[String]): Form[SubmittedTextAnswer] =
    answer.fold(form(name))(value => form(name).bind(Map(name -> value)))

  def form(name: String): Form[SubmittedTextAnswer] =
    Form(mapping(name -> nonEmptyText)(SubmittedTextAnswer.apply)(SubmittedTextAnswer.unapply))
}
