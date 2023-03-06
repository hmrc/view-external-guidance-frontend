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
import models.ui.SubmittedListAnswer
import services.ValueMissingError
import play.api.i18n.Messages
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.data.FormBinding.Implicits._
import forms._

class StringListFormProvider extends FormProvider[SubmittedListAnswer] {
  def bind(name: String)(implicit request: Request[_], messages: Messages): Binding =
    apply(name).bindFromRequest().fold(fe => Left((fe, ValueMissingError)), fd => Right((apply(name).fill(fd), fd)))

  def populated(name: String, answer: Option[String]): Form[SubmittedListAnswer] =
    answer match {
      case Some(value) if value.nonEmpty => apply(name).fill(SubmittedListAnswer(value.split(",").map(_.trim).toList))
      case _ => apply(name)
    }

  def apply(name: String): Form[SubmittedListAnswer] =
    Form(mapping(name -> list(text).verifying(Constraint[List[String]]("constraint.required")
                                             (l => if(l.nonEmpty) Valid else Invalid(ValidationError("error.required")))))
    (SubmittedListAnswer.apply)(SubmittedListAnswer.unapply))
}