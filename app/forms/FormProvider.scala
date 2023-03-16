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

import scala.util.matching.Regex
import play.api.data.{Form, Mapping}
import play.api.data.Forms._
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import models.ui.{SubmittedDateAnswer, SubmittedListAnswer, SubmittedTextAnswer}

trait FormProvider

class SubmittedTextAnswerFormProvider extends FormProvider {

  def apply(bindData: (String, Mapping[String])): Form[SubmittedTextAnswer] =

    Form(
      mapping(
        bindData._1 -> bindData._2
      )(SubmittedTextAnswer.apply)(SubmittedTextAnswer.unapply)
    )

}

class SubmittedDateAnswerFormProvider extends FormProvider {
  private val inputDateRegex: Regex = "(.+?)\\/(.+?)\\/(.+?)$".r

  def splitInputString(dateString: String): (String, String, String) =
    inputDateRegex.findFirstMatchIn(dateString).fold(("","","")){m => (m.group(1), m.group(2), m.group(3))}

  def apply(): Form[SubmittedDateAnswer] =

    Form(
      mapping(
        "day" -> nonEmptyText,
        "month" -> nonEmptyText,
        "year" -> nonEmptyText
      )(SubmittedDateAnswer.apply)(SubmittedDateAnswer.unapply)
    )

}

class SubmittedListAnswerFormProvider extends FormProvider {

  def apply(name: String): Form[SubmittedListAnswer] =

    Form(
      mapping(name -> list(text).verifying(validateListIsPopulated))
      (SubmittedListAnswer.apply)(SubmittedListAnswer.unapply)
    )

  private def validateListIsPopulated[T]: Constraint[List[T]] =
    Constraint[List[T]]("constraint.required")
      {list => if(list.nonEmpty) Valid else Invalid(ValidationError("error.required"))}
}


