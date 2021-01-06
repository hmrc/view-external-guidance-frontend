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

package models.ui

import java.time.LocalDate
import java.time.format.ResolverStyle

import models.ocelot.dateFormatter

trait Input extends FormComponent

case class CurrencyInput(text: Text, hint: Option[Text], body: Seq[UIComponent], errorMsgs: Seq[ErrorMsg] = Nil) extends Input
case class CurrencyPoundsOnlyInput(text: Text, hint: Option[Text], body: Seq[UIComponent], errorMsgs: Seq[ErrorMsg] = Nil) extends Input
case class DateInput(text: Text, hint: Option[Text], body: Seq[UIComponent], errorMsgs: Seq[ErrorMsg] = Nil) extends Input

object DateInput {

  def partitionSubmittedDateAnswer(submittedDateAnswer: String): (String, String, String) = {

    val submittedDate: LocalDate = LocalDate.parse(submittedDateAnswer, dateFormatter.withResolverStyle(ResolverStyle.STRICT))

    (submittedDate.getDayOfMonth.toString, submittedDate.getMonthValue.toString, submittedDate.getYear.toString)
  }

}
