/*
 * Copyright 2022 HM Revenue & Customs
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

import core.models.ocelot.Ten

sealed trait Input extends FormComponent {
  val dontRepeatText: Boolean
  val width: String
}

case class NumberInput(text: Text,
                       hint: Option[Text],
                       body: Seq[UIComponent],
                       errorMsgs: Seq[ErrorMsg] = Nil,
                       dontRepeatText: Boolean = false,
                       width: String = Ten) extends Input
case class TextInput(text: Text,
                       hint: Option[Text],
                       body: Seq[UIComponent],
                       errorMsgs: Seq[ErrorMsg] = Nil,
                       dontRepeatText: Boolean = false,
                       width: String = Ten) extends Input
case class CurrencyInput(text: Text,
                       hint: Option[Text],
                       body: Seq[UIComponent],
                       errorMsgs: Seq[ErrorMsg] = Nil,
                       dontRepeatText: Boolean = false,
                       width: String = Ten) extends Input
case class CurrencyPoundsOnlyInput(text: Text,
                       hint: Option[Text],
                       body: Seq[UIComponent],
                       errorMsgs: Seq[ErrorMsg] = Nil,
                       dontRepeatText: Boolean = false,
                       width: String = Ten) extends Input
case class DateInput(text: Text,
                       hint: Option[Text],
                       body: Seq[UIComponent],
                       errorMsgs: Seq[ErrorMsg] = Nil,
                       dontRepeatText: Boolean = false,
                       width: String = Ten) extends Input
