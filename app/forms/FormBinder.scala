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

package forms

import forms.bindings._
import play.api.mvc._
import play.api.data.Form
import play.api.i18n.MessagesApi
import core.models.ocelot.stanzas.{DateInput, DataInput, Input, Question, Sequence}

class FormBinder(messagesApi: MessagesApi) {
  lazy val dateBinding: FormBinding = new DateBinding(messagesApi)

  def bind(input: DataInput, name: String)(implicit request: Request[_]): Binding = binding(input).bind(name)
  def populated(input: DataInput, name: String, answer: Option[String]): Form[_] = binding(input).populated(name, answer)
  def binding(input: DataInput): FormBinding = input match {
      case _: DateInput => dateBinding
      case _: Input | _: Question => StringBinding
      case _: Sequence => StringListBinding
    }
}