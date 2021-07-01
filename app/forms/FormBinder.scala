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

import forms.binders._
import play.api.mvc._
import play.api.data.Form
import play.api.i18n.MessagesApi
import core.models.ocelot.stanzas.{DateInput, DataInput, Input, Question, Sequence}

class FormBinder(messagesApi: MessagesApi) {
  val dateInputBinder: DateFormBinder = new DateFormBinder(messagesApi)
  val stringInputBinder: StringFormBinder = new StringFormBinder
  val stringListBinder: StringListFormBinder = new StringListFormBinder

  def bind(input: DataInput, name: String)(implicit request: Request[_]): Binding =
    input match {
      case _: DateInput => dateInputBinder.bind(name)
      case _: Input | _: Question => stringInputBinder.bind(name)
      case _: Sequence => stringListBinder.bind(name)
    }

  def populated(input: DataInput, name: String, answer: Option[String]): Form[_] =
    input match {
      case _: DateInput => dateInputBinder.populated(name, answer)
      case _: Input | _: Question => stringInputBinder.populated(name, answer)
      case _: Sequence => stringListBinder.populated(name, answer)
    }
}