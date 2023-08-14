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

import javax.inject.{Inject, Singleton}
import play.api.mvc.Request
import play.api.data.Form
import play.api.i18n.Messages
import forms.providers._
import core.models.ocelot.stanzas.{DataInput, DateInput, Input, Question, Sequence}
import play.api.Logging

trait FormProvider[T] {
  def bind(name: String)(implicit request: Request[_], messages: Messages): Binding
  def populated(name: String, answer: Option[String]): Form[T]
}

@Singleton
class FormProviderFactory @Inject() (
  dateFormProvider: DateFormProvider,
  stringFormProvider: StringFormProvider,
  stringListFormProvider: StringListFormProvider) extends Logging {

  def apply(input: DataInput): FormProvider[_] = input match {
      case _: DateInput => dateFormProvider
      case _: Input | _: Question => stringFormProvider
      case _: Sequence => stringListFormProvider
      case _ =>
        logger.error("ERROR!: Unsupported Stanza type, please provide a form provider for this stanza type")
        stringFormProvider
    }
}

