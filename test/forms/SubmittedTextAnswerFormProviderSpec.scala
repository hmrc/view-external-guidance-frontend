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

package forms

import play.api.data.Form
import play.api.data.Forms._

import models.ui.SubmittedTextAnswer

import base.BaseSpec

class SubmittedTextAnswerFormProviderSpec extends BaseSpec {

  val formProvider: SubmittedTextAnswerFormProvider = new SubmittedTextAnswerFormProvider()

  val inputValue: String = "10.00"

  "Forms created by SubmittedTextAnswerFormProvider" should {

    "correctly bind a single text input value" in {

      val form: Form[SubmittedTextAnswer] = formProvider("answer" -> nonEmptyText)

      val boundForm: Form[SubmittedTextAnswer] = form.bind(Map("answer" -> inputValue))

      boundForm.get shouldBe SubmittedTextAnswer(inputValue)
    }

    "be able to be able to execute the unapply method held in mapping" in {

      val form: Form[SubmittedTextAnswer] = formProvider("answer" -> nonEmptyText)

      val map: Map[String, String] = form.mapping.unbind(SubmittedTextAnswer(inputValue))

      map.keySet shouldBe Set("answer")

      map("answer") shouldBe inputValue
    }

  }

}
