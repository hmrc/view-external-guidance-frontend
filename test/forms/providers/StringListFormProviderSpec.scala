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

import play.api.data.Form
import models.ui.ListAnswer

import base.BaseSpec

class ListAnswerFormProviderSpec extends BaseSpec {

  val formProvider: StringListFormProvider = new StringListFormProvider()

  val listItem0Value: String = "Monday"
  val listItem1Value: String = "Tuesday"
  val listItem2Value: String = "Wednesday"

  "Forms created by ListAnswerFormProvider" should {

    "correctly bind submitted data" in {

      val form: Form[ListAnswer] = formProvider("path")

      val boundForm: Form[ListAnswer] = form.bind(
        Map(
          "path[0]" -> listItem0Value,
          "path[1]" -> listItem1Value,
          "path[2]" -> listItem2Value
        )
      )

      boundForm.get shouldBe ListAnswer(List(listItem0Value, listItem1Value, listItem2Value))
    }
  }

  "be able to execute the unbind method held in mapping" in {

    val form: Form[ListAnswer] = formProvider("path")

    val map: Map[String, String] = form.mapping.unbind(ListAnswer(List(listItem0Value, listItem1Value, listItem2Value)))

    map.keySet shouldBe Set("path[0]", "path[1]", "path[2]")

    map("path[0]") shouldBe listItem0Value
    map("path[1]") shouldBe listItem1Value
    map("path[2]") shouldBe listItem2Value
  }

}
