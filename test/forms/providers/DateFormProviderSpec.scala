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
import models.ui.DateAnswer
import base.BaseSpec


class DateFormProviderSpec extends BaseSpec {

  val formProvider: DateFormProvider = new DateFormProvider()

  val day: String = "21"
  val month: String = "12"
  val year: String = "2020"

  "Form created by DateAnswerFormProvider" should {

    "correctly bind a submitted date" in {

      val form: Form[DateAnswer] = formProvider()

      val boundForm: Form[DateAnswer] = form.bind(
        Map(
          "day" -> day,
          "month" -> month,
          "year" -> year
        )
      )

      boundForm.get shouldBe DateAnswer(day, month, year)
    }

    "be able to execute the unbind method held in mapping" in {

      val form: Form[DateAnswer] = formProvider()

      val map: Map[String, String] = form.mapping.unbind(DateAnswer(day, month, year))

      map.keySet shouldBe Set("day", "month", "year")

      map("day") shouldBe day
      map("month") shouldBe month
      map("year") shouldBe year
    }
  }

}
