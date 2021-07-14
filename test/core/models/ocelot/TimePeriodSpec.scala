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

package core.models.ocelot

import base.BaseSpec

class TimePeriodSpec extends BaseSpec {

  "A TimePeriod instance" must {
    "construct from a number of days" in {
      asTimePeriod("23455day") shouldBe Some(TimePeriod(23455, Day))
    }
    "construct from a number of weeks" in {
      asTimePeriod("23455week") shouldBe Some(TimePeriod(23455, Week))
    }
    "construct from a number of months" in {
      asTimePeriod("23455month") shouldBe Some(TimePeriod(23455, Month))
    }
    "construct from a number of years" in {
      asTimePeriod("23455year") shouldBe Some(TimePeriod(23455, Year))
    }

    "not construct when the count digit length is greater than the digit length of Int.MaxValue" in {
      asTimePeriod("12345678901day") shouldBe None
    }
  }
}
