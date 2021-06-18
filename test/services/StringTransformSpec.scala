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

package services

import base.BaseSpec

class StringTransformSpec extends BaseSpec {
  val PlainString: String = """Licensed under the Apache License, Version 2.0 (the "License");you may not use this file except in compliance with the License."""
  val ApostropheString: String = """Uses of an apostrophe ', include ownership and the location of missed text as in Geeorge's book and Janet hasn't arrived"""
  val DashString: String = """You can claim the actual amount you have spent - you will need to keep receipts."""
  val MixedString: String = """You can claim the actual amount you've spent - you'll need to keep receipts."""

  "String data under transformation" must {

    "Mirror empty input data with similar output" in {
      StringTransform.transform("") shouldBe ""
    }

    "Remain unchanged if no transformable groups found" in {
      StringTransform.transform(PlainString) shouldBe PlainString
    }

    "Replace single apostrophe with curly apostrophe" in {
      StringTransform.transform("'") shouldBe "’"
    }

    "Replace standard dash group with long dash" in {
      StringTransform.transform(" - ") shouldBe " – "
    }

    "Replace standard apostrophe with curly apostrophe" in {
      val transformation = """Uses of an apostrophe ’, include ownership and the location of missed text as in Geeorge’s book and Janet hasn’t arrived"""
      StringTransform.transform(ApostropheString) shouldBe transformation
    }

    "Replace standard dash with long dash" in {
      val transformation = """You can claim the actual amount you have spent – you will need to keep receipts."""
      StringTransform.transform(DashString) shouldBe transformation
    }

    "Replace apostrophes and dashes with their GDS equivalent" in {
      val transformation = """You can claim the actual amount you’ve spent – you’ll need to keep receipts."""
      StringTransform.transform(MixedString) shouldBe transformation
    }

  }
}