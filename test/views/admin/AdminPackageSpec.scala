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

package views.admin

import base.BaseSpec
import core.models.ocelot.Phrase
import core.models.ocelot.stanzas._

class AdminPackageSpec extends BaseSpec {

  "stanzaTypeName" must {

    "support PageStanza" in {
      stanzaTypeName(PageStanza("/usr",Seq("1"), false)) shouldBe "Page"
    }
    "support Instruction" in {
      stanzaTypeName(Instruction(Phrase("Input", "Input"), Seq("1"), None, false)) shouldBe "Instruction"
    }
    "support all Callout stanza types" in {
      stanzaTypeName(ErrorCallout(Phrase("Input", "Input"), Seq("1"), false)) shouldBe "Error"
    }
    "support Calculation" in {
      stanzaTypeName(Calculation(Seq("1"), Seq(AddOperation("l","op","r")))) shouldBe "Calculation"
    }
    "support ValueStanza" in {
      stanzaTypeName(ValueStanza(List(Value(ScalarType, "l", "0")), Seq("1"), false)) shouldBe "Value"
    }
  }

}
