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

package models.errors

import base.BaseSpec
import core.models.ocelot.errors._

class ErrorsPackageSpec extends BaseSpec {

  "fromRuntimeError" should {
    "translate UnsupportedOperationError" in {
      val message = fromRuntimeError(UnsupportedOperationError("AddOperation", "lvalue", "rvalue", "left", "right"), "stanzaId")
      message shouldBe "UnsupportedOperationError: Operation AddOperation, left left (lvalue), right right (rvalue) on stanza stanzaId"
    }

    "translate NonTerminatingPageError" in {
      val message = fromRuntimeError(NonTerminatingPageError, "stanzaId")
      message shouldBe "NonTerminatingPageError: Guidance contains non-terminating loop which includes stanza stanzaId"
    }

    "translate UnsupportedUiPatternError" in {
      val message = fromRuntimeError(UnsupportedUiPatternError, "stanzaId")
      message shouldBe "UnsupportedUiPatternError: Stanza grouping (including stanza stanzaId) does not form a GDS UI pattern"
    }

  }
}
