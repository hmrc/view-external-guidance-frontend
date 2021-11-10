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

package models.ui

import base.BaseSpec

import models.ocelot.stanzas.YourCallGroup

class GroupSpec extends BaseSpec {

  "Grouping no YourCallCallouts into a group" should {
    "Create an empty group" in {
      YourCallGroup(Seq.empty) shouldBe YourCallGroup(Seq.empty, Seq.empty, false)
    }
  }
}
