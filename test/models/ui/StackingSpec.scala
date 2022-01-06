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

package models.ui

import base.BaseSpec
import core.models.ocelot._
import core.models.ocelot.stanzas._
import models.ocelot.stanzas._

class StackingSpec extends BaseSpec {

  trait Test {
    def stackedTitle(stack: Boolean): TitleCallout = TitleCallout(Phrase(), Seq.empty, stack)
    def stackedError(stack: Boolean): ErrorCallout = ErrorCallout(Phrase(), Seq.empty, stack)
    def stackedRowGroup(stack: Boolean): RowGroup = RowGroup(Seq.empty, Seq.empty, stack)
  }

  "Stacking" should {
    "Group stacked stanzas together" in new Test {
      val stanzas = Seq(stackedTitle(false), stackedTitle(false), stackedTitle(true), stackedTitle(false), stackedRowGroup(true))

      val stacked = stackStanzas(Nil)(stanzas)

      stacked.length shouldBe 3
      stacked(0) shouldBe stackedTitle(false)
      stacked(1) shouldBe StackedGroup(Seq(stackedTitle(false), stackedTitle(true)))
      stacked(2) shouldBe StackedGroup(Seq(stackedTitle(false), stackedRowGroup(true)))
    }

    "Group stacked stanzas including initial isolated unstackable stanza marked stack" in new Test {
      val stanzas = Seq(stackedTitle(true), stackedTitle(false), stackedTitle(true), stackedTitle(false), stackedRowGroup(true))

      val stacked = stackStanzas(Nil)(stanzas)

      stacked.length shouldBe 3
      stacked(0) shouldBe stackedTitle(true)
      stacked(1) shouldBe StackedGroup(Seq(stackedTitle(false), stackedTitle(true)))
      stacked(2) shouldBe StackedGroup(Seq(stackedTitle(false), stackedRowGroup(true)))
    }

    "Group stacked stanzas including embedded isolated unstackable stanza marked stack" in new Test {
      val stanzas = Seq(stackedTitle(true), stackedTitle(false), stackedError(false), stackedTitle(false), stackedRowGroup(true))

      val stacked = stackStanzas(Nil)(stanzas)

      stacked.length shouldBe 4
      stacked(0) shouldBe stackedTitle(true)
      stacked(1) shouldBe stackedTitle(false)
      stacked(2) shouldBe stackedError(false)
      stacked(3) shouldBe StackedGroup(Seq(stackedTitle(false), stackedRowGroup(true)))
    }


    "Group stacked stanzas in alternate group" in new Test {
      val stanzas = Seq(stackedTitle(true), stackedTitle(true), stackedError(false), stackedTitle(false), stackedRowGroup(true))

      val stacked = stackStanzas(Nil)(stanzas)
      stacked.length shouldBe 3
      stacked(0) shouldBe StackedGroup(Seq(stackedTitle(true), stackedTitle(true)))
      stacked(1) shouldBe stackedError(false)
      stacked(2) shouldBe StackedGroup(Seq(stackedTitle(false), stackedRowGroup(true)))
    }

    "Group stacked stanzas in alternate group 2" in new Test {
      val stanzas = Seq(stackedTitle(true), stackedTitle(true), stackedError(true), stackedTitle(true), stackedRowGroup(false))

      val stacked = stackStanzas(Nil)(stanzas)
      stacked.length shouldBe 2
      stacked(0) shouldBe StackedGroup(Seq(stackedTitle(true), stackedTitle(true), stackedError(true), stackedTitle(true)))
      stacked(1) shouldBe stackedRowGroup(false)
    }

  }
}
