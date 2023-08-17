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

package services

import core.models.ocelot.Phrase
import core.models.ocelot.stanzas._
import models.ocelot.stanzas._

import base.BaseSpec

class StanzaAggregatorSpec extends BaseSpec {

  private trait Test {

    // Define text used in stanzas
    val title: Phrase = Phrase(Vector("Stanza aggregator test", "Welsh: Stanza aggregator test"))
    val introduction: Phrase = Phrase(Vector("Introduction", "Welsh: Introduction"))
    val instruction5Phrase: Phrase = Phrase(Vector("See above", "Welsh: See above"))

    val bp1Item1: Phrase = Phrase(Vector("First bullet point: Hello", "Welsh: First bullet point: Hello"))
    val bp1Item2: Phrase = Phrase(Vector("First bullet point: World", "Welsh: First bullet point: World"))
    val bp1Item3: Phrase = Phrase(Vector("First bullet point: !", "Welsh: First bullet point: !"))

    val dl1r1c1: Phrase = Phrase(Vector("Summary list 1 - Row 1 - Cell 1", "Welsh: Summary list 1 - Row 1 - Cell 1"))
    val dl1r1c2: Phrase = Phrase(Vector("Summary list 1 - Row 1 - Cell 2", "Welsh: Summary list 1 - Row 1 - Cell 2"))
    val dl1r1c3: Phrase = Phrase(Vector("Summary list 1 - Row 1 - Cell 3", "Welsh: Summary list 1 - Row 1 - Cell 3"))
    val dl1r2c1: Phrase = Phrase(Vector("Summary list 1 - Row 2 - Cell 1", "Welsh: Summary list 1 - Row 2 - Cell 1"))
    val dl1r2c2: Phrase = Phrase(Vector("Summary list 1 - Row 2 - Cell 2", "Welsh: Summary list 1 - Row 2 - Cell 2"))
    val dl1r2c3: Phrase = Phrase(Vector("Summary list 1 - Row 2 - Cell 3", "Welsh: Summary list 1 - Row 2 - Cell 3"))
    val dl1r3c1: Phrase = Phrase(Vector("Summary list 1 - Row 3 - Cell 1", "Welsh: Summary list 1 - Row 3 - Cell 1"))
    val dl1r3c2: Phrase = Phrase(Vector("Summary list 1 - Row 3 - Cell 2", "Welsh: Summary list 1 - Row 3 - Cell 2"))
    val dl1r3c3: Phrase = Phrase(Vector("Summary list 1 - Row 3 - Cell 3", "Welsh: Summary list 1 - Row 3 - Cell 3"))

    val dl1r1Cells: Seq[Phrase] = Seq(dl1r1c1, dl1r1c2, dl1r1c3)
    val dl1r2Cells: Seq[Phrase] = Seq(dl1r2c1, dl1r2c2, dl1r2c3)
    val dl1r3Cells: Seq[Phrase] = Seq(dl1r3c1, dl1r3c2, dl1r3c3)

    val dl2r1c1: Phrase = Phrase(Vector("Summary list 2 - Row 1 - Cell 1", "Welsh: Summary list 2 - Row 1 - Cell 1"))
    val dl2r1c2: Phrase = Phrase(Vector("Summary list 2 - Row 1 - Cell 2", "Welsh: Summary list 2 - Row 1 - Cell 2"))
    val dl2r2c1: Phrase = Phrase(Vector("Summary list 2 - Row 2 - Cell 1", "Welsh: Summary list 2 - Row 2 - Cell 1"))
    val dl2r2c2: Phrase = Phrase(Vector("Summary list 2 - Row 2 - Cell 2", "Welsh: Summary list 2 - Row 2 - Cell 2"))
    val dl2r3c1: Phrase = Phrase(Vector("Summary list 2 - Row 3 - Cell 1", "Welsh: Summary list 2 - Row 3 - Cell 1"))
    val dl2r3c2: Phrase = Phrase(Vector("Summary list 2 - Row 3 - Cell 2", "Welsh: Summary list 2 - Row 3 - Cell 2"))
    val dl2r4c1: Phrase = Phrase(Vector("Summary list 2 - Row 4 - Cell 1", "Welsh: Summary list 2 - Row 4 - Cell 1"))
    val dl2r4c2: Phrase = Phrase(Vector("Summary list 2 - Row 4 - Cell 2", "Welsh: Summary list 2 - Row 4 - Cell 2"))

    val dl2r1Cells: Seq[Phrase] = Seq(dl2r1c1, dl2r1c2)
    val dl2r2Cells: Seq[Phrase] = Seq(dl2r2c1, dl2r2c2)
    val dl2r3Cells: Seq[Phrase] = Seq(dl2r3c1, dl2r3c2)
    val dl2r4Cells: Seq[Phrase] = Seq(dl2r4c1, dl2r4c2)

    val dl3r1c1: Phrase = Phrase(Vector("Summary list 3 - Row 1 - Cell 1", "Welsh: Summary list 3 - Row 1 - Cell 1"))
    val dl3r1c2: Phrase = Phrase(Vector("Summary list 3 - Row 1 - Cell 2", "Welsh: Summary list 3 - Row 1 - Cell 2"))

    val dl3r1Cells: Seq[Phrase] = Seq(dl3r1c1, dl3r1c2)

    val bp2Item1: Phrase = Phrase(Vector("Items for sale today: water melons", "Welsh: Items for sale today: water melons"))
    val bp2Item2: Phrase = Phrase(Vector("Items for sale today: apples", "Welsh: Items for sale today: apples"))
    val bp2Item3: Phrase = Phrase(Vector("Items for sale today: onions", "Welsh: Items for sale today: onions"))
    val bp2Item4: Phrase = Phrase(Vector("Items for sale today: radishes", "Welsh: Items for sale today: radishes"))

    val instruction10Phrase: Phrase = Phrase(Vector("Further comments", "Welsh: Further comments"))

    val dl4r1c1: Phrase = Phrase(Vector("Summary list 4 - Row 1 - Cell 1", "Welsh: Summary list 4 - Row 1 - Cell 1"))
    val dl4r1c2: Phrase = Phrase(Vector("Summary list 4 - Row 1 - Cell 2", "Welsh: Summary list 4 - Row 1 - Cell 2"))
    val dl4r1c3: Phrase = Phrase(Vector("Summary list 4 - Row 1 - Cell 3", "Welsh: Summary list 4 - Row 1 - Cell 3"))

    val dl4r1Cells: Seq[Phrase] = Seq(dl4r1c1, dl4r1c2, dl4r1c3)

    val instruction11Phrase: Phrase = Phrase(Vector("The end", "Welsh: The end"))

    // Define stanzas used as input to aggregator
    val pageStanza: Stanza = PageStanza("/aggregator-test", Seq("1"), stack = false)
    val callout: Callout = TitleCallout(title, Seq("2"), stack = false)
    val instruction1: Instruction = Instruction(introduction, Seq("3"), None, stack = false)
    val instruction2: Instruction = Instruction(bp1Item1, Seq("4"), None, stack = false)
    val instruction3: Instruction = Instruction(bp1Item2, Seq("5"), None, stack = true)
    val instruction4: Instruction = Instruction(bp1Item3, Seq("6"), None, stack = true)
    val row1: Row = Row(dl1r1Cells, Seq("7"), stack = false)
    val row2: Row = Row(dl1r2Cells, Seq("8"), stack = true)
    val row3: Row = Row(dl1r3Cells, Seq("9"), stack = true)
    val instruction5: Instruction = Instruction(instruction5Phrase, Seq("10"), None, stack = false)
    val row4: Row = Row(dl2r1Cells, Seq("11"), stack = false)
    val row5: Row = Row(dl2r2Cells, Seq("12"), stack = true)
    val row6: Row = Row(dl2r3Cells, Seq("13"), stack = true)
    val row7: Row = Row(dl2r4Cells, Seq("14"), stack = true)
    val row8: Row = Row(dl3r1Cells, Seq("15"), stack = false)
    val instruction6: Instruction = Instruction(bp2Item1, Seq("16"), None, stack = false)
    val instruction7: Instruction = Instruction(bp2Item2, Seq("17"), None, stack = true)
    val instruction8: Instruction = Instruction(bp2Item3, Seq("18"), None, stack = true)
    val instruction9: Instruction = Instruction(bp2Item4, Seq("19"), None, stack = true)
    val instruction10: Instruction = Instruction(instruction10Phrase, Seq("20"), None, stack = false)
    val row9: Row = Row(dl4r1Cells, Seq("21"), stack = true)
    val instruction11: Instruction = Instruction(instruction11Phrase, Seq("end"), None, stack = false)
    val uiPreProcessTransformations: Seq[List[VisualStanza] => List[VisualStanza]] = Seq(Aggregator.aggregateStanzas(Nil))
  }

  "Stanza aggregator" must {

    "aggregate appropriate instructions and rows in a sequence of stanzas" in new Test {

      val stanzas: List[VisualStanza] = List(
        pageStanza,
        callout,
        instruction1,
        instruction2,
        instruction3,
        instruction4,
        row1,
        row2,
        row3,
        instruction5,
        row4,
        row5,
        row6,
        row7,
        row8,
        instruction6,
        instruction7,
        instruction8,
        instruction9,
        instruction10,
        row9,
        instruction11,
        EndStanza
      ).collect{case s: VisualStanza => s}


      val aggregatedStanzas: List[Stanza] = uiPreProcessTransformations.foldLeft(stanzas){case (s, t) => t(s)}

      aggregatedStanzas.head shouldBe callout
      aggregatedStanzas(1) shouldBe instruction1
      aggregatedStanzas(2) shouldBe InstructionGroup(Seq(instruction2, instruction3, instruction4))
      aggregatedStanzas(3) shouldBe RowGroup(Seq(row1, row2, row3))
      aggregatedStanzas(four) shouldBe instruction5
      aggregatedStanzas(five) shouldBe RowGroup(Seq(row4, row5, row6, row7))
      aggregatedStanzas(six) shouldBe RowGroup(Seq(row8))
      aggregatedStanzas(seven) shouldBe InstructionGroup(Seq(instruction6, instruction7, instruction8, instruction9))
      aggregatedStanzas(eight) shouldBe instruction10
      aggregatedStanzas(nine) shouldBe RowGroup(Seq(row9))
      aggregatedStanzas(ten) shouldBe instruction11
    }

    "return sequence of stanzas unchanged when no aggregation is necessary" in new Test {

      val inputStanzas: List[VisualStanza] = List(
        pageStanza,
        callout,
        instruction1,
        instruction5,
        instruction10,
        instruction11,
        EndStanza
      ).collect{case s: VisualStanza => s}

      val outputStanzas: Seq[Stanza] = uiPreProcessTransformations.foldLeft(inputStanzas){case (s, t) => t(s)}

      outputStanzas shouldBe inputStanzas
    }
  }

}
