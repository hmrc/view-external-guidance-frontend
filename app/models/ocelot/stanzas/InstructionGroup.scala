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

package models.ocelot.stanzas

import core.models.ocelot.stanzas.{Instruction, VisualStanza}
import core.models.ocelot.Phrase

/**
  * Case class InstructionGroup is a container class for groups of Instructions
  * with similar leading text that comprise bullet point lists
  *
  * @param next - Identifier for next stanza in page
  * @param group - A group of instructions
  */
case class InstructionGroup(override val next: Seq[String], group: Seq[Instruction], stack: Boolean) extends VisualStanza {
  override def rendered(expand: Phrase => Phrase): VisualStanza =
    InstructionGroup(next, group.map(_.rendered(expand)).collect{case ig: Instruction => ig}, stack)
}

object InstructionGroup {

  def apply(group: Seq[Instruction], stack: Boolean = false): InstructionGroup = InstructionGroup(group.last.next, group, stack)

}
