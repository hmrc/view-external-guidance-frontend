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

package views

import core.models.ocelot.stanzas._

// $COVERAGE-OFF$

package object admin {
  def stanzaTypeName(s: PopulatedStanza): String =
    s match {
      case _: PageStanza => "Page"
      case _: Instruction => "Instruction"
      case _: TitleCallout => "Title"
      case _: SubTitleCallout => "SubTitle"
      case _: SectionCallout => "Section"
      case _: SubSectionCallout => "SubSection"
      case _: LedeCallout => "Lede"
      case _: ErrorCallout => "Error"
      case _: ValueErrorCallout => "ValueError"
      case _: TypeErrorCallout => "TypeError"
      case _: ImportantCallout => "Important"
      case _: YourCallCallout => "YourCall"
      case _: NumberedListItemCallout => "NumberedListItem"
      case _: NumberedCircleListItemCallout => "NumberedCircleListItem"
      case _: NoteCallout => "NoteCallout"
      case _: Question => "Question"
      case _: Sequence => "Sequence"
      case _: NumberInput => "NumberInput"
      case _: TextInput => "TextInput"
      case _: PassphraseInput => "PassphraseInput"
      case _: CurrencyInput => "CurrencyInput"
      case _: CurrencyPoundsOnlyInput => "CurrencyPoundsOnlyInput"
      case _: DateInput => "DateInput"
      case _: Calculation => "Calculation"
      case _: Choice => "Choice"
      case _: Row => "Row"
      case _: ValueStanza => "Value"

      case _ => "Other"
    }

  def operationName(op: Operation): String =
    op match {
      case _: AddOperation => "Add"
      case _: SubtractOperation => "Subtract"
      case _: MultiplyOperation => "Multiply"
      case _: DivideOperation => "Divide"
      case _: CeilingOperation => "Ceiling"
      case _: FloorOperation => "Floor"
    }

  def choiceTestName(op: ChoiceTest): String =
    op match {
      case _: EqualsTest => "equals"
      case _: NotEqualsTest => "not equals"
      case _: MoreThanTest => "more than"
      case _: MoreThanOrEqualsTest => "more than or equals"
      case _: LessThanTest => "less than"
      case _: LessThanOrEqualsTest => "less than or equals"
      case _: ContainsTest => "contains"
    }

  def renderIds(ids: Seq[String], pageStanzaIds: Seq[String]): String = ids.map(renderId(_, pageStanzaIds)).mkString(", ")
  def renderNext(nxt: Seq[String], pageStanzaIds: Seq[String]): String = nxt.headOption.fold("")(id => renderId(id, pageStanzaIds))
  def renderId(id: String, pageStanzaIds: Seq[String]): String = if (pageStanzaIds.contains(id)) s"<a href='#$id'>$id</a>" else s"<a href='#$id'>Page $id</a>"
}
