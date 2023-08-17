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

import core.models.ocelot.stanzas._
import models.ocelot.stanzas._
import scala.annotation.tailrec

object Aggregator {

  @tailrec
  def aggregateStanzas(acc: List[VisualStanza])(inputSeq: List[VisualStanza]): List[VisualStanza] =
    inputSeq match {
      case Nil => acc
      case (x: Row) :: xs =>
        val (vs: VisualStanza, remainder) = aggregateRows(xs, List(x))
        aggregateStanzas(acc :+ vs)(remainder)

      case (x: NumberedListItemCallout) :: xs =>
        val (vs: VisualStanza, remainder) = aggregateNumLists(xs, List(x))
        aggregateStanzas(acc :+ vs)(remainder)

      case (x: NumberedCircleListItemCallout) :: xs =>
        val (vs: VisualStanza, remainder) = aggregateNumCircLists(xs, List(x))
        aggregateStanzas(acc :+ vs)(remainder)

      case (x: NoteCallout) :: xs =>
        val (vs: VisualStanza , remainder) = aggregateNotes(xs, List(x))
        aggregateStanzas(acc :+ vs)(remainder)

      case (x: ImportantCallout) :: xs =>
        val (vs: VisualStanza , remainder) = aggregateImportant(xs, List(x))
        aggregateStanzas(acc :+ vs)(remainder)

      case (x: YourCallCallout) :: xs =>
        val (vs: VisualStanza, remainder) = aggregateYourCall(xs, List(x))
        aggregateStanzas(acc :+ vs)(remainder)

      case (x: TypeErrorCallout) :: xs =>
        val (vs: VisualStanza, remainder) = aggregateTypeError(xs, List(x))
        aggregateStanzas(acc :+ vs)(remainder)

      case (x: ErrorCallout) :: xs =>
        val (vs: VisualStanza, remainder) = aggregateError(xs, List(x))
        aggregateStanzas(acc :+ vs)(remainder)

      case (x: Instruction) :: xs =>
        val (vs: VisualStanza, remainder) = aggregateBulletPointInstruction(xs, List(x))
        aggregateStanzas(acc :+ vs)(remainder)

      case x :: xs => aggregateStanzas(acc :+ x)(xs)
    }

  @tailrec
  private def aggregateRows(inputSeq: List[VisualStanza], acc: List[Row]): (VisualStanza, List[VisualStanza]) =
    inputSeq match {
      case (x: Row) :: xs if x.stack => aggregateRows(xs, acc :+ x)
      case xs => (RowGroup(acc), xs)
    }

  @tailrec
  private def aggregateNumLists(inputSeq: List[VisualStanza], acc: List[NumberedListItemCallout]): (VisualStanza, List[VisualStanza]) =
    inputSeq match {
      case (x: NumberedListItemCallout) :: xs if x.stack => aggregateNumLists(xs, acc :+ x)
      case xs => (NumberedList(acc), xs)
    }

  @tailrec
  private def aggregateNumCircLists(inputSeq: List[VisualStanza],
                                    acc: List[NumberedCircleListItemCallout]): (VisualStanza, List[VisualStanza]) =
    inputSeq match {
      case (x: NumberedCircleListItemCallout) :: xs if x.stack => aggregateNumCircLists(xs, acc :+ x)
      case xs => (NumberedCircleList(acc), xs)
    }

  @tailrec
  private def aggregateNotes(inputSeq: List[VisualStanza], acc: List[NoteCallout]): (VisualStanza, List[VisualStanza]) =
    inputSeq match {
      case (x: NoteCallout) :: xs if x.stack => aggregateNotes(xs, acc :+ x)
      case xs if acc.length == 1 => (acc.head, xs)
      case xs => (NoteGroup(acc), xs)
    }

  @tailrec
  private def aggregateImportant(inputSeq: List[VisualStanza], acc: List[ImportantCallout]): (VisualStanza, List[VisualStanza]) =
    inputSeq match {
      case (x: ImportantCallout) :: xs if x.stack => aggregateImportant(xs, acc :+ x)
      case xs if acc.length == 1 => (acc.head, xs)
      case xs => (ImportantGroup(acc), xs)
    }
  @tailrec
  private def aggregateYourCall(inputSeq: List[VisualStanza], acc: List[YourCallCallout]): (VisualStanza, List[VisualStanza]) =
    inputSeq match {
      case (x: YourCallCallout) :: xs if x.stack => aggregateYourCall(xs, acc :+ x)
      case xs if acc.length == 1 => (acc.head, xs)
      case xs => (YourCallGroup(acc), xs)
    }

  @tailrec
  private def aggregateError(inputSeq: List[VisualStanza], acc: List[ErrorCallout]): (VisualStanza, List[VisualStanza]) =
    inputSeq match {
      case (x: ErrorCallout) :: xs if x.stack => aggregateError(xs, acc :+ x)
      case xs if acc.length == 1 => (acc.head, xs)
      case xs => (RequiredErrorGroup(acc), xs)
    }

  @tailrec
  private def aggregateTypeError(inputSeq: List[VisualStanza], acc: List[TypeErrorCallout]): (VisualStanza, List[VisualStanza]) =
    inputSeq match {
      case (x: TypeErrorCallout) :: xs if x.stack => aggregateTypeError(xs, acc :+ x)
      case xs if acc.length == 1 => (acc.head, xs)
      case xs => (TypeErrorGroup(acc), xs)
    }

  @tailrec
  private def aggregateBulletPointInstruction(inputSeq: List[VisualStanza], acc: List[Instruction]): (VisualStanza, List[VisualStanza]) =
    inputSeq match {
      case (x: Instruction) :: xs if x.stack && BulletPointBuilder.matchPhrases(acc.last.text, x.text)=> aggregateBulletPointInstruction(xs, acc :+ x)
      case xs if acc.length == 1 => (acc.head, xs)
      case xs => (InstructionGroup(acc), xs)
    }

}
