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

import models.ocelot.stanzas._

import scala.annotation.tailrec

object Aggregator {

  @tailrec
  def aggregateStanzas(acc: Seq[VisualStanza])(inputSeq: Seq[VisualStanza]): Seq[VisualStanza] =
    inputSeq match {
      case Nil => acc
      case (x: Row) :: xs =>
        val (vs: VisualStanza, remainder) = aggregateRows(xs, Seq (x))
        aggregateStanzas(acc :+ vs)(remainder)

      case (x: NumberedListItemCallout) :: xs =>
        val (vs: VisualStanza, remainder) = aggregateNumLists(xs, Seq (x))
        aggregateStanzas(acc :+ vs)(remainder)

      case (x: NumberedCircleListItemCallout) :: xs =>
        val (vs: VisualStanza, remainder) = aggregateNumCircLists(xs, Seq (x))
        aggregateStanzas(acc :+ vs)(remainder)

      case (x: NoteCallout) :: xs =>
        val (vs: VisualStanza , remainder) = aggregateNotes(xs, Seq (x))
        aggregateStanzas(acc :+ vs)(remainder)

      case (x: WarningCallout) :: xs =>
        val (vs: VisualStanza , remainder) = aggregateWarning(xs, Seq (x))
        aggregateStanzas(acc :+ vs)(remainder)

      case (x: YourCallCallout) :: xs =>
        val (vs: VisualStanza, remainder) = aggregateYourCall(xs, Seq (x))
        aggregateStanzas(acc :+ vs)(remainder)

      case x :: xs => aggregateStanzas(acc :+ x)(xs)
    }

  @tailrec
  private def aggregateRows(inputSeq: Seq[VisualStanza], acc: Seq[Row]): (VisualStanza, Seq[VisualStanza]) =
    inputSeq match {
      case (x: Row) :: xs if x.stack => aggregateRows(xs, acc :+ x)
      case xs => (RowGroup(acc), xs)
    }

  @tailrec
  private def aggregateNumLists(inputSeq: Seq[VisualStanza], acc: Seq[NumberedListItemCallout]): (VisualStanza, Seq[VisualStanza]) =
    inputSeq match {
      case (x: NumberedListItemCallout) :: xs if x.stack => aggregateNumLists(xs, acc :+ x)
      case xs => (NumberedList(acc), xs)
    }

  @tailrec
  private def aggregateNumCircLists(inputSeq: Seq[VisualStanza],
                                    acc: Seq[NumberedCircleListItemCallout]): (VisualStanza, Seq[VisualStanza]) =
    inputSeq match {
      case (x: NumberedCircleListItemCallout) :: xs if x.stack => aggregateNumCircLists(xs, acc :+ x)
      case xs => (NumberedCircleList(acc), xs)
    }

  @tailrec
  private def aggregateNotes(inputSeq: Seq[VisualStanza], acc: Seq[NoteCallout]): (VisualStanza, Seq[VisualStanza]) =
    inputSeq match {
      case (x: NoteCallout) :: xs if x.stack => aggregateNotes(xs, acc :+ x)
      case xs if acc.length == 1 => (acc.head, xs)
      case xs => (NoteGroup(acc), xs)
    }

  @tailrec
  private def aggregateWarning(inputSeq: Seq[VisualStanza], acc: Seq[WarningCallout]): (VisualStanza, Seq[VisualStanza]) =
    inputSeq match {
      case (x: WarningCallout) :: xs if x.stack => aggregateWarning(xs, acc :+ x)
      case xs if acc.length == 1 => (acc.head, xs)
      case xs => (WarningText(acc), xs)
    }
  @tailrec
  private def aggregateYourCall(inputSeq: Seq[VisualStanza], acc: Seq[YourCallCallout]): (VisualStanza, Seq[VisualStanza]) =
    inputSeq match {
      case (x: YourCallCallout) :: xs if x.stack => aggregateYourCall(xs, acc :+ x)
      case xs if acc.length == 1 => (acc.head, xs)
      case xs => (YourCallGroup(acc), xs)
    }

}
