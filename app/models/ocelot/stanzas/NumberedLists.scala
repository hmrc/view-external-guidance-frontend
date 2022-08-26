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

package models.ocelot.stanzas

import core.models.ocelot.stanzas.{NumberedListItemCallout, NumberedCircleListItemCallout, VisualStanza}
import core.models.ocelot.Phrase

case class NumberedList(override val next: Seq[String], group: Seq[NumberedListItemCallout], stack: Boolean) extends VisualStanza {
  override def rendered(expand: Phrase => Phrase): VisualStanza =
    NumberedList(next, group.map(_.rendered(expand)).collect{case n: NumberedListItemCallout => n}, stack)
}

object NumberedList {
  def apply(group: Seq[NumberedListItemCallout]): NumberedList =
    group match {
      case Nil => NumberedList(Seq.empty, Seq.empty, false)
      case _ => NumberedList(group.last.next, group, group.head.stack)
    }
}

case class NumberedCircleList(override val next: Seq[String], group: Seq[NumberedCircleListItemCallout], stack: Boolean) extends VisualStanza {
  override def rendered(expand: Phrase => Phrase): VisualStanza =
    NumberedCircleList(next, group.map(_.rendered(expand)).collect{case n: NumberedCircleListItemCallout => n}, stack)
}

object NumberedCircleList {
  def apply(group: Seq[NumberedCircleListItemCallout]): NumberedCircleList =
    group match {
      case Nil => NumberedCircleList(Seq.empty, Seq.empty, false)
      case _ => NumberedCircleList(group.last.next, group, group.head.stack)
    }
}