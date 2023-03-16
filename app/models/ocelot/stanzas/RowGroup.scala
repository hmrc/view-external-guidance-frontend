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

import core.models.ocelot.{isLinkOnlyPhrase, isBoldOnlyPhrase}
import core.models.ocelot.stanzas.{Row, VisualStanza}
import core.models.ocelot.Phrase

case class RowGroup (override val next: Seq[String], group: Seq[Row], stack: Boolean) extends VisualStanza {
  lazy val columnCount:Int = group.headOption.fold(0)(_ => group.map(_.cells.length).max)
  lazy val paddedRows: Seq[Seq[Phrase]] = group.map(row => (row.cells ++ Seq.fill(columnCount - row.cells.size)(Phrase())))
  lazy val isTableCandidate: Boolean = columnCount > 0 &&                           // At least one column
                                       group.length > 1 &&                          // A row of column headings and at least one data row
                                       group.head.cells.forall(isBoldOnlyPhrase(_)) // Column headings are all bold
  lazy val isCYASummaryList: Boolean = columnCount == 3 && // Considering some short columns, overall column count must be 3
                                       group.forall(r => !isBoldOnlyPhrase(r.cells(0)) && (r.cells.length < 3 || isLinkOnlyPhrase(r.cells(2))))
  lazy val isNameValueSummaryList: Boolean = columnCount == 2 &&                    // Two columns
                                             group.forall(r => !isBoldOnlyPhrase(r.cells(0))) // All the initial column cells are not bold

  override def rendered(expand: Phrase => Phrase): VisualStanza = RowGroup(next, group.map(_.rendered(expand)).collect{case r: Row => r}, stack)
}

object RowGroup {
  def apply(group: Seq[Row]): RowGroup =
    group match {
      case Nil => RowGroup(Seq.empty, Seq.empty, false)
      case _ => RowGroup(group.last.next, group, group.head.stack)
    }
}