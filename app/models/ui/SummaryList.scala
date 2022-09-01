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

// $COVERAGE-OFF$

package models.ui

sealed trait SummaryList {
  val columnCount: Int
  val text = Text()
  val rows: Seq[Seq[Text]]
  val numericColumns: Seq[Boolean]
  lazy val validRows = rows.forall(r => r.length == columnCount)
}

case class CyaSummaryList(val rows: Seq[Seq[Text]]) extends SummaryList with UIComponent {
  val columnCount: Int = 3
  val numericColumns: Seq[Boolean] = Seq.fill(3)(false)
}

case class NameValueSummaryList(val rows: Seq[Seq[Text]]) extends SummaryList with UIComponent {
  val columnCount: Int = 2
  lazy val numericColumns: Seq[Boolean] = Seq(false, validRows && rows.forall(r => r(1).isNumeric))
}