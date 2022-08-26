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
//
// PopulatedStanza stacking, Caption and RowGroup requirements
// 1. First Row all cells non-blank and all bold to encode <thead> containing row of <th> elements
// 2. RowGroup stacked to a SubSection Callout, encode callout text as a <caption> within <table>
// 3. Cell contents numeric (lable ref with output format currency), add the govuk-table__cell--numeric class
case class Table(caption: Text, headingRow: Seq[Text], rows: Seq[Seq[Text]]) extends UIComponent {
  val numericColumns: Seq[Boolean] = (for(colIdx <- headingRow.indices) yield {
    rows.map(r => r(colIdx).isNumericLabelRef)
  }).map(l => l.forall(x => x))

  val text: Text = caption
}