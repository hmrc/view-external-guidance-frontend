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

package models.admin

import core.models.ocelot.{Label, Labels}
import core.models.ocelot.ScalarLabel
import core.models.ocelot.ListLabel

case class DebugLabelRow(name: String, dataType: String, initialValue: Option[String], updatedValue: Option[String])

case class DebugInformation(processPageStructure: Option[ProcessPageStructure], preRenderLabels: Option[Labels], postRenderLabels: Option[Labels]) {

  def labels(): List[DebugLabelRow] = {
    val allPostRenderLabels: Map[String, Label] = postRenderLabels.fold(Map[String, Label]())(pr => pr.flush().labelMap)
    val allPreRenderLabels: Map[String, Label] = preRenderLabels.fold(Map[String, Label]())(pr => pr.flush().labelMap)
    val names: List[String] = (allPreRenderLabels.keySet.toList ++ allPostRenderLabels.keySet.toList).distinct
    val allLabels: Map[String, Label] = allPreRenderLabels ++ allPostRenderLabels

    val all: List[DebugLabelRow] = names.map{n =>
      allLabels(n) match {
        case s: ScalarLabel => 
          DebugLabelRow(n, "Scalar", preRenderLabels.fold[Option[String]](None)(_.flush().value(n)),
                                    postRenderLabels.fold[Option[String]](None)(_.flush().value(n)))
        case l: ListLabel => 
          DebugLabelRow(n, "List", preRenderLabels.fold[Option[String]](None)(_.flush().valueAsList(n).map(_.mkString(","))), 
                                   postRenderLabels.fold[Option[String]](None)(_.flush().valueAsList(n).map(_.mkString(","))))
      }
    }
    all.filterNot(_.initialValue.isDefined) ::: all.filter(_.initialValue.isDefined) // Show new labels (undefined initial value) first in list
  }
}
