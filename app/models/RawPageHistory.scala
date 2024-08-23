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

// $COVERAGE-OFF$

package models

import core.models.ocelot.{FlowStage, Label}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

final case class RawPageHistory(stanzId: String, revertOps: List[LabelOperation], flowStack: List[FlowStage])

sealed trait LabelOperation

  final case class Delete(name: String) extends LabelOperation
  object Delete {
    implicit lazy val formats: OFormat[Delete] = Json.format[Delete]
  }

  final case class Update(l: Label) extends LabelOperation
  object Update {
    implicit lazy val formats: OFormat[Update] = Json.format[Update]
}

object LabelOperation {
  implicit val reads: Reads[LabelOperation] = (js: JsValue) => {
    (js \ "t").validate[String] match {
      case err @ JsError(_) => err
      case JsSuccess(typ, _) => typ match {
        case "D" => js.validate[Delete]
        case "U" => js.validate[Update]
      }
    }
  }

  implicit val writes: Writes[LabelOperation] = {
    case d: Delete => Json.obj("t" -> "D") ++ Json.toJsObject[Delete](d)
    case u: Update => Json.obj("t" -> "U") ++ Json.toJsObject[Update](u)  }
}

object RawPageHistory {
  implicit val reads: Reads[RawPageHistory] = (
    (__ \ "stanzId").read[String] and
      (__ \ "revertOps").readNullable[List[LabelOperation]] and
      (__ \ "flowStack").read[List[FlowStage]]
    )(RawPageHistory.applyOptionalRevertOps _)

  implicit val writes: Writes[RawPageHistory] = (
    (__ \ "stanzId").write[String] and
      (__ \ "revertOps").write[List[LabelOperation]] and
      (__ \ "flowStack").write[List[FlowStage]]
    )(unlift(RawPageHistory.unapply))

  private def applyOptionalRevertOps(stanzId: String, revertOps: Option[List[LabelOperation]], flowStack: List[FlowStage]): RawPageHistory =
    RawPageHistory(stanzId, revertOps.getOrElse(Nil), flowStack)

}
