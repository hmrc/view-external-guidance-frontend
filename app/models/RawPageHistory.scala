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

import core.models.ocelot.FlowStage
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

final case class RawPageHistory(stanzId: String, flowStack: List[FlowStage], revertOps: List[LabelOperation] = Nil)

sealed trait LabelOperation

final case class Delete(name: String) extends LabelOperation

object Delete {
  implicit lazy val formats: OFormat[Delete] = Json.format[Delete]
}

object LabelOperation {
  implicit val reads: Reads[LabelOperation] = (js: JsValue) => {
    (js \ "action").validate[String] match {
      case err @ JsError(_) => err
      case JsSuccess(typ, _) => typ match {
        case "D" => js.validate[Delete]
      }
    }
  }

  implicit val writes: Writes[LabelOperation] = {
    case u: Delete => Json.obj("action" -> "D") ++ Json.toJsObject[Delete](u)
  }
}

object RawPageHistory {
  implicit val reads: Reads[RawPageHistory] = (
    (__ \ "stanzId").read[String] and
      (__ \ "flowStack").read[List[FlowStage]] and
        (__ \ "revertOps").readNullable[List[LabelOperation]]
    )(RawPageHistory.applyOptionalRevertOps _)

  implicit val writes: Writes[RawPageHistory] = (
    (__ \ "stanzId").write[String] and
      (__ \ "flowStack").write[List[FlowStage]] and
        (__ \ "revertOps").write[List[LabelOperation]]
    )(unlift(RawPageHistory.unapply))

  private def applyOptionalRevertOps(stanzId: String, flowStack: List[FlowStage], revertOps: Option[List[LabelOperation]]): RawPageHistory =
    RawPageHistory(stanzId, flowStack, revertOps.getOrElse(Nil))

}
