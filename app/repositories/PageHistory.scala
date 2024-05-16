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

package repositories

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import core.models.ocelot.FlowStage

final case class PageHistory(url: String, flowStack: List[FlowStage])

object PageHistory {
  implicit val reads: Reads[PageHistory] = (
    (__ \ "url").read[String] and
      (__ \ "flowStack").read[List[FlowStage]]
  )(PageHistory.apply _)

  implicit val writes: Writes[PageHistory] = (
    (__ \ "url").write[String] and
      (__ \ "flowStack").write[List[FlowStage]]
  )(unlift(PageHistory.unapply))
}

final case class RawPageHistory(id: String, flowStack: List[FlowStage])

object RawPageHistory {
  implicit val reads: Reads[RawPageHistory] = (
    (__ \ "id").read[String] and
      (__ \ "flowStack").read[List[FlowStage]]
  )(RawPageHistory.apply _)

  implicit val writes: Writes[RawPageHistory] = (
    (__ \ "id").write[String] and
      (__ \ "flowStack").write[List[FlowStage]]
  )(unlift(RawPageHistory.unapply))
}
