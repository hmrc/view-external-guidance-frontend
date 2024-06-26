/*
 * Copyright 2024 HM Revenue & Customs
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

package core.models.ocelot

import play.api.libs.json._

sealed trait RunMode

case object PageReview extends RunMode
case object Scratch extends RunMode
case object Approval extends RunMode
case object Published extends RunMode
case object Debugging extends RunMode

object RunMode {
  val reads: Reads[RunMode] = {
    case JsString("PageReview") => JsSuccess(PageReview, __)
    case JsString("Scratch") => JsSuccess(Scratch, __)
    case JsString("Approval") => JsSuccess(Approval, __)
    case JsString("Published") => JsSuccess(Published, __)
    case JsString("Debugging") => JsSuccess(Debugging, __)
    case unknown => JsError(JsonValidationError(Seq("RunMode"), unknown.toString))
  }

  val writes: Writes[RunMode] = {
    case PageReview => Json.toJson("PageReview")
    case Scratch => Json.toJson("Scratch")
    case Approval => Json.toJson("Approval")
    case Published => Json.toJson("Published")
    case Debugging => Json.toJson("Debugging")
  }

  implicit val formats: Format[RunMode] = Format(reads, writes)
}
