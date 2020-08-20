/*
 * Copyright 2020 HM Revenue & Customs
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

package models

import java.time.{Instant, ZonedDateTime}

import play.api.libs.json._

trait MongoDateTimeFormats {

  val localZoneID = ZonedDateTime.now.getZone

  implicit val zonedDateTimeRead: Reads[ZonedDateTime] =
    (__ \ "$date").read[Long].map { millis => Instant.ofEpochMilli(millis).atZone(localZoneID)}

  implicit val zonedDateTimeWrite: Writes[ZonedDateTime] = new Writes[ZonedDateTime] {
    def writes(dateTime: ZonedDateTime): JsValue = Json.obj("$date" -> dateTime.toInstant.toEpochMilli)
  }

  implicit val zonedFormats      = Format(zonedDateTimeRead, zonedDateTimeWrite)
}

object MongoDateTimeFormats extends MongoDateTimeFormats
