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

package models

import play.api.libs.json._
import core.models.ocelot._
import core.models.ocelot.stanzas.Stanza
import java.time.Instant
import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats.Implicits._

case class SessionKey(id: String, processCode: String)

object SessionKey {
  implicit lazy val format: Format[SessionKey] = Json.format[SessionKey]
}

final case class Session(
    _id: SessionKey,
   runMode: Option[RunMode],
   processId: String,
   labels: Map[String, Label],
   flowStack: List[FlowStage],
   continuationPool: Map[String, Stanza],
   answers: Map[String, String],
   rawPageHistory: List[RawPageHistory],
   legalPageIds: List[String],
   requestId: Option[String],
   lastAccessed: Instant, // expiry time
   processVersion: Long,
   timescalesVersion : Option[Long],
   ratesVersion : Option[Long]
)

object Session {
  def apply(key: SessionKey,
            runMode: RunMode,
            processId: String,
            processVersion: Long,
            legalPageIds: List[String],
            lastAccessed: Instant = Instant.now,
            timescalesVersion : Option[Long],
            ratesVersion : Option[Long]): Session =
    Session(key, Some(runMode), processId, Map(), Nil, Map(), Map(), Nil, legalPageIds, None, lastAccessed, processVersion, timescalesVersion, ratesVersion)

  implicit lazy val format: Format[Session] = Json.format[Session]
}
