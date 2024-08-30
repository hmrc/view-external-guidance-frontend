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

package models

import base.BaseSpec
import core.models.ocelot._
import java.time.Instant


class SessionSpec extends BaseSpec {

  "Session companion object apply" must {

    "Construct a valid Session object will correct defaults" in {
      val processCode: String = "session-test"
      val runMode: RunMode = Published;
      val processId: String = "ext90126"
      val processVersion: Long = 123456789L
      val legalPageIds: List[String] = List("start", "33")
      val lastAccessed: Instant = Instant.now
      val timescalesVersion: Option[Long] = Some(23456L)
      val ratesVersion: Option[Long] = Some(93456L)
      val sessionKey: SessionKey = SessionKey(processId, processCode)
      val session = Session.apply(sessionKey, runMode, processId, processVersion, legalPageIds, lastAccessed, timescalesVersion, ratesVersion)

      session.labels shouldBe Map()
      session.flowStack shouldBe Nil
      session.continuationPool shouldBe Map()
      session.answers shouldBe Map()
      session.rawPageHistory shouldBe Nil
      session.requestId shouldBe None
    }
  }
}
