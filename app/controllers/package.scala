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

import java.time.Instant
import config.AppConfig

package object controllers {
  val SessionIdPrefix: String = "session-"

  /**
    * If last request update is available check if session has timed out
    *
    * @param session - Browser session
    * @return Returns "true" if time since last update exceeds timeout limit or is very close to the limit
    */
  def hasSessionExpired(sessionLastRequestTime: Option[String], appConfig: AppConfig, timeNow: Long = Instant.now.toEpochMilli): Boolean =
    sessionLastRequestTime.fold(false){lastRequestTs =>
      val elapsedMilliseconds = timeNow - lastRequestTs.toLong  // How many millis since last request
      // Is the elapsed period greater than the timeout minus the grace period
      elapsedMilliseconds >= (appConfig.timeoutInSeconds * 1000L -appConfig.expiryErrorMarginInMilliSeconds)
    }
}
