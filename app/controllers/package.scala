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
import play.api.i18n.Messages
import core.models.ocelot.errors._
import models.errors.ErrorReport

package object controllers {
  val SessionIdPrefix: String = "session-"

  def hasSessionExpired(sessionLastRequestTime: Option[String], appConfig: AppConfig, timeNow: Long = Instant.now.toEpochMilli): Boolean =
    sessionLastRequestTime.fold(false){lastRequestTs =>
      val elapsedMilliseconds = timeNow - lastRequestTs.toLong  // How many millis since last request
      // Is the elapsed period greater than the timeout minus the grace period
      elapsedMilliseconds >= (appConfig.timeoutInSeconds * 1000L -appConfig.expiryErrorMarginInMilliSeconds)
    }

  def fromRuntimeError(err: RuntimeError, stanzaId: String)(implicit messages: Messages): String = err match {
    case e: UnsupportedOperationError => messages("guidance.error.unsupported_operation", stanzaId, e.op, e.left, e.right)
    case NonTerminatingPageError => messages("guidance.error.nonterminating_loop", stanzaId)
    case UnsupportedUiPatternError => messages("guidance.error.unsupported_ui_pattern", stanzaId)
  }

  def errorSolutions(errors: List[RuntimeError], stanzaId: String)(implicit messages: Messages): List[List[String]] = {
    println(UnsupportedOperationError.getClass().getSimpleName())
    println(NonTerminatingPageError.getClass().getSimpleName())
    println(UnsupportedUiPatternError.getClass().getSimpleName())
    errors.groupBy(_.getClass.getSimpleName()).keys.toList.map{
      case "UnsupportedOperationError" => List(messages("guidance.error.unsupported_operation.soln"))
      case "NonTerminatingPageError" => List(messages("guidance.error.nonterminating_loop.soln"))
      case "UnsupportedUiPatternError" =>
        List(messages("guidance.error.unsupported_ui_pattern.soln"),
             messages("guidance.error.unsupported_ui_pattern.soln1"),
             messages("guidance.error.unsupported_ui_pattern.soln2"),
             messages("guidance.error.unsupported_ui_pattern.soln3"))
    }
  }


}
