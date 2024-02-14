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

import java.time.Instant
import config.AppConfig
import play.api.i18n.Messages
import core.models.ocelot.errors._
import core.models.ocelot.UrlPathPattern
import uk.gov.hmrc.http.SessionKeys
import play.api.mvc.Request


package object controllers {
  val SessionIdPrefix: String = "session-"
  val RedirectWhenNoSessionUrlParam: String = "c=1"

  def sessionStillActive(request: Request[_], appConfig: AppConfig, timeNow: Long = Instant.now.toEpochMilli): Boolean =
    request.session.get(SessionKeys.lastRequestTimestamp).fold(false){lastRequestTs =>
      val elapsedMilliseconds = timeNow - lastRequestTs.toLong  // How many millis since last request
      // Is the elapsed period greater than the timeout minus the grace period
      elapsedMilliseconds < (appConfig.timeoutInSeconds * 1000L -appConfig.expiryErrorMarginInMilliSeconds)
    }

  def fromRuntimeError(err: RuntimeError, stanzaId: String)(implicit messages: Messages): String = err match {
    case e: UnsupportedOperationError => 
      messages("guidance.error.unsupported_operation", 
               stanzaId, e.op, e.left, e.right, e.lvalue.getOrElse("UNINITIALISED"), e.rvalue.getOrElse("UNINITIALISED"))
    case NonTerminatingPageError => messages("guidance.error.nonterminating_loop", stanzaId)
    case UnsupportedUiPatternError => messages("guidance.error.unsupported_ui_pattern", stanzaId)
    case e: DivideByZeroError => messages("guidance.error.divide_by_zero", stanzaId, e.left, e.right)
    case e: ProgrammingError => messages("guidance.error.programming_error", e.msg, stanzaId)
  }

  def errorSolutions(errors: List[RuntimeError], stanzaId: String)(implicit messages: Messages): List[List[String]] =
    List(errors.collectFirst{
      case e: UnsupportedOperationError => List(messages("guidance.error.unsupported_operation.soln"))
    }, errors.collectFirst{
      case NonTerminatingPageError => List(messages("guidance.error.nonterminating_loop.soln"))
    }, errors.collectFirst{
      case UnsupportedUiPatternError =>
         List(messages("guidance.error.unsupported_ui_pattern.soln"),
             messages("guidance.error.unsupported_ui_pattern.soln1"),
             messages("guidance.error.unsupported_ui_pattern.soln2"),
             messages("guidance.error.unsupported_ui_pattern.soln3"),
             messages("guidance.error.unsupported_ui_pattern.soln4"))
    }, errors.collectFirst{
      case e: DivideByZeroError => List(messages("guidance.error.divide_by_zero.soln"))
    }, errors.collectFirst{
      case e: ProgrammingError => List(messages("guidance.error.programming_error", e.msg, stanzaId))
    }
    ).collect{case Some(s) => s}

  def decodeUrlPath(url: String): Option[String] =
    try { Some(java.net.URLDecoder.decode(url, "UTF-8"))}
    catch{case _: Throwable => None}

  def validateUrl(url: String): Option[String] =
    decodeUrlPath(url).flatMap{
      case decodedUrl: String if decodedUrl.matches(UrlPathPattern) => Some(decodedUrl)
      case _ => None
    }
}
