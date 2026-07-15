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

package mocks

import config.AppConfig
import play.api.Configuration
import play.api.i18n.Lang
import play.api.mvc.RequestHeader

import scala.collection.immutable.ListMap

object MockAppConfig extends AppConfig {
  val assetsPrefix: String = "someUrl"
  val languageMap: Map[String, Lang] = ListMap("en" -> Lang("en"), "cy" -> Lang("cy"))
  val config: Configuration = Configuration()
  val analyticsToken: String = "token"
  val analyticsHost: String = "host"
  val reportAProblemPartialUrl: String = "someUrl"
  val reportAProblemNonJSUrl: String = "someJsUrl"
  lazy val externalGuidanceBaseUrl: String = "http://external-guidance-base-url"
  val cookies: String = "someUrl"
  val privacy: String = "someUrl"
  val termsConditions: String = "someUrl"
  val govukHelp: String = "someUrl"
  lazy val accessibilityStatement: String = "/accessibility-statement/interactive-guidance"
  lazy val timeOutUrl: String = "/guidance/endSession"
  lazy val signOutUrl: String = "/guidance/sessionTimeout"
  val defaultSignOutUrl: String = "https://www.gov.uk"
  lazy val timeoutInSeconds: Int = 1200
  lazy val timeoutWarningInSeconds: Int = 300
  lazy val expiryErrorMarginInMilliSeconds: Int = 100
  def feedbackUrl(implicit request: RequestHeader): String = "somefeedbackUrl"
  lazy val baseUrl: String = "/guidance"
  lazy val host: String = "http://localhost:9741"
  lazy val adminHost: String = "http://adminhost"
  lazy val hostBaseUrl: String = s"${host}${baseUrl}"
  lazy val adminHostBaseUrl: String = s"${adminHost}${baseUrl}"
  lazy val pageStanzaLimit: Int = 1000
  lazy val optimizelyProjectId: Option[String] = None
  lazy val processCacheTimeoutHours: Int = 120
  lazy val processCacheScratchTimeoutHours: Int = 12
  lazy val passphraseHashKey: String = "gvBoGdgzqG1AarzF1LY0zQ=="
}
