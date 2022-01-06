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

package mocks

import config.AppConfig
import play.api.Configuration
import play.api.i18n.Lang
import play.api.mvc.RequestHeader

import scala.collection.immutable.ListMap

object MockAppConfig extends AppConfig {
  val assetsPrefix: String = "someUrl"
  val languageMap: Map[String, Lang] = ListMap("english" -> Lang("en"), "cymraeg" -> Lang("cy"))
  val config: Configuration = Configuration()
  val analyticsToken: String = "token"
  val analyticsHost: String = "host"
  val reportAProblemPartialUrl: String = "someUrl"
  val reportAProblemNonJSUrl: String = "someJsUrl"
  val externalGuidanceBaseUrl: String = "http://external-guidance-base-url"
  val cookies: String = "someUrl"
  val privacy: String = "someUrl"
  val termsConditions: String = "someUrl"
  val govukHelp: String = "someUrl"
  val accessibilityStatement: String = "/accessibility-statement/interactive-guidance"
  val signOutUrl: String = "/guidance/sessionTimeout"
  val defaultSignOutUrl: String = "https://www.gov.uk"
  val timeoutInSeconds: Int = 1200
  val timeoutWarningInSeconds: Int = 300
  val toMilliSeconds: Int = 1000
  val expiryErrorMarginInMilliSeconds: Int = 100
  def feedbackUrl(implicit request: RequestHeader): String = "somefeedbackUrl"
  val baseUrl: String = "/guidance"
  val host: String = "http://localhost:9741"
  val adminHost: String = "http://adminhost"
  val hostBaseUrl: String = s"${host}${baseUrl}"
  val adminHostBaseUrl: String = s"${adminHost}${baseUrl}"
  val pageStanzaLimit: Int = 1000
}
