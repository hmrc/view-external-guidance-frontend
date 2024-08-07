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

package config

import javax.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.i18n.Lang
import play.api.mvc.RequestHeader
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.http.StringContextOps

import scala.collection.immutable.ListMap

trait AppConfig {
  val reportAProblemPartialUrl: String
  val reportAProblemNonJSUrl: String
  val languageMap: Map[String, Lang]
  val externalGuidanceBaseUrl: String
  val config: Configuration
  val accessibilityStatement: String
  val signOutUrl: String
  val timeOutUrl: String
  val timeoutInSeconds: Int
  val timeoutWarningInSeconds: Int
  val expiryErrorMarginInMilliSeconds: Int
  def feedbackUrl(implicit request: RequestHeader): String
  val host: String
  val adminHost: String
  val baseUrl: String
  val hostBaseUrl: String
  val adminHostBaseUrl: String
  val pageStanzaLimit: Int
  val processCacheTimeoutHours: Int
  val processCacheScratchTimeoutHours: Int
  val passphraseHashKey: String
}

@Singleton
class AppConfigImpl @Inject() (val config: Configuration, servicesConfig: ServicesConfig) extends AppConfig {
  val serviceIdentifier = "EGVWR"
  val SessionTimeoutURL = "/session-timeout"
  val EndSessionURL = "/end-session"
  val PageStanzaCountLimit = 1000
  private val contactBaseUrl = servicesConfig.baseUrl("contact-frontend")
  lazy val host: String = servicesConfig.getString("host")
  lazy val adminHost: String = servicesConfig.getString("adminHost")
  lazy val betaFeedback: String = servicesConfig.getString("betafeedback")

  val reportAProblemPartialUrl: String = s"$contactBaseUrl/contact/problem_reports_ajax?service=$serviceIdentifier"
  val reportAProblemNonJSUrl: String = s"$contactBaseUrl/contact/problem_reports_nonjs?service=$serviceIdentifier"
  val languageMap: Map[String, Lang] = ListMap("en" -> Lang("en"), "cy" -> Lang("cy"))

  def feedbackUrl(implicit request: RequestHeader): String = {
    val urlString = s"$contactBaseUrl$betaFeedback?service=$serviceIdentifier&backUrl=$host${request.uri}"
    url"$urlString".toExternalForm()
  }

  lazy val externalGuidanceBaseUrl: String = servicesConfig.baseUrl("external-guidance")
  lazy val accessibilityStatement: String = config.get[String]("urls.footer.accessibilityStatement")
  lazy val signOutUrl: String = config.getOptional[String]("session-timeout.signOutUrl").getOrElse(EndSessionURL)
  lazy val timeOutUrl: String = config.getOptional[String]("session-timeout.timeOutUrl").getOrElse(SessionTimeoutURL)
  lazy val timeoutInSeconds: Int = config.get[Int]("session-timeout.seconds")
  lazy val timeoutWarningInSeconds: Int = config.get[Int]("session-timeout.warning")
  lazy val expiryErrorMarginInMilliSeconds: Int = config.get[Int]("session-timeout.expiryErrorMarginInMilliSeconds")
  lazy val baseUrl: String = config.get[String]("urls.baseUrl")
  lazy val hostBaseUrl: String = s"$host$baseUrl"
  lazy val adminHostBaseUrl: String = s"$adminHost$baseUrl"
  lazy val pageStanzaLimit: Int = config.getOptional[Int]("page-rendering.page-stanza-limit").getOrElse(PageStanzaCountLimit)
  lazy val processCacheTimeoutHours: Int = config.get[Int]("session-process-cache.timeoutHours")
  lazy val processCacheScratchTimeoutHours: Int = config.get[Int]("session-process-cache.scratchTimeoutHours")
  lazy val passphraseHashKey: String = config.get[String]("passphrase-hashkey")
}
