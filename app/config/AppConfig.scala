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

package config

import javax.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.i18n.Lang
import play.api.mvc.RequestHeader
import uk.gov.hmrc.play.bootstrap.binders.SafeRedirectUrl
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.collection.immutable.ListMap

trait AppConfig {
  val assetsPrefix: String
  val analyticsToken: String
  val analyticsHost: String
  val reportAProblemPartialUrl: String
  val reportAProblemNonJSUrl: String
  val languageMap: Map[String, Lang]
  val externalGuidanceBaseUrl: String
  val config: Configuration
  val cookies: String
  val privacy: String
  val termsConditions: String
  val govukHelp: String
  val signOutUrl: String
  val timeoutInSeconds: Int
  val timeoutWarningInSeconds: Int
  def feedbackUrl(implicit request: RequestHeader): String
  val gtmContainer: String
  val baseUrl: String
  val baseHostUrl: String
}

@Singleton
class AppConfigImpl @Inject() (val config: Configuration, servicesConfig: ServicesConfig) extends AppConfig {
  private val contactBaseUrl = servicesConfig.baseUrl("contact-frontend")

  private val assetsUrl = config.get[String]("assets.url")
  val serviceIdentifier = "EGVWR"
  lazy val host: String = servicesConfig.getString("host")
  lazy val betaFeedback: String = servicesConfig.getString("betafeedback")

  val assetsPrefix: String = assetsUrl + config.get[String]("assets.version")
  val analyticsToken: String = config.get[String](s"google-analytics.token")
  val analyticsHost: String = config.get[String](s"google-analytics.host")
  val reportAProblemPartialUrl: String = s"$contactBaseUrl/contact/problem_reports_ajax?service=$serviceIdentifier"
  val reportAProblemNonJSUrl: String = s"$contactBaseUrl/contact/problem_reports_nonjs?service=$serviceIdentifier"
  val languageMap: Map[String, Lang] = ListMap("english" -> Lang("en"), "cymraeg" -> Lang("cy"))

  def feedbackUrl(implicit request: RequestHeader): String =
    s"$contactBaseUrl$betaFeedback?service=$serviceIdentifier&backUrl=${SafeRedirectUrl(host + request.uri).encodedUrl}"

  lazy val externalGuidanceBaseUrl: String = servicesConfig.baseUrl("external-guidance")

  lazy val cookies: String = config.get[String]("urls.footer.cookies")
  lazy val privacy: String = config.get[String]("urls.footer.privacy")
  lazy val termsConditions: String = config.get[String]("urls.footer.termsConditions")
  lazy val govukHelp: String = config.get[String]("urls.footer.govukHelp")
  lazy val gtmContainer: String = config.get[String]("gtm.container")

  lazy val signOutUrl: String = config.get[String]("session-timeout.signOutUrl")
  lazy val timeoutInSeconds: Int = config.get[Int]("session-timeout.seconds")
  lazy val timeoutWarningInSeconds: Int = config.get[Int]("session-timeout.warning")
  lazy val baseUrl: String = config.get[String]("urls.baseUrl")
  lazy val baseHostUrl: String = s"$host$baseUrl"

}
