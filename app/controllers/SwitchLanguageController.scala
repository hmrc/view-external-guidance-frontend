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

package controllers

import com.google.inject.{Inject, Singleton}
import play.api.mvc._
import play.api.i18n.Lang
import config.AppConfig
import play.api.Logger
import uk.gov.hmrc.play.language.{LanguageController, LanguageUtils}

@Singleton
class SwitchLanguageController @Inject() (appConfig: AppConfig, languageUtils: LanguageUtils, cc: MessagesControllerComponents)
    extends LanguageController(languageUtils, cc) {

  val logger: Logger = Logger(getClass)

  override def languageMap: Map[String, Lang] = appConfig.languageMap
  private val SwitchIndicatorKey = "switching-language"
  private val FlashWithSwitchIndicator = Flash(Map(SwitchIndicatorKey -> "true"))
  override def fallbackURL: String = appConfig.accessibilityStatement

  override def switchToLanguage(language: String): Action[AnyContent] = Action { implicit request =>
    val enabled: Boolean = languageMap.get(language).exists(languageUtils.isLangAvailable)
    val lang: Lang =
      if (enabled) languageMap.getOrElse(language, languageUtils.getCurrentLang) else languageUtils.getCurrentLang

    val relativeRedirectUrl: String = request.headers.get(REFERER).collect{
      case url if url.startsWith(appConfig.hostBaseUrl) => url.drop(appConfig.host.length)
      case url if url.startsWith(appConfig.adminHostBaseUrl) => url.drop(appConfig.adminHost.length)
    }.getOrElse(fallbackURL)

    logger.info(s"Redirecting to $relativeRedirectUrl")
    Redirect(relativeRedirectUrl).withLang(Lang.apply(lang.code)).flashing(FlashWithSwitchIndicator)
  }
}
