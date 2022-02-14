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

package config

import javax.inject.{Inject, Singleton}
import play.api.i18n.{Messages, MessagesApi}
import play.api.mvc.Request
import play.twirl.api.Html
import uk.gov.hmrc.play.bootstrap.frontend.http.FrontendErrorHandler
import views.html.error_template

@Singleton
class ErrorHandler @Inject() (val messagesApi: MessagesApi, view: error_template, implicit val appConfig: AppConfig) extends FrontendErrorHandler {

  override def standardErrorTemplate(pageTitle: String, heading: String, message: String)(implicit request: Request[_]): Html =
    view(pageTitle, heading, message, None, false)

  def standardErrorTemplate(pageTitle: String, heading: String, message: String, processCode: Option[String], betaPhaseBanner: Boolean)(implicit request: Request[_]): Html =
    view(pageTitle, heading, message, processCode, betaPhaseBanner)

  def badRequestTemplateWithProcessCode(processCode: Option[String], betaPhaseBanner: Boolean)(implicit request: Request[_]): Html =
    standardErrorTemplate(
      Messages("global.error.badRequest400.title"),
      Messages("global.error.badRequest400.heading"),
      Messages("global.error.badRequest400.message"),
      processCode,
      betaPhaseBanner)

  def notFoundTemplateWithProcessCode(processCode: Option[String], betaPhaseBanner: Boolean)(implicit request: Request[_]): Html =
    standardErrorTemplate(
      Messages("global.error.pageNotFound404.title"),
      Messages("global.error.pageNotFound404.heading"),
      Messages("global.error.pageNotFound404.message"),
      processCode,
      betaPhaseBanner)

  def internalServerErrorTemplateWithProcessCode(processCode: Option[String], betaPhaseBanner: Boolean)(implicit request: Request[_]): Html =
    standardErrorTemplate(
      Messages("global.error.InternalServerError500.title"),
      Messages("global.error.InternalServerError500.heading"),
      Messages("global.error.InternalServerError500.message"),
      processCode,
      betaPhaseBanner)
}
