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

package controllers.entry

import config.{AppConfig, ErrorHandler}
import controllers.actions.SessionIdAction
import controllers.CacheAndRedirectToView
import play.api.Logger
import play.api.mvc._
import services.RetrieveAndCacheService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.play.language.LanguageUtils

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton
class StartGuidanceController @Inject() (
    val errorHandler: ErrorHandler,
    service: RetrieveAndCacheService,
    val sessionIdAction: SessionIdAction,
    val mcc: MessagesControllerComponents,
    val appConfig: AppConfig,
    val languageUtils: LanguageUtils
)(implicit val ec: ExecutionContext) extends FrontendController(mcc)
    with CacheAndRedirectToView {

  val logger: Logger = Logger(getClass)

  def scratch(uuid: String): Action[AnyContent] = Action.async { implicit request =>
    logger.info(s"Starting scratch journey")
    retrieveCacheAndRedirectToView(uuid, service.retrieveAndCacheScratch, defaultErrorHandler)
  }

  def approval(processId: String): Action[AnyContent] = Action.async { implicit request =>
    logger.warn(s"ST: Starting approval direct view journey")
    retrieveCacheAndRedirectToView(processId, service.retrieveAndCacheApproval, defaultErrorHandler)
  }

  def approvalPage(processId: String, url: String): Action[AnyContent] = Action.async { implicit request =>
    logger.warn(s"ST: Starting approval direct page view journey")
    retrieveCacheAndRedirectToView(processId, service.retrieveAndCacheApprovalByPageUrl(s"/$url"), defaultErrorHandler)
  }

  def approvalWithDebugging(processId: String): Action[AnyContent] = Action.async { implicit request =>
    logger.warn(s"ST: Starting approval direct view journey")
    retrieveCacheAndRedirectToView(processId, service.retrieveAndCacheApprovalDebugging, defaultErrorHandler)
  }

  def publishedWithDebugging(processCode: String,
                             c: Option[String] = None,
                             lang: Option[String] = None): Action[AnyContent] = Action.async { implicit request =>
    logger.warn(s"ST: Starting publish journey for $processCode, lang = $lang")
    retrieveCacheAndRedirectToView(processCode, service.retrieveAndCachePublishedDebugging, publishedErrorHandler, c, lang)
  }
}
