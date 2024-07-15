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
import controllers.{SessionIdPrefix, validateUrl}
import core.models.RequestOutcome
import core.models.errors._
import play.api.Logger
import play.api.i18n.I18nSupport
import play.api.mvc._
import services.RetrieveAndCacheService
import uk.gov.hmrc.http.{HeaderNames, SessionKeys}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.play.language.LanguageUtils

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StartGuidanceController @Inject() (
    errorHandler: ErrorHandler,
    service: RetrieveAndCacheService,
    sessionIdAction: SessionIdAction,
    mcc: MessagesControllerComponents,
    appConfig: AppConfig,
    languageUtils: LanguageUtils
)(implicit ec: ExecutionContext) extends FrontendController(mcc)
    with I18nSupport {

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

  def published(processCode: String, c: Option[String] = None, lang: Option[String] = None): Action[AnyContent] = Action.async { implicit request =>
    logger.warn(s"ST: Starting publish journey for $processCode, lang = $lang")
    retrieveCacheAndRedirectToView(processCode, service.retrieveAndCachePublished, publishedErrorHandler, c, lang)
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

  private def retrieveCacheAndRedirectToView(id: String,
                                             retrieveAndCache: (String, String) => Future[RequestOutcome[(String,String)]],
                                             errHandler: (Error, String, String) => Future[Result],
                                             c: Option[String] = None,
                                             lang: Option[String] = None)(
                                              implicit request: Request[_]
                                            ): Future[Result] = {
    val (sessionId, egNewSessionId) = existingOrNewSessionId()
    logger.warn(s"Retrieve and cache service for process $id using sessionId = $sessionId, EG = ${egNewSessionId}, request id: ${hc.requestId.map(_.value)}")
    validateUrl(id).fold {
      logger.warn(s"Invalid process code $id, code contains unsupported characters. Returning NotFound")
      errorHandler.notFoundTemplateWithProcessCode(None).map(NotFound(_))
    } { _ => retrieveAndCache(id, sessionId).flatMap {
      case Right((url, processCode)) =>
        val target = controllers.routes.GuidanceController.getPage(processCode, url.drop(1), None, c, lang).url
        logger.warn(s"Redirecting to begin viewing process $id/$processCode at ${target} using sessionId $sessionId, EG_NEW_SESSIONID = $egNewSessionId")
        Future.successful(lang.foldLeft(egNewSessionId.foldLeft(Redirect(target))((redirect, newId) =>
          redirect
            .addingToSession(sessionIdAction.EgNewSessionIdName -> newId, SessionKeys.sessionId -> sessionId)
            .withHeaders(HeaderNames.xSessionId -> sessionId)
        ))((redirect, l) => {
          val langToUse = appConfig.languageMap.getOrElse(l, languageUtils.getCurrentLang)
          redirect.withLang(langToUse)
        }))
      case Left(err) => errHandler(err, id, sessionId)
    }
    }
  }

  private def publishedErrorHandler(error: Error, id: String, sessionId: String)(implicit request: Request[_]): Future[Result] =
    error match {
      case DuplicateKeyError =>
        // Trigger return to the start URL after highly unlikely duplicate key error generated when attempting to create sesssion
        logger.warn(s"Unable to retrieve and cache due to  duplicate key error detected using sessionId $sessionId and id $id, restarting")
        Future.successful(Redirect(s"${appConfig.baseUrl}/$id"))
      case err => defaultErrorHandler(err, id, sessionId)
    }

  private def defaultErrorHandler(error: Error, id: String, sessionId: String)(implicit request: Request[_]): Future[Result] =
    error match {
      case NotFoundError =>
        logger.warn(s"Unable to find process $id and render using sessionId $sessionId")
        errorHandler.notFoundTemplate.map(NotFound(_))
      case err =>
        logger.error(s"Error $err returned from Guidance service when trying to access process $id using sessionId $sessionId")
        errorHandler.internalServerErrorTemplate.map(InternalServerError(_))
    }

  private def newSessionId(uuid: String)(implicit request: Request[_]): (String, Option[String]) = {
    val id: String = s"${SessionIdPrefix}${uuid}"
    logger.warn(s"Creating new session id: $id, request id: ${hc.requestId.map(_.value)}")
    (id, Some(id))
  }

  private def existingOrNewSessionId()(implicit request: Request[_]): (String, Option[String]) =
    hc.sessionId.fold[(String, Option[String])](newSessionId(java.util.UUID.randomUUID.toString))(id => (id.value, None))
}
