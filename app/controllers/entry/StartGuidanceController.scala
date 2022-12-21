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

package controllers.entry

import config.{AppConfig, ErrorHandler}
import javax.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc._
import services.RetrieveAndCacheService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import core.models.errors._
import core.models.RequestOutcome
import play.api.Logger
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.SessionIdPrefix
import controllers.actions.SessionIdAction
import controllers.validateUrl
import uk.gov.hmrc.http.SessionKeys
import uk.gov.hmrc.http.HeaderNames

@Singleton
class StartGuidanceController @Inject() (
    errorHandler: ErrorHandler,
    service: RetrieveAndCacheService,
    sessionIdAction: SessionIdAction,
    mcc: MessagesControllerComponents,
    appConfig: AppConfig
) extends FrontendController(mcc)
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

  def published(processCode: String): Action[AnyContent] = Action.async { implicit request =>
    logger.warn(s"ST: Starting publish journey for $processCode")
    retrieveCacheAndRedirectToView(processCode, service.retrieveAndCachePublished, publishedErrorHandler)
  }

  private def retrieveCacheAndRedirectToView(id: String,
                                             retrieveAndCache: (String, String) => Future[RequestOutcome[(String,String)]],
                                             errHandler: (Error, String, String) => Result)(
      implicit request: Request[_]
  ): Future[Result] = {
    val (sessionId, egNewSessionId) = existingOrNewSessionId()
    logger.warn(s"Calling Retrieve and cache service for process $id using sessionId = $sessionId, EG = ${egNewSessionId}, request id: ${hc.requestId.map(_.value)}")
    validateUrl(id).fold {
      logger.warn(s"Invalid process code $id, code contains unsupported characters. Returning NotFound")
      Future.successful(NotFound(errorHandler.notFoundTemplateWithProcessCode(None)))
    } { _ => retrieveAndCache(id, sessionId).map {
        case Right((url, processCode)) =>
          val target = controllers.routes.GuidanceController.getPage(processCode, url.drop(1), None).url
          logger.warn(s"Redirecting to begin viewing process $id/$processCode at ${target} using sessionId $sessionId, EG_NEW_SESSIONID = $egNewSessionId")
          egNewSessionId.fold(Redirect(target))(newId =>
            Redirect(target)
              .addingToSession(sessionIdAction.EgNewSessionIdName -> newId, SessionKeys.sessionId -> sessionId)
              .withHeaders(HeaderNames.xSessionId -> sessionId)
          )
        case Left(err) => errHandler(err, id, sessionId)
      }
    }
  }

  private def publishedErrorHandler(error: Error, id: String, sessionId: String)(implicit request: Request[_]): Result =
    error match {
      case DuplicateKeyError =>
        // Trigger to return to the start URL after highly unlikely duplicate key error generated when attempting to create sesssion
        logger.warn(s"Unable to retrieve and cache due to  duplicate key error detected using sessionId $sessionId and id $id, restarting")
        Redirect(s"${appConfig.baseUrl}/$id")
      case err => defaultErrorHandler(err, id, sessionId)
    }

  private def defaultErrorHandler(error: Error, id: String, sessionId: String)(implicit request: Request[_]): Result =
    error match {
      case NotFoundError =>
        logger.warn(s"Unable to find process $id and render using sessionId $sessionId")
        NotFound(errorHandler.notFoundTemplate)
      case err =>
        logger.error(s"Error $err returned from Guidance service when trying to access process $id using sessionId $sessionId")
        InternalServerError(errorHandler.internalServerErrorTemplate)
    }

  private def newSessionId(uuid: String)(implicit request: Request[_]): (String, Option[String]) = {
    val id: String = s"${SessionIdPrefix}${uuid}"
    logger.warn(s"Creating new session id: $id, request id: ${hc.requestId.map(_.value)}")
    (id, Some(id))
  }

  private def existingOrNewSessionId()(implicit request: Request[_]): (String, Option[String]) =
    hc.sessionId.fold[(String, Option[String])](newSessionId(java.util.UUID.randomUUID.toString))(id => (id.value, None))
}
