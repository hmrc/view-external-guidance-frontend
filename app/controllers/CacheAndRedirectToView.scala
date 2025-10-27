/*
 * Copyright 2025 HM Revenue & Customs
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

import config.{AppConfig, ErrorHandler}
import controllers.actions.SessionIdAction
import core.models.RequestOutcome
import core.models.errors.{DuplicateKeyError, Error, NotFoundError}
import play.api.Logger
import play.api.i18n.I18nSupport
import play.api.mvc.{MessagesControllerComponents, Request, Result}
import uk.gov.hmrc.http.{HeaderNames, SessionKeys}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.play.language.LanguageUtils

import scala.concurrent.{ExecutionContext, Future}

trait CacheAndRedirectToView extends I18nSupport {
  this: FrontendController =>
  val logger: Logger
  val appConfig: AppConfig
  val errorHandler: ErrorHandler
  val languageUtils: LanguageUtils
  val sessionIdAction: SessionIdAction
  val mcc: MessagesControllerComponents
  implicit val ec: ExecutionContext

  protected def retrieveCacheAndRedirectToView(id: String,
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

  protected def publishedErrorHandler(error: Error, id: String, sessionId: String)(implicit request: Request[_]): Future[Result] =
    error match {
      case DuplicateKeyError =>
        // Trigger return to the start URL after highly unlikely duplicate key error generated when attempting to create sesssion
        logger.warn(s"Unable to retrieve and cache due to  duplicate key error detected using sessionId $sessionId and id $id, restarting")
        Future.successful(Redirect(s"${appConfig.baseUrl}/$id"))
      case err => defaultErrorHandler(err, id, sessionId)
    }

  protected def defaultErrorHandler(error: Error, id: String, sessionId: String)(implicit request: Request[_]): Future[Result] =
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
