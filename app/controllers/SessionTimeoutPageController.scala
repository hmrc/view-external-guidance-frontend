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

import play.twirl.api.Html
import config.{AppConfig, ErrorHandler}
import javax.inject.{Inject, Singleton}
import core.models.errors.SessionNotFoundError
import play.api.Logger
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc._
import services.GuidanceService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import views.html.{user_deleted_session, system_timedout_session}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Singleton
class SessionTimeoutPageController @Inject()(appConfig: AppConfig,
                                             service: GuidanceService,
                                             errorHandler: ErrorHandler,
                                             mcc: MessagesControllerComponents,
                                             system_timedout_session_view: system_timedout_session,
                                             user_deleted_session_view: user_deleted_session
                                             )
  extends FrontendController(mcc)
  with I18nSupport {

  val logger: Logger = Logger(getClass)

  def endSession(processCode: String): Action[AnyContent] = Action.async { implicit request =>
    logger.warn(s"User initiated end-session for $processCode")
    endSessionResult(processCode, userDeletedSessionView).map{result =>
      if (sessionStillActive(request, appConfig)) result else result.withNewSession
    }
  }

  def sessionTimeout(processCode: String): Action[AnyContent] = Action.async { implicit request =>
    logger.warn(s"Session timed out for $processCode")
    endSessionResult(processCode, systemTimedoutSessionView).map{result =>
      if (sessionStillActive(request, appConfig)) result else result.withNewSession
    }
  }

  def endSessionResult(processCode: String, view:(String, String) => Html)(implicit request: Request[_]): Future[Result] = {
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    hc.sessionId match {
      case Some(id) =>
        service.getCurrentGuidanceSession(processCode)(id.value).flatMap {
          case Right(session) if sessionStillActive(request, appConfig) =>
            Future.successful(Ok(view(session.process.title.value(messages.lang), processCode)))
          case Right(session) =>
            service.deleteSession(processCode, id.value).map(_ => Ok(view(session.process.title.value(messages.lang), processCode)))
          case Left(SessionNotFoundError) =>
            logger.warn(s"Session end ($id) - retrieving session with processCode $processCode returned NotFound")
            Future.successful(Ok(view(messages("session.timeout.header.title"), processCode)))
          case Left(err) =>
            logger.error(s"Error $err occurred retrieving process context for process $processCode when removing session ($id)")
            Future.successful(InternalServerError(errorHandler.internalServerErrorTemplateWithProcessCode(Some(processCode))))
        }
      case _ => Future.successful(Ok(view(messages("session.timeout.header.title"), processCode)))
    }
  }

  def userDeletedSessionView(processTitle: String, processCode: String)(implicit request: Request[_]): Html =
    user_deleted_session_view(processTitle, Some(processCode), s"${appConfig.baseUrl}/$processCode")

  def systemTimedoutSessionView(processTitle: String, processCode: String)(implicit request: Request[_]): Html =
    system_timedout_session_view(processTitle, Some(processCode), s"${appConfig.baseUrl}/$processCode")
}
