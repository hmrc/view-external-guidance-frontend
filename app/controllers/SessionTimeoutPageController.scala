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

import java.time.Instant

import play.twirl.api.Html
import config.{AppConfig, ErrorHandler}
import javax.inject.{Inject, Singleton}
import play.api.Logger
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc._
import services.GuidanceService
import uk.gov.hmrc.http.SessionKeys._
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import views.html.{user_deleted_session, system_timedout_session}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import core.models.errors.NotFoundError

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

    def sessionTimeout(processCode: String): Action[AnyContent] = Action.async { implicit request =>
      implicit val messages: Messages = mcc.messagesApi.preferred(request)
      hc.sessionId match {
        case Some(id) if !hasSessionExpired(request.session) =>
          service.deleteSession(processCode, id.value).map{
            case Right(()) =>
              logger.warn(s"User deleted session for $id with $processCode")
              Ok(userDeletedSessionView(messages("session.timeout.header.title"), processCode))
            case Left(NotFoundError) =>
              logger.warn(s"User deleted session for $id with $processCode")
              Ok(systemTimedoutSessionView(messages("session.timeout.header.title"), processCode))
            case Left(err) =>
              logger.error(s"Attempt to delete user session for $id with $processCode, returned error $err")
              InternalServerError(errorHandler.internalServerErrorTemplateWithProcessCode(Some(processCode), false))
          }
        case Some(id) =>
          service.deleteSession(processCode, id.value).map{
            case Right(()) | Left(NotFoundError) =>
              logger.warn(s"Session ($id) has expired")
              Ok(systemTimedoutSessionView(messages("session.timeout.header.title"), processCode))
            case Left(err) =>
              logger.error(s"Attempt to delete user session for $id with $processCode, returned error $err")
              InternalServerError(errorHandler.internalServerErrorTemplateWithProcessCode(Some(processCode), false))
          }
        case None =>
          logger.warn(s"No session id found ")
          Future.successful(Ok(systemTimedoutSessionView(messages("session.timeout.header.title"), processCode)))
      }
    }

  def userDeletedSessionView(processTitle: String, processCode: String)(implicit request: Request[_]): Html =
    user_deleted_session_view(
      processTitle,
      Some(processCode),
      None,
      s"${appConfig.baseUrl}/$processCode")

  def systemTimedoutSessionView(processTitle: String, processCode: String)(implicit request: Request[_]): Html =
    system_timedout_session_view(
      processTitle,
      Some(processCode),
      None,
      s"${appConfig.baseUrl}/$processCode")

  /**
    * If last request update is available check if session has timed out
    *
    * @param session - Browser session
    * @return Returns "true" if time since last update exceeds timeout limit or is very close to the limit
    */
  def hasSessionExpired(session: Session): Boolean = {

    session.get(lastRequestTimestamp).fold(false){tsStr =>

      val now = Instant.now.toEpochMilli
      val ts = tsStr.toLong

      val duration = now - ts
      val timeout = appConfig.timeoutInSeconds * appConfig.toMilliSeconds
      val diff = duration - timeout

      (duration > timeout) || (diff.abs < appConfig.expiryErrorMarginInMilliSeconds)
    }
  }

}
