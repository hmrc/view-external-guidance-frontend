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

package controllers

import config.{AppConfig, ErrorHandler}
import javax.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc._
import services.GuidanceService
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import models.errors._
import models.RequestOutcome
import models.ui.{PageContext, StandardPage, QuestionPage, FormData}
import forms.NextPageFormProvider
import views.html.{standard_page, question_page}
import play.api.Logger
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import uk.gov.hmrc.http.SessionKeys

@Singleton
class GuidanceController @Inject() (
    appConfig: AppConfig,
    errorHandler: ErrorHandler,
    standardView: standard_page,
    questionView: question_page,
    formProvider: NextPageFormProvider,
    service: GuidanceService,
    mcc: MessagesControllerComponents
) extends FrontendController(mcc)
    with I18nSupport {

  val logger = Logger(getClass)

  def getPage(path: String): Action[AnyContent] = Action.async { implicit request =>
    withExistingSession[PageContext](service.getPageContext(s"/$path", _)).map {
      case Right(pageContext) =>
        logger.info(s"Retrieved page at ${pageContext.page.urlPath}, start at ${pageContext.processStartUrl}, answer = ${pageContext.answer}")
        pageContext.page match {
          case page: StandardPage => Ok(standardView(page, pageContext.processStartUrl))
          case page: QuestionPage =>
            val form = pageContext.answer.fold(formProvider(questionName(path))) { answer =>
              formProvider(questionName(path)).bind(Map(questionName(path) -> answer))
            }
            Ok(questionView(page, pageContext.processStartUrl, questionName(path), form))
        }
      case Left(NotFoundError) =>
        logger.warn(s"Request for PageContext at /$path returned NotFound, returning NotFound")
        NotFound(errorHandler.notFoundTemplate)
      case Left(BadRequestError) =>
        logger.warn(s"Request for PageContext at /$path returned BadRequest")
        BadRequest(errorHandler.badRequestTemplate)
      case Left(err) =>
        logger.error(s"Request for PageContext at /$path returned $err, returning InternalServerError")
        InternalServerError(errorHandler.internalServerErrorTemplate)
    }
  }

  def submitPage(path: String): Action[AnyContent] = Action.async { implicit request =>
    formProvider(questionName(path)).bindFromRequest.fold(
      formWithErrors => {
        val formData = FormData(path, formWithErrors.data, formWithErrors.errors)
        withExistingSession[PageContext](service.getPageContext(s"/$path", _, Some(formData))).map {
          case Right(pageContext) =>
            pageContext.page match {
              case page: QuestionPage => BadRequest(questionView(page, pageContext.processStartUrl, questionName(path), formWithErrors))
              case _ => BadRequest(errorHandler.badRequestTemplate)
            }
          case Left(NotFoundError) =>
            logger.warn(s"Request for PageContext at /$path returned NotFound during form submission, returning NotFound")
            NotFound(errorHandler.notFoundTemplate)
          case Left(BadRequestError) =>
            logger.warn(s"Request for PageContext at /$path returned BadRequest during form submission, returning BadRequest")
            BadRequest(errorHandler.badRequestTemplate)
          case Left(err) =>
            logger.error(s"Request for PageContext at /$path returned $err during form submission, returning InternalServerError")
            InternalServerError(errorHandler.internalServerErrorTemplate)
        }
      },
      nextPageUrl => {
        val redirectLocation  = routes.GuidanceController.getPage(nextPageUrl.url.drop(appConfig.baseUrl.length + 1))
        withExistingSession[Unit](service.saveAnswerToQuestion(_, s"/$path", nextPageUrl.url)).map {
          case Left(err) =>
            logger.error(s"Save Answer on page: $path failed, answser: /${redirectLocation.toString}, error: $err")
            Redirect(redirectLocation)
          case Right(_) =>
            Redirect(redirectLocation)
        }
      }
    )
  }

  def startJourney(processId: String): Action[AnyContent] = Action.async { implicit request =>
    logger.info(s"Starting journey")
    retreiveCacheAndRedirectToView(processId, service.getStartPageUrl)
  }

  def scratch(uuid: String): Action[AnyContent] = Action.async { implicit request =>
    logger.info(s"Starting scratch journey")
    retreiveCacheAndRedirectToView(uuid, service.retrieveAndCacheScratch)
  }

  def published(processId: String): Action[AnyContent] = Action.async { implicit request =>
    logger.info(s"Starting publish journey")
    retreiveCacheAndRedirectToView(processId, service.retrieveAndCachePublished)
  }

  def approval(processId: String): Action[AnyContent] = Action.async { implicit request =>
    logger.info(s"Starting approval direct view journey")
    retreiveCacheAndRedirectToView(processId, service.retrieveAndCacheApproval)
  }

  def approvalPage(processId: String, url: String): Action[AnyContent] = Action.async { implicit request =>
    def retrieveCacheAndRedirect(startUrl: String)(processId: String, repositoryId: String): Future[RequestOutcome[String]] =
      service.retrieveAndCacheApproval(processId, repositoryId).map {
        case Right(_) => Right(startUrl)
        case err @ Left(_) => err
      }

    logger.info(s"Starting approval direct page view journey")
    retreiveCacheAndRedirectToView(processId, retrieveCacheAndRedirect(s"/$url"))
  }

  private def retreiveCacheAndRedirectToView(id: String, retreiveAndCache: (String, String) => Future[RequestOutcome[String]])(
      implicit request: Request[_]
  ): Future[Result] = {
    val sessionId = existingOrNewSessionId()
    logger.info(s"Calling Retreive and cache service for process $id using sessionId = $sessionId")    
    retreiveAndCache(id, sessionId).map {
      case Right(url) =>
        val redirectLocation = routes.GuidanceController.getPage(url.drop(1))
        logger.info(s"Redirecting to begin viewing process $id at ${redirectLocation.toString} using sessionId $sessionId")        
        Redirect(redirectLocation).addingToSession((SessionKeys.sessionId -> sessionId))
      case Left(NotFoundError) =>
        logger.warn(s"Unable to find process $id and render using sessionId $sessionId")
        NotFound(errorHandler.notFoundTemplate)
      case Left(err) =>
        logger.error(s"Error $err returned from Guidance service when trying to access process $id using sessionId $sessionId")
        InternalServerError(errorHandler.internalServerErrorTemplate)
    }
  }

  private def existingOrNewSessionId()(implicit request: Request[_]): String = 
    hc.sessionId.fold(s"session-${java.util.UUID.randomUUID.toString}")(_.value)

  private def withExistingSession[T](block: String => Future[RequestOutcome[T]])(implicit request: Request[_]): Future[RequestOutcome[T]] =
    hc.sessionId.fold {
      logger.error(s"Session Id missing from request when required")
      Future.successful(Left(BadRequestError): RequestOutcome[T])
    } { sessionId =>
      logger.info(s"Found existing sessionId = $sessionId")
      block(sessionId.value)
    }

  private def questionName(path: String): String = path.reverse.takeWhile(_ != '/').reverse
}
