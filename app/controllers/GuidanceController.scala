/*
 * Copyright 2021 HM Revenue & Customs
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
import play.api.i18n.{Lang, Messages}
import play.api.mvc._
import play.api.data.Form
import services.{ErrorStrategy, GuidanceService, ValueTypeError}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import core.models.RequestOutcome
import core.models.errors._
import core.models.ocelot.SecuredProcess
import models.{PageContext, PageEvaluationContext}
import models.ui.{FormPage, StandardPage, SubmittedAnswer}
import views.html.{form_page, standard_page}
import play.api.Logger
import models.GuidanceSession
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.actions.SessionIdAction
import play.twirl.api.Html

import scala.concurrent.Future
import forms.FormsHelper.{bindFormData, populatedForm}

@Singleton
class GuidanceController @Inject() (
    appConfig: AppConfig,
    sessionIdAction: SessionIdAction,
    errorHandler: ErrorHandler,
    standardView: standard_page,
    formView: form_page,
    service: GuidanceService,
    mcc: MessagesControllerComponents
) extends FrontendController(mcc)
  with SessionFrontendController {

  val logger: Logger = Logger(getClass)

  def sessionRestart(processCode: String): Action[AnyContent] = sessionIdAction.async { implicit request =>
    withExistingSession[String](service.sessionRestart(processCode, _)).flatMap {
      case Right(url) =>
        logger.info(s"Redirecting to guidance start url $url after session reset")
        Future.successful(Redirect(controllers.routes.GuidanceController.getPage(processCode, url.drop(1), None).url))
      case Left(SessionNotFoundError) =>
        logger.warn(s"SessionNotFoundError error on sessionRestart. Redirecting to start of processCode $processCode at ${appConfig.baseUrl}/$processCode")
        Future.successful(Redirect(s"${appConfig.baseUrl}/$processCode"))
      case Left(NotFoundError) =>
        logger.warn(s"NotFoundError error on sessionRestart. Redirecting to start of processCode $processCode at ${appConfig.baseUrl}/$processCode")
        Future.successful(Redirect(s"${appConfig.baseUrl}/$processCode"))
      case Left(ExpectationFailedError) =>
        logger.warn(s"ExpectationFailed error on sessionRestart. Redirecting to start of processCode $processCode at ${appConfig.baseUrl}/$processCode")
        Future.successful(Redirect(s"${appConfig.baseUrl}/$processCode"))
      case Left(err) =>
        logger.error(s"Request for Reset GuidanceSession returned $err, returning InternalServerError")
        Future.successful(InternalServerError(errorHandler.internalServerErrorTemplate))
    }
  }

  def getPage(processCode: String, path: String, p: Option[String]): Action[AnyContent] = sessionIdAction.async { implicit request =>
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    implicit val lang: Lang = messages.lang
    val sId: Option[String] = hc.sessionId.map(_.value)
    val rId: Option[String] = hc.requestId.map(_.value)
    val uri: String = request.target.uriString
    logger.warn(s"GP: sessionId: ${sId}, requestId: ${rId}, URI: ${uri}")
    withExistingSession[PageContext](sId =>service.getPageContext(processCode, s"/$path", p.isDefined, sId)).flatMap {
      case Right(pageCtx) =>
        logger.info(s"Retrieved page: ${pageCtx.page.urlPath}, start: ${pageCtx.processStartUrl}, answer: ${pageCtx.answer}, backLink: ${pageCtx.backLink}")
        pageCtx.page match {
          case page: StandardPage => service.savePageState(pageCtx.sessionId, pageCtx.labels).map {
            case Right(_) =>
              logger.warn(s"GSP=>V: sessionId: ${sId}, requestId: ${rId}, URI: ${uri}")
              Ok(standardView(page, pageCtx))
            case Left(_) => InternalServerError(errorHandler.internalServerErrorTemplate)
          }
          case page: FormPage => pageCtx.dataInput match {
            case Some(input) =>
              val inputName: String = formInputName(path)
              logger.warn(s"GFP=>V: sessionId: ${sId}, requestId: ${rId}, URI: ${uri}")
              Future.successful(Ok(formView(page, pageCtx, inputName, populatedForm(input, inputName, pageCtx.answer))))
            case _ =>
              logger.error(s"Unable to locate input stanza for process ${pageCtx.processCode} on page load")
              Future.successful(BadRequest(errorHandler.badRequestTemplateWithProcessCode(Some(processCode))))
          }
        }
      case Left(ForbiddenError) => logAndTranslateGetPageForbiddenError(processCode)
      case Left(err) => Future.successful(logAndTranslateGetPageError(err, processCode, path))
    }
  }

  def submitPage(processCode: String, path: String): Action[AnyContent] = Action.async { implicit request =>
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    implicit val lang: Lang = messages.lang
    val sId: Option[String] = hc.sessionId.map(_.value)
    val rId: Option[String] = hc.requestId.map(_.value)
    val uri: String = request.target.uriString
    logger.info(s"SP: sessionId: ${sId}, requestId: ${rId}, URI: ${uri}")
    withExistingSession[PageEvaluationContext](service.getSubmitEvaluationContext(processCode, s"/$path", _)).flatMap {
      case Right(ctx) => ctx.dataInput.fold{
          logger.error( s"Unable to locate input stanza for process ${ctx.processCode} on submission")
          Future.successful(BadRequest(errorHandler.badRequestTemplateWithProcessCode(Some(processCode))))
        }{ input =>
          val inputName: String = formInputName(path)
          bindFormData(input, inputName) match {
            case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
                Future.successful(BadRequest(createErrorView(service.getPageContext(ctx, errorStrategy), inputName, formWithErrors)))
            case Right((form: Form[_], submittedAnswer: SubmittedAnswer)) =>
              ctx.dataInput.fold[Option[String]](None)(_.validInput(submittedAnswer.text)).fold{
                // Answer didn't pass page DataInput stanza validation
                Future.successful(BadRequest(createErrorView(service.getPageContext(ctx, ValueTypeError), inputName, form)))
              }{ answer =>
                service.submitPage(ctx, s"/$path", answer, submittedAnswer.text).map {
                  case Right((None, labels)) =>      // No valid next page id indicates page should be re-displayed
                    logger.info(s"Post submit page evaluation indicates guidance detected input error")
                    BadRequest(createErrorView(service.getPageContext(ctx.copy(labels = labels)), inputName, form))
                  case Right((Some(stanzaId), _)) => // Some(stanzaId) here indicates a redirect to the page with id "stanzaId"
                    val url = ctx.pageMapById(stanzaId).url
                    val pageUrl = url.drop(appConfig.baseUrl.length + processCode.length + 2)
                    logger.info(s"SP=>V: sessionId: ${sId}, requestId: ${rId}, URI: ${uri}")
                    logger.info(s"Post submit page next: $stanzaId => $url")
                    Redirect(routes.GuidanceController.getPage(processCode, pageUrl, previousPageQueryString(url, ctx.backLink)))
                  case Left(err) => logAndTranslateSubmitError(err, processCode, path)
                }
              }
          }
        }
      case Left(IllegalPageSubmissionError) => logAndTranslateIllegalPageSubmissionError(processCode)
      case Left(err) => Future.successful(logAndTranslateSubmitError(err, processCode, path))
    }
  }

  private def logAndTranslateIllegalPageSubmissionError(processCode: String)(implicit request: Request[_]): Future[Result] =
    withExistingSession[GuidanceSession](service.getCurrentGuidanceSession).map{
      case Right(session) =>
        session.currentPageUrl.fold[Result]({
          logger.warn(s"Illegal page submission, no current url found, redirecting to start of $processCode process")
          Redirect(s"${appConfig.baseUrl}/${session.process.meta.processCode}")
        })(currentUrl => {
          if (processCode != session.process.meta.processCode) {
            logger.warn(s"Illegal page submission, process code doesnt match session, syncing to current session page ${currentUrl}")
          }
          else {
            logger.warn(s"Illegal page submission (possible multi browser tab access to process), syncing to current session page ${currentUrl}")
          }
          Redirect(s"${appConfig.baseUrl}/${session.process.meta.processCode}$currentUrl")
        })
      case Left(err) =>
        logger.warn(s"Failed ($err) to retrieve current session on IllegalPageSubmissionError, returning InternalServerError")
        InternalServerError(errorHandler.internalServerErrorTemplate)
    }

  private def logAndTranslateSubmitError(err: Error, processCode: String, path: String)(implicit request: Request[_]): Result =
    err match {
      case AuthenticationError =>
        Redirect(routes.GuidanceController.getPage(processCode, SecuredProcess.SecuredProcessStartUrl, None))
      case NotFoundError =>
        logger.warn(s"Request for PageContext at /$path returned NotFound during form submission, returning NotFound")
        NotFound(errorHandler.notFoundTemplateWithProcessCode(Some(processCode)))
      case BadRequestError =>
        logger.warn(s"Request for PageContext at /$path returned BadRequest during form submission, returning BadRequest")
        BadRequest(errorHandler.badRequestTemplateWithProcessCode(Some(processCode)))
      case ExpectationFailedError =>
        logger.warn(s"ExpectationFailed error on submitPage. Redirect to ${appConfig.baseUrl}/$processCode")
        Redirect(s"${appConfig.baseUrl}/$processCode")
      case SessionNotFoundError =>
        logger.warn(s"Request for page at /$path returned SessionNotFound. Redirect to ${appConfig.baseUrl}/$processCode")
        Redirect(s"${appConfig.baseUrl}/$processCode")
      case NonTerminatingPageError =>
        logger.error(s"Encountered non terminating page error within submit to page /$path of processCode ${processCode}")
        InternalServerError(errorHandler.internalServerErrorTemplate)
      case err =>
        logger.error(s"Request for PageContext at /$path returned $err during form submission, returning InternalServerError")
        InternalServerError(errorHandler.internalServerErrorTemplate)
    }

  private def logAndTranslateGetPageForbiddenError(processCode: String)(implicit request: Request[_]): Future[Result] =
    withExistingSession[GuidanceSession](service.getCurrentGuidanceSession).map{
      case Right(session) => session.legalPageIds match {
        case Nil =>
          logger.warn(s"Redirection after ForbiddenError to beginning of process as legalPageIds is empty")
          Redirect(s"${appConfig.baseUrl}/$processCode")
        case x :: _ => // Current page always the head of the legalPageIds
          val idToUrlMap: Map[String, String] = session.pageMap.map{case (k, v) => (v.id, k)}
          logger.warn(s"Redirection after ForbiddenError to previous page at ${idToUrlMap(x)}")
          Redirect(s"${appConfig.baseUrl}/${processCode}${idToUrlMap(x)}")
      }
      case Left(err) =>
        logger.warn(s"Failed ($err) to retrieve current session, redirecting after ForbiddenError to beginning of process")
        Redirect(s"${appConfig.baseUrl}/$processCode")
    }

  private def logAndTranslateGetPageError(err: Error, processCode: String, path: String)(implicit request: Request[_]): Result =
    err match {
      case AuthenticationError =>
        logger.warn(s"Request for PageContext at /$path returned AuthenticationError, redirecting to process passphrase page")
        Redirect(routes.GuidanceController.getPage(processCode, SecuredProcess.SecuredProcessStartUrl, None))
      case SessionNotFoundError =>
        logger.warn(s"Request for page at /$path returned SessionNotFound. Redirect to ${appConfig.baseUrl}/$processCode")
        Redirect(s"${appConfig.baseUrl}/$processCode")
      case NotFoundError =>
        logger.warn(s"Request for PageContext at /$path returned NotFound, returning NotFound")
        NotFound(errorHandler.notFoundTemplateWithProcessCode(Some(processCode)))
      case ExpectationFailedError =>
        logger.warn(s"ExpectationFailed error on getPage. Redirecting to ${appConfig.baseUrl}/$processCode")
        Redirect(s"${appConfig.baseUrl}/$processCode")
      case NonTerminatingPageError =>
        logger.error(s"Encountered non terminating page error within page /$path of processCode ${processCode}")
        InternalServerError(errorHandler.internalServerErrorTemplate)
      case err =>
        logger.error(s"Request for PageContext at /$path returned $err, returning InternalServerError")
        InternalServerError(errorHandler.internalServerErrorTemplate)
    }

  private def createErrorView(ctxOutcome: RequestOutcome[PageContext], inputName: String, form: Form[_])
                                  (implicit request: Request[_], messages: Messages): Html =
    ctxOutcome match {
      case Left(err) => errorHandler.internalServerErrorTemplateWithProcessCode(None)
      case Right(ctx) =>
        ctx.page match {
          case page: FormPage => formView(page, ctx, inputName, form)
          case _ => errorHandler.badRequestTemplateWithProcessCode(Some(ctx.processCode))
        }
    }


  private def formInputName(path: String): String = path.reverse.takeWhile(_ != '/').reverse
  private def previousPageQueryString(url: String, backLink: Option[String]): Option[String] = backLink.collect{case bl if bl == url => "1"}
}
