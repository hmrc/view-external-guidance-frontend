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

package controllers

import config.{AppConfig, ErrorHandler}

import javax.inject.{Inject, Singleton}
import play.api.i18n.Messages
import play.api.mvc._
import play.api.data.Form
import services.{ErrorStrategy, GuidanceService, ValueTypeError, ValueTypeGroupError}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import core.models.RequestOutcome
import core.models.errors._
import core.models.ocelot.errors.RuntimeError
import core.models.ocelot.stanzas.DateInput
import core.models.ocelot.{RunMode, Published, SecuredProcess}
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
        Future.successful(Redirect(controllers.routes.GuidanceController.getPage(processCode, url.drop(1)).url))
      case Left(SessionNotFoundError) =>
        logger.warn(s"SessionNotFoundError error on sessionRestart. Redirecting to start of processCode $processCode at ${appConfig.baseUrl}/$processCode")
        Future.successful(redirectToGuidanceStart(processCode))
      case Left(NotFoundError) =>
        logger.warn(s"NotFoundError error on sessionRestart. Redirecting to start of processCode $processCode at ${appConfig.baseUrl}/$processCode")
        Future.successful(redirectToGuidanceStart(processCode))
      case Left(ExpectationFailedError) =>
        logger.warn(s"ExpectationFailed error on sessionRestart. Redirecting to start of processCode $processCode at ${appConfig.baseUrl}/$processCode")
        Future.successful(redirectToGuidanceStart(processCode))
      case Left(err) =>
        logger.error(s"Request for Reset GuidanceSession returned $err, returning InternalServerError")
        Future.successful(InternalServerError(errorHandler.internalServerErrorTemplate))
    }
  }

  def getPage(processCode: String, path: String, p: Option[String] = None, c: Option[String] = None): Action[AnyContent] = sessionIdAction.async { implicit request =>
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    val sId: Option[String] = hc.sessionId.map(_.value)
    val rId: Option[String] = hc.requestId.map(_.value)
    val uri: String = request.target.uriString
    logger.warn(s"GP: sessionId: ${sId}, requestId: ${rId}, URI: ${uri}")

    validateUrl(s"$processCode/$path").fold {
      logger.warn(s"Request for PageContext at /$path returned NotFound, returning NotFound")
      Future.successful(NotFound(errorHandler.notFoundTemplateWithProcessCode(None)))
    } { _ =>
      withExistingSession[PageContext](sId =>service.getPageContext(processCode, s"/$path", p.isDefined, sId)).flatMap {
        case Left(err) => translateGetPageError(err, processCode, path, c)
        case Right(pageCtx) =>
          logger.info(s"Retrieved page: ${pageCtx.page.urlPath}, start: ${pageCtx.processStartUrl}, answer: ${pageCtx.answer}, backLink: ${pageCtx.backLink}")
          pageCtx.page match {
            case page: StandardPage => service.savePageState(pageCtx.sessionId, processCode, pageCtx.labels).flatMap {
              case Right(_) =>
                logger.warn(s"GSP=>V: sessionId: ${sId}, requestId: ${rId}, URI: ${uri}")
                Future.successful(Ok(standardView(page, pageCtx)))
              case Left(err) => translateGetPageError(err, processCode, path, c)
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
      }
    }
  }

  private def translateGetPageError(err: Error, processCode: String, path: String, c: Option[String])(implicit request: Request[_]): Future[Result] = err match {
    case ForbiddenError =>
      redirectToActiveSessionFallbackRestart(processCode, "ForbiddenError")
    case TransactionFaultError =>
      logger.warn(s"Attempting to redirect to current session state after transaction fault")
      redirectToActiveSessionFallbackRestart(processCode, "TransactionFaultError")
    case AuthenticationError =>
      logger.warn(s"Request for PageContext at /$path returned AuthenticationError, redirecting to process passphrase page")
      Future.successful(Redirect(routes.GuidanceController.getPage(processCode, SecuredProcess.SecuredProcessStartUrl, None, c)))
    case SessionNotFoundError =>
      logger.warn(s"Request for page at /$path returned SessionNotFound. Redirect to ${appConfig.baseUrl}/$processCode")
      Future.successful(redirectToGuidanceStart(processCode))
    case NotFoundError =>
      logger.warn(s"Request for PageContext at /$path returned NotFound, returning NotFound")
      Future.successful(NotFound(errorHandler.notFoundTemplateWithProcessCode(Some(processCode))))
    case ExpectationFailedError if c.isDefined =>
      logger.warn(s"ExpectationFailed error on getPage after similar redirect to process start. Log ISE")
      Future.successful(Redirect(s"${appConfig.baseUrl}/$processCode/session-timeout"))
    case ExpectationFailedError =>
      logger.warn(s"ExpectationFailed error on getPage. Redirecting to ${appConfig.baseUrl}/$processCode")
      Future.successful(redirectToGuidanceStartWhenNoSession(processCode))
    case Error(Error.ExecutionError, errs, Some(errorRunMode), stanzaId) =>
      Future.successful(translateExecutionError(err.errors.collect{case e: RuntimeError => e}, processCode, path, errorRunMode, stanzaId))
    case err =>
      logger.error(s"Request for PageContext at /$path returned $err, returning InternalServerError")
      Future.successful(InternalServerError(errorHandler.internalServerErrorTemplate))
  }

  def submitPage(processCode: String, path: String): Action[AnyContent] = Action.async { implicit request =>
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    val sId: Option[String] = hc.sessionId.map(_.value)
    val rId: Option[String] = hc.requestId.map(_.value)
    val uri: String = request.target.uriString

    logger.warn(s"SP: sessionId: ${sId}, requestId: ${rId}, URI: ${uri}")
    withExistingSession[PageEvaluationContext](service.getSubmitEvaluationContext(processCode, s"/$path", _)).flatMap {
      case Left(err) => translateSubmitError(err, processCode, path)
      case Right(ctx) => ctx.dataInput.fold{
          logger.error( s"Unable to locate input stanza for process ${ctx.processCode} on submission")
          Future.successful(BadRequest(errorHandler.badRequestTemplateWithProcessCode(Some(processCode))))
        }{ input =>
          val inputName: String = formInputName(path)
          bindFormData(input, inputName) match {
            case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
              Future.successful(BadRequest(createErrorView(service.getSubmitPageContext(ctx, errorStrategy), inputName, formWithErrors)))
            case Right((form: Form[_], submittedAnswer: SubmittedAnswer)) =>
              (input.validInput(submittedAnswer.text), input) match {
                case (Left(fieldErrors), dateInput: DateInput) =>
                  val missingFieldNames = fieldErrors.map(id => messages(s"${dateInput.FieldMsgBase}.${dateInput.FieldNames(id)}"))
                  val pageCtx = service.getSubmitPageContext(ctx, ValueTypeGroupError(missingFieldNames, fieldErrors.map(dateInput.FieldNames)))
                  // Answer didn't pass DateInput stanza validation
                  Future.successful(BadRequest(createErrorView(pageCtx, inputName, form)))
                case (Left(fieldErrors), _) =>
                  // Answer didn't pass other DataInput stanza validation
                  Future.successful(BadRequest(createErrorView(service.getSubmitPageContext(ctx, ValueTypeError), inputName, form)))
                case (Right(answer), _) =>
                  service.submitPage(ctx, s"/$path", answer, submittedAnswer.text).flatMap {
                    case Right((None, labels)) =>      // No valid next page id indicates page should be re-displayed
                      logger.info(s"Post submit page evaluation indicates guidance detected input error")
                      Future.successful(BadRequest(createErrorView(service.getSubmitPageContext(ctx.copy(labels = labels)), inputName, form)))
                    case Right((Some(stanzaId), _)) => // Some(stanzaId) here indicates a redirect to the page with id "stanzaId"
                      val url = ctx.pageMapById(stanzaId).url
                      val pageUrl = url.drop(appConfig.baseUrl.length + processCode.length + 2)
                      logger.info(s"SF=>V: sessionId: ${sId}, requestId: ${rId}, URI: ${uri}, page next: $stanzaId => $url")
                      Future.successful(Redirect(routes.GuidanceController.getPage(processCode, pageUrl, previousPageQueryString(url, ctx.backLink))))
                    case Left(err) => translateSubmitError(err, processCode, path)
                  }
              }
          }
        }
    }
  }

  private def translateSubmitError(err: Error, processCode: String, path: String)(implicit request: Request[_]): Future[Result] = err match {
    case IllegalPageSubmissionError =>
      redirectToActiveSessionFallbackRestart(processCode, "IllegalPageSubmissionError")
    case TransactionFaultError =>
      redirectToActiveSessionFallbackRestart(processCode, "TransactionFaultError")
    case AuthenticationError =>
      Future.successful(Redirect(routes.GuidanceController.getPage(processCode, SecuredProcess.SecuredProcessStartUrl)))
    case NotFoundError =>
      logger.warn(s"Request for PageContext at /$path returned NotFound during form submission, returning NotFound")
      Future.successful(NotFound(errorHandler.notFoundTemplateWithProcessCode(Some(processCode))))
    case BadRequestError =>
      logger.warn(s"Request for PageContext at /$path returned BadRequest during form submission, returning BadRequest")
      Future.successful(BadRequest(errorHandler.badRequestTemplateWithProcessCode(Some(processCode))))
    case ExpectationFailedError =>
      logger.warn(s"ExpectationFailed error on submitPage. Redirect to ${appConfig.baseUrl}/$processCode")
      Future.successful(redirectToGuidanceStart(processCode))
    case SessionNotFoundError =>
      logger.warn(s"Request for page at /$path returned SessionNotFound. Redirect to ${appConfig.baseUrl}/$processCode")
      Future.successful(redirectToGuidanceStart(processCode))
    case Error(Error.ExecutionError, errs, Some(errorRunMode), stanzaId) =>
      Future.successful(translateExecutionError(err.errors.collect{case e: RuntimeError => e}, processCode, path, errorRunMode, stanzaId))
    case err =>
      logger.error(s"Request for PageContext at /$path returned $err during form submission, returning InternalServerError")
      Future.successful(InternalServerError(errorHandler.internalServerErrorTemplate))
  }

  private def translateExecutionError(errors: List[RuntimeError], processCode: String, path: String, runMode: RunMode, stanzaId: Option[String])
                                           (implicit request: Request[_]): Result = {
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    val errorMsgs = errors.map(err => fromRuntimeError(err, stanzaId.getOrElse("UNKNOWN")))
    runMode match {
      case Published =>
        errorMsgs.foreach{err => logger.error(s"RuntimeError: $err within page /$path of processCode ${processCode}")}
        InternalServerError(errorHandler.internalServerErrorTemplate)
      case _ =>
        errorMsgs.foreach{err => logger.warn(s"RuntimeError: $err within page /$path of processCode ${processCode}")}
        InternalServerError(errorHandler.runtimeErrorHandler(processCode, errorMsgs, errorSolutions(errors, stanzaId.getOrElse("UNKNOWN")), stanzaId))
    }
  }

  private def redirectToActiveSessionFallbackRestart(processCode: String, cause: String)(implicit request: Request[_]): Future[Result] =
    withExistingSession[GuidanceSession](service.getCurrentGuidanceSession(processCode)).map{
      case Right(session) => session.currentPageUrl.fold[Result]({
          logger.warn(s"$cause, no current url found, redirecting to start of $processCode process")
          redirectToGuidanceStart(session.process.meta.processCode)
        })(currentUrl => {
          val currentPage: String = s"${appConfig.baseUrl}/${session.process.meta.processCode}$currentUrl"
          logger.warn(s"Recovering after $cause, Redirecting to current page $currentPage")
          Redirect(currentPage)
        })
      case Left(err) =>
        logger.warn(s"Failed ($err) to retrieve current session, redirecting after $cause to beginning of process")
        redirectToGuidanceStart(processCode)
    }

  private def redirectToGuidanceStart(processCode: String): Result = Redirect(s"${appConfig.baseUrl}/$processCode")
  private def redirectToGuidanceStartWhenNoSession(processCode: String): Result = Redirect(s"${appConfig.baseUrl}/$processCode?$RedirectWhenNoSessionUrlParam")

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
