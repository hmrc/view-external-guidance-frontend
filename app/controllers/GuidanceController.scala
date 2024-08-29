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
import controllers.actions.SessionIdAction
import models.DebuggableRequestOutcome
import core.models.errors._
import core.models.errors.Error
import core.models.ocelot.errors.RuntimeError
import core.models.ocelot.stanzas.DateInput
import core.models.ocelot.{Debugging, Published, RunMode, SecuredProcess}
import forms.FormProviderFactory
import models.ui.{FormPage, StandardPage, SubmittedAnswer}
import models.{GuidanceSession, PageContext, PageEvaluationContext}
import play.api.Logger
import play.api.data.Form
import play.api.i18n.Messages
import play.api.mvc._
import play.twirl.api.Html
import services.{ErrorStrategy, GuidanceService, ValueTypeError, ValueTypeGroupError}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import views.html.{form_page, standard_page}
import models.admin.DebugInformation
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GuidanceController @Inject() (
    appConfig: AppConfig,
    sessionIdAction: SessionIdAction,
    errorHandler: ErrorHandler,
    standardView: standard_page,
    formView: form_page,
    service: GuidanceService,
    mcc: MessagesControllerComponents,
    formProvider: FormProviderFactory
)(implicit ec: ExecutionContext) extends FrontendController(mcc)
  with SessionFrontendController {

  val logger: Logger = Logger(getClass)

  def sessionRestart(processCode: String): Action[AnyContent] = sessionIdAction.async { implicit request =>
    withExistingSession[String](service.sessionRestart(processCode, _)).flatMap {
      case Right(url) =>
        logger.info(s"Redirecting to guidance start url $url after session reset")
        Future.successful(Redirect(controllers.routes.GuidanceController.getPage(processCode, url.drop(1)).url))
      case Left((SessionNotFoundError, _)) =>
        logger.warn(s"SessionNotFoundError error on sessionRestart. Redirecting to start of processCode $processCode at ${appConfig.baseUrl}/$processCode")
        Future.successful(redirectToGuidanceStart(processCode))
      case Left((NotFoundError, _)) =>
        logger.warn(s"NotFoundError error on sessionRestart. Redirecting to start of processCode $processCode at ${appConfig.baseUrl}/$processCode")
        Future.successful(redirectToGuidanceStart(processCode))
      case Left((ExpectationFailedError, _)) =>
        logger.warn(s"ExpectationFailed error on sessionRestart. Redirecting to start of processCode $processCode at ${appConfig.baseUrl}/$processCode")
        Future.successful(redirectToGuidanceStart(processCode))
      case Left((err, _)) =>
        logger.error(s"Request for Reset GuidanceSession returned $err, returning InternalServerError")
        errorHandler.internalServerErrorTemplate.map(InternalServerError(_))
    }
  }

  def getPage(processCode: String,
              path: String,
              p: Option[String] = None,
              c: Option[String] = None,
              lang: Option[String] = None): Action[AnyContent] = sessionIdAction.async { implicit request =>

    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    val sId: Option[String] = hc.sessionId.map(_.value)
    val rId: Option[String] = hc.requestId.map(_.value)
    val uri: String = request.target.uriString
    logger.warn(s"GP: sessionId: ${sId}, requestId: ${rId}, URI: ${uri}, p: $p, c: $c, lang: $lang")

    validateUrl(s"$processCode/$path").fold {
      logger.warn(s"Request for PageContext at /$path returned NotFound, returning NotFound")
      errorHandler.notFoundTemplateWithProcessCode(None).map(NotFound(_))
    } { _ =>
      withExistingSession[PageContext](service.getPageContext(processCode, s"/$path", p.isDefined, _)).flatMap {
        case Left((err, debugInformation)) => translateGetPageError(err, processCode, path, c, lang, sId, debugInformation)
        case Right(pageCtx) =>
          logger.info(s"Retrieved page: ${pageCtx.page.urlPath}, start: ${pageCtx.processStartUrl}, answer: ${pageCtx.answer}, backLink: ${pageCtx.backLink}")
          pageCtx.page match {
            case page: StandardPage => service.savePageState(pageCtx).flatMap {
              case Right(_) =>
                logger.warn(s"GSP=>V: sessionId: ${sId}, requestId: ${rId}, URI: ${uri}")
                Future.successful(Ok(standardView(page, pageCtx)))
              case Left(err) => translateGetPageError(err, processCode, path, c, lang, sId, pageCtx.debugInformation)
            }
            case page: FormPage => pageCtx.dataInput match {
              case Some(input) =>
                val inputName: String = formInputName(path)
                logger.warn(s"GFP=>V: sessionId: ${sId}, requestId: ${rId}, URI: ${uri}")
                Future.successful(Ok(formView(page, pageCtx, inputName, formProvider(input).populated(inputName, pageCtx.answer))))
              case _ =>
                logger.error(s"Unable to locate input stanza for process ${pageCtx.processCode} on page load")
                errorHandler.badRequestTemplateWithProcessCode(Some(processCode)).map(BadRequest(_))
            }
          }
      }
    }
  }

  private def translateGetPageError(err: Error,
                                    processCode: String,
                                    path: String,
                                    c: Option[String],
                                    lang: Option[String],
                                    sessionId: Option[String],
                                    debugInformation: Option[DebugInformation])
                                   (implicit request: Request[_]): Future[Result] = err match {
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
      errorHandler.notFoundTemplateWithProcessCode(Some(processCode)).map(NotFound(_))
    case ExpectationFailedError if c.isDefined =>
      logger.warn(s"ExpectationFailed error on getPage after similar redirect to process start. Log ISE")
      Future.successful(Redirect(s"${appConfig.baseUrl}/$processCode/session-blocked${lang.fold("")(l => s"?lang=$l")}"))
    case ExpectationFailedError =>
      logger.warn(s"ExpectationFailed error on getPage. Redirecting to ${appConfig.baseUrl}/$processCode")
      Future.successful(redirectToGuidanceStartWhenNoSession(processCode, lang))
    case Error(Error.ExecutionError, errs, Some(errorRunMode), stanzaId) =>
      translateExecutionError(err.errors.collect{case e: RuntimeError => e},
                                                processCode, path, errorRunMode, stanzaId, sessionId, debugInformation)
    case err =>
      logger.error(s"Request for PageContext at /$path returned $err, returning InternalServerError")
      errorHandler.internalServerErrorTemplate.map(InternalServerError(_))
  }

  def submitPage(processCode: String, path: String): Action[AnyContent] = Action.async { implicit request =>
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    val sId: Option[String] = hc.sessionId.map(_.value)
    val rId: Option[String] = hc.requestId.map(_.value)
    val uri: String = request.target.uriString

    logger.warn(s"SP: sessionId: ${sId}, requestId: ${rId}, URI: ${uri}")
    withExistingSession[PageEvaluationContext](service.getSubmitEvaluationContext(processCode, s"/$path", _)).flatMap {
      case Left((err, debugInformation)) => translateSubmitError(err, processCode, path, sId, debugInformation)
      case Right(ctx) => ctx.dataInput.fold{
          logger.error( s"Unable to locate input stanza for process ${ctx.processCode} on submission")
          errorHandler.badRequestTemplateWithProcessCode(Some(processCode)).map(BadRequest(_))
        }{ input =>
          val inputName: String = formInputName(path)
          formProvider(input).bind(inputName) match {
            case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
              createErrorView(service.getSubmitPageContext(ctx, errorStrategy), inputName, formWithErrors).map(BadRequest(_))
            case Right((form: Form[_], submittedAnswer: SubmittedAnswer)) =>
              (input.validInput(submittedAnswer.text), input) match {
                case (Left(fieldErrors), dateInput: DateInput) =>
                  val missingFieldNames = fieldErrors.map(id => messages(s"${dateInput.FieldMsgBase}.${dateInput.FieldNames(id)}"))
                  val pageCtx = service.getSubmitPageContext(ctx, ValueTypeGroupError(missingFieldNames, fieldErrors.map(dateInput.FieldNames)))
                  // Answer didn't pass DateInput stanza validation
                  createErrorView(pageCtx, inputName, form).map(BadRequest(_))
                case (Left(fieldErrors), _) =>
                  // Answer didn't pass other DataInput stanza validation
                  createErrorView(service.getSubmitPageContext(ctx, ValueTypeError), inputName, form).map(BadRequest(_))
                case (Right(answer), _) =>
                  service.submitPage(ctx, s"/$path", answer, submittedAnswer.text).flatMap {
                    case Right((None, labels)) =>      // No valid next page id indicates page should be re-displayed
                      logger.info(s"Post submit page evaluation indicates guidance detected input error")
                      createErrorView(service.getSubmitPageContext(ctx.copy(labels = labels)), inputName, form).map(BadRequest(_))
                    case Right((Some(stanzaId), _)) => // Some(stanzaId) here indicates a redirect to the page with id "stanzaId"
                      val url = ctx.pageMapById(stanzaId).url
                      val pageUrl = url.drop(appConfig.baseUrl.length + processCode.length + 2)
                      logger.info(s"SF=>V: sessionId: ${sId}, requestId: ${rId}, URI: ${uri}, page next: $stanzaId => $url")
                      Future.successful(Redirect(routes.GuidanceController.getPage(processCode, pageUrl, previousPageQueryString(url, ctx.backLink))))
                    case Left((err, debugInformation)) => translateSubmitError(err, processCode, path, sId, debugInformation)
                  }
              }
          }
        }
    }
  }

  private def translateSubmitError(err: Error,
                                   processCode: String,
                                   path: String,
                                   sessionId: Option[String],
                                   debugInformation: Option[DebugInformation])(implicit request: Request[_]): Future[Result] = err match {
    case IllegalPageSubmissionError =>
      redirectToActiveSessionFallbackRestart(processCode, "IllegalPageSubmissionError")
    case TransactionFaultError =>
      redirectToActiveSessionFallbackRestart(processCode, "TransactionFaultError")
    case AuthenticationError =>
      Future.successful(Redirect(routes.GuidanceController.getPage(processCode, SecuredProcess.SecuredProcessStartUrl)))
    case NotFoundError =>
      logger.warn(s"Request for PageContext at /$path returned NotFound during form submission, returning NotFound")
      errorHandler.notFoundTemplateWithProcessCode(Some(processCode)).map(NotFound(_))
    case BadRequestError =>
      logger.warn(s"Request for PageContext at /$path returned BadRequest during form submission, returning BadRequest")
      errorHandler.badRequestTemplateWithProcessCode(Some(processCode)).map(BadRequest(_))
    case ExpectationFailedError =>
      logger.warn(s"ExpectationFailed error on submitPage. Redirect to ${appConfig.baseUrl}/$processCode")
      Future.successful(redirectToGuidanceStart(processCode))
    case SessionNotFoundError =>
      logger.warn(s"Request for page at /$path returned SessionNotFound. Redirect to ${appConfig.baseUrl}/$processCode")
      Future.successful(redirectToGuidanceStart(processCode))
    case Error(Error.ExecutionError, errs, Some(errorRunMode), stanzaId) =>
      translateExecutionError(err.errors.collect{case e: RuntimeError => e},
                                                processCode, path, errorRunMode, stanzaId, sessionId, debugInformation)
    case err =>
      logger.error(s"Request for PageContext at /$path returned $err during form submission, returning InternalServerError")
      errorHandler.internalServerErrorTemplate.map(InternalServerError(_))
  }

  private def translateExecutionError(errors: List[RuntimeError],
                                      processCode: String,
                                      path: String,
                                      runMode: RunMode,
                                      stanzaId: Option[String],
                                      sessionId: Option[String],
                                      debugInformation: Option[DebugInformation])
                                           (implicit request: Request[_]): Future[Result] = {
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    val errorMsgs = errors.map(err => fromRuntimeError(err, stanzaId.getOrElse("UNKNOWN")))
    runMode match {
      case Published =>
        errorMsgs.foreach{err => logger.error(s"RuntimeError: $err within page /$path of processCode ${processCode}, sessionId=$sessionId")}
        errorHandler.internalServerErrorTemplate.map(InternalServerError(_))
      case Debugging =>
        errorHandler
          .runtimeErrorHandler(processCode, errorMsgs,
                               errorSolutions(errors, stanzaId.getOrElse("UNKNOWN")), stanzaId, debugInformation)
          .map(InternalServerError(_))
      case _ =>
        errorMsgs.foreach{err => logger.warn(s"RuntimeError: $err within page /$path of processCode ${processCode}, sessionId=$sessionId")}
        errorHandler.runtimeErrorHandler(processCode, errorMsgs, errorSolutions(errors, stanzaId.getOrElse("UNKNOWN")), stanzaId, None).map(InternalServerError(_))
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
  private def redirectToGuidanceStartWhenNoSession(processCode: String, lang: Option[String]): Result =
    Redirect(s"${appConfig.baseUrl}/$processCode?$RedirectWhenNoSessionUrlParam${lang.fold("")(l => s"&lang=$l")}")

  private def createErrorView(ctxOutcome: DebuggableRequestOutcome[PageContext], inputName: String, form: Form[_])
                                  (implicit request: Request[_], messages: Messages): Future[Html] =
    ctxOutcome match {
      case Left(err) => errorHandler.internalServerErrorTemplateWithProcessCode(None)
      case Right(ctx) =>
        ctx.page match {
          case page: FormPage => Future.successful(formView(page, ctx, inputName, form))
          case page => 
            logger.error(s"ERROR: Expected a form page, but standard page created $page")
            errorHandler.badRequestTemplateWithProcessCode(Some(ctx.processCode))
        }
    }

  private def formInputName(path: String): String = path.reverse.takeWhile(_ != '/').reverse
  private def previousPageQueryString(url: String, backLink: Option[String]): Option[String] = backLink.collect{case bl if bl == url => "1"}
}
