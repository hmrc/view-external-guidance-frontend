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
import core.models.errors._
import core.models.ocelot.SecuredProcess
import models.{PageContext, PageEvaluationContext, POST}
import models.ui.{FormPage, StandardPage, SubmittedAnswer}
import views.html.{form_page, standard_page}
import play.api.Logger

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
      case Left(ExpectationFailedError) =>
        logger.error(s"Redirecting to start of processCode $processCode at ${appConfig.baseUrl}/$processCode")
        Future.successful(Redirect(s"${appConfig.baseUrl}/$processCode"))
      case Left(err) =>
        logger.error(s"Request for Reset ProcessContext returned $err, returning InternalServerError")
        Future.successful(InternalServerError(errorHandler.internalServerErrorTemplate))
    }
  }

  def getPage(processCode: String, path: String, p: Option[String]): Action[AnyContent] = sessionIdAction.async { implicit request =>
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    implicit val lang: Lang = messages.lang
    withExistingSession[PageContext](service.getPageContext(processCode, s"/$path", p.isDefined, _)).flatMap {
      case Right(pageCtx) =>
        logger.info(s"Retrieved page at ${pageCtx.page.urlPath}, start at ${pageCtx.processStartUrl}," +
                    s" answer = ${pageCtx.answer}, backLink = ${pageCtx.backLink}")
        pageCtx.page match {
          case page: StandardPage => service.savePageState(pageCtx).map {
              case Right(_) => Ok(standardView(page, pageCtx))
              case Left(_) => InternalServerError(errorHandler.internalServerErrorTemplate)
            }
          case page: FormPage => pageCtx.dataInput match {
            case Some(input) =>
              val inputName: String = formInputName(path)
              Future.successful(Ok(formView(page, pageCtx, inputName, populatedForm(input, inputName, pageCtx.answer))))
            case _ =>
              logger.error(s"Unable to locate input stanza for process ${pageCtx.processCode} on page load")
              Future.successful(BadRequest(errorHandler.badRequestTemplateWithProcessCode(Some(processCode))))
          }
        }
      case Left(AuthenticationError) =>
        logger.warn(s"Request for PageContext at /$path returned AuthenticationError, redirecting to process passphrase page")
        Future.successful(Redirect(routes.GuidanceController.getPage(processCode, SecuredProcess.SecuredProcessStartUrl, None)))
      case Left(NotFoundError) =>
        logger.warn(s"Request for PageContext at /$path returned NotFound, returning NotFound")
        Future.successful(NotFound(errorHandler.notFoundTemplateWithProcessCode(Some(processCode))))
      case Left(BadRequestError) =>
        logger.warn(s"Request for PageContext at /$path returned BadRequest")
        Future.successful(BadRequest(errorHandler.badRequestTemplateWithProcessCode(Some(processCode))))
      case Left(ExpectationFailedError) =>
        logger.error(s"Redirecting to start of processCode $processCode at ${appConfig.baseUrl}/$processCode")
        Future.successful(Redirect(s"${appConfig.baseUrl}/$processCode"))
      case Left(ForbiddenError) =>
        logger.error(s"Redirecting to start of processCode $processCode at ${appConfig.baseUrl}/$processCode")
        Future.successful(Redirect(s"${appConfig.baseUrl}/$processCode"))
      case Left(err) =>
        logger.error(s"Request for PageContext at /$path returned $err, returning InternalServerError")
        Future.successful(InternalServerError(errorHandler.internalServerErrorTemplate))
    }
  }

  def submitPage(processCode: String, path: String): Action[AnyContent] = Action.async { implicit request =>
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    implicit val lang: Lang = messages.lang
    withExistingSession[PageEvaluationContext](service.getPageEvaluationContext(processCode, s"/$path", previousPageByLink = false, _, POST)).flatMap {
      case Right(ctx) => ctx.dataInput.fold{
          logger.error( s"Unable to locate input stanza for process ${ctx.processCode} on submission")
          Future.successful(BadRequest(errorHandler.badRequestTemplateWithProcessCode(Some(processCode))))
        }{ input =>
          val inputName: String = formInputName(path)
          bindFormData(input, inputName) match {
            case Left((formWithErrors: Form[_], errorStrategy: ErrorStrategy)) =>
                Future.successful(BadRequest(createInputView(service.getPageContext(ctx, errorStrategy), inputName, formWithErrors)))
            case Right((form: Form[_], submittedAnswer: SubmittedAnswer)) =>
              service.validateUserResponse(ctx, submittedAnswer.text).fold{
                // Answer didn't pass page DataInput stanza validation
                Future.successful(BadRequest(createInputView(service.getPageContext(ctx, ValueTypeError), inputName, form)))
              }{ answer =>
                service.submitPage(ctx, s"/$path", answer, submittedAnswer.text).map {
                  case Right((None, labels)) =>      // No valid next page id indicates page should be re-displayed
                    logger.info(s"Post submit page evaluation indicates guidance detected input error")
                    BadRequest(createInputView(service.getPageContext(ctx.copy(labels = labels)), inputName, form))
                  case Right((Some(stanzaId), _)) => // Some(stanzaId) here indicates a redirect to the page with id "stanzaId"
                    val url = ctx.pageMapById(stanzaId).url
                    logger.info(s"Post submit page evaluation indicates next page at stanzaId: $stanzaId => $url")
                    Redirect(routes.GuidanceController.getPage(
                      processCode,
                      url.drop(appConfig.baseUrl.length + processCode.length + 2),
                      previousPageQueryString(url, ctx.backLink)))
                  case Left(err) =>
                    logger.error(s"Page submission failed: $err")
                    InternalServerError(errorHandler.internalServerErrorTemplate)
                }
            }
          }
        }
      case Left(AuthenticationError) =>
        Future.successful(Redirect(routes.GuidanceController.getPage(processCode, SecuredProcess.SecuredProcessStartUrl, None)))
      case Left(NotFoundError) =>
        logger.warn(s"Request for PageContext at /$path returned NotFound during form submission, returning NotFound")
        Future.successful(NotFound(errorHandler.notFoundTemplateWithProcessCode(Some(processCode))))
      case Left(BadRequestError) =>
        logger.warn(s"Request for PageContext at /$path returned BadRequest during form submission, returning BadRequest")
        Future.successful(BadRequest(errorHandler.badRequestTemplateWithProcessCode(Some(processCode))))
      case Left(ExpectationFailedError) =>
        logger.error(s"Redirecting to start of processCode $processCode at ${appConfig.baseUrl}/$processCode")
        Future.successful(Redirect(s"${appConfig.baseUrl}/$processCode"))
      case Left(err) =>
        logger.error(s"Request for PageContext at /$path returned $err during form submission, returning InternalServerError")
        Future.successful(InternalServerError(errorHandler.internalServerErrorTemplate))
    }
  }

  private def createInputView(ctx: PageContext, inputName: String, form: Form[_])(implicit request: Request[_], messages: Messages): Html =
    ctx.page match {
      case page: FormPage => formView(page, ctx, inputName, form)
      case _ => errorHandler.badRequestTemplateWithProcessCode(Some(ctx.processCode))
    }

  private def formInputName(path: String): String = path.reverse.takeWhile(_ != '/').reverse
  private def previousPageQueryString(url: String, backLink: Option[String]): Option[String] = backLink.collect{case bl if bl == url => "1"}
}
