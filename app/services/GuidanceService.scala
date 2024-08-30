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

package services

import core.services._
import config.AppConfig
import javax.inject.{Inject, Singleton}
import models.{DebuggableRequestOutcome, GuidanceSession, PageDesc, PageContext, PageEvaluationContext}
import play.api.Logger
import uk.gov.hmrc.http.HeaderCarrier
import core.models.errors._
import core.models.RequestOutcome
import play.api.i18n.{MessagesApi, Messages}
import scala.concurrent.{ExecutionContext, Future}
import repositories.SessionFSM
import core.models.ocelot.{LabelCache, Labels, Process, Label, flowPath, Debugging, LabelOperation}
import core.models.ocelot.SecuredProcess
import core.services.EncrypterService
import models.admin.DebugInformation

@Singleton
class GuidanceService @Inject() (
    appConfig: AppConfig,
    sessionService: SessionService,
    debugService: DebugService,
    pageBuilder: PageBuilder,
    pageRenderer: PageRenderer,
    spb: SecuredProcessBuilder,
    uiBuilder: UIBuilder,
    transition: SessionFSM,
    messagesApi: MessagesApi,
    encrypter: EncrypterService
) {
  val logger: Logger = Logger(getClass)

  def sessionRestart(processCode: String, sessionId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): 
                                                            Future[DebuggableRequestOutcome[String]] =
    sessionService.reset(sessionId, processCode, hc.requestId.map(_.value)).map{
      case Right(session) =>
        session.pageMap.collectFirst{case (k,v) if v.id == session.process.startPageId => k}
          .fold[DebuggableRequestOutcome[String]]{
            logger.error(s"Process start pageId (${session.process.startPageId}) missing from retrieved session map" )
            Left((InternalServerError, None))
          }(Right(_))
      case Left(err) => Left((err, None))
    }

  def deleteSession(processCode: String, sessionId: String): Future[RequestOutcome[Unit]] = sessionService.delete(sessionId, processCode)

  def getSubmitPageContext(pec: PageEvaluationContext, errStrategy: ErrorStrategy = NoError)
                          (implicit messages: Messages): DebuggableRequestOutcome[PageContext] =
    pageRenderer.renderPage(pec.page, pec.labels) match {
      case Left((err, updatedLabels)) =>
        logger.error(s"Execution error $err on page ${pec.page.id} of processCode ${pec.processCode}")
        Left((err, pec.debugInformation.map(_.copy(postRenderLabels = Some(updatedLabels)))))
      case Right((visualStanzas, labels, dataInput)) =>
        uiBuilder.buildPage(pec.page.url, visualStanzas, errStrategy)(UIContext(labels, pec.pageMapById, messages)).fold(
          err => Left((err, pec.debugInformation.map(_.copy(postRenderLabels = Some(labels))))),
          uiPage => Right(PageContext(pec.copy(dataInput = dataInput), uiPage, labels))
        )
    }

  def getPageContext(processCode: String, url: String, previousPageByLink: Boolean, sessionId: String)
                    (implicit hc: HeaderCarrier, context: ExecutionContext, messages: Messages): Future[DebuggableRequestOutcome[PageContext]] =
    getPageEvaluationContext(processCode, url, previousPageByLink, sessionId).map{
      case Right(ctx) =>
        uiBuilder.buildPage(ctx.page.url, ctx.visualStanzas.toList)(UIContext(ctx.labels, ctx.pageMapById, messages)).fold(
          err => Left((err, ctx.debugInformation)),
          page => Right(PageContext(ctx, page))
        )
      case Left(err) => Left(err)
    }

  def getCurrentGuidanceSession(processCode: String)(sessionId: String)(implicit context: ExecutionContext): 
                                                                       Future[DebuggableRequestOutcome[GuidanceSession]] =
    sessionService.getNoUpdate(sessionId, processCode) map {
      case Left(err) => Left((err, None))
      case Right(result) => Right(result)
    }

  def getPageEvaluationContext(processCode: String, url: String, previousPageByLink: Boolean, sessionId: String)
                              (implicit hc: HeaderCarrier, context: ExecutionContext, messages: Messages): 
                              Future[DebuggableRequestOutcome[PageEvaluationContext]] = {
    val pageUrl: Option[String] = if (isAuthenticationUrl(url)) None else Some(s"$processCode$url")
    getPageGuidanceSession(sessionId, processCode, pageUrl, previousPageByLink).map{
      case Left(err) => Left(err)
      case Right(session) if pageUrl.isDefined && !session.secure => Left((AuthenticationError, None))
      case Right(session) => buildEvaluationContext(sessionId, processCode, url, session)
    }
  }

  def getPageGuidanceSession(key: String, processCode: String, pageUrl: Option[String], previousPageByLink: Boolean)
                            (implicit hc: HeaderCarrier, context: ExecutionContext): Future[DebuggableRequestOutcome[GuidanceSession]] = 
    sessionService.get(key, processCode, hc.requestId.map(_.value)).flatMap{
      case Left(err) => Future.successful(Left((err, None)))
      case Right(sp) =>
      pageUrl.fold[Future[DebuggableRequestOutcome[GuidanceSession]]](Future.successful(Right(sp))){url =>
        sp.pageMap.get(url.drop(sp.process.meta.processCode.length)).fold[Future[DebuggableRequestOutcome[GuidanceSession]]]{
          logger.warn(s"Attempt to move to unknown page $url in process ${sp.process.meta.id}, page count = ${sp.pageMap.size}")
          Future.successful(Left((NotFoundError, None)))
        }{pageNext =>
          logger.debug(s"Incoming Page: ${pageNext.id}, $url, current legalPageIds: ${sp.legalPageIds}")
          val requestId: Option[String] = hc.requestId.map(_.value)
          if (sp.legalPageIds.isEmpty || sp.legalPageIds.contains(pageNext.id)){ // Wild card or fixed list of valid page ids
            val firstPageUrl: String = s"${sp.process.meta.processCode}${sp.process.startUrl.getOrElse("")}"
            val (backLink, updatedPageHistory, flowStackUpdate, labelUpdates) = transition(url, sp.pageHistory, sp.flowStack, previousPageByLink, firstPageUrl)
            val labels: Map[String, Label] = sp.labels ++ labelUpdates.map(l => l.name -> l).toMap
            val legalPageIds = (pageNext.id :: Process.StartStanzaId :: pageNext.linked ++
                                backLink.fold(List.empty[String])(bl => List(sp.pageMap(bl.drop(sp.process.meta.processCode.length)).id))).distinct

            sessionService.updateForNewPage(key, processCode, sp.pageMap, updatedPageHistory, flowStackUpdate, labelUpdates, legalPageIds, requestId).map {
              case Left(NotFoundError) =>
                logger.warn(s"TRANSACTION FAULT(Recoverable): saveUpdates _id=$key, requestId: $requestId")
                Left((TransactionFaultError, None))
              case Left(err) =>
                logger.error(s"Unable to update session data, error = $err")
                Left((err, None))
              case _ => Right(sp.copy(labels = labels, flowStack = flowStackUpdate.getOrElse(sp.flowStack), backLink = backLink))
            }
          } else {
            logger.warn(s"Attempt to move to illegal page $url, LEGALPIDS ${sp.legalPageIds}")
            Future.successful(Left((ForbiddenError, None)))
          }
        }
      }
    }

  def getSubmitEvaluationContext(processCode: String, url: String, sessionId: String)
                                (implicit hc: HeaderCarrier, context: ExecutionContext, messages: Messages): 
                                Future[DebuggableRequestOutcome[PageEvaluationContext]] = {
    val pageUrl: Option[String] = if (isAuthenticationUrl(url)) None else Some(s"$processCode$url")
    getSubmitGuidanceSession(sessionId, processCode, pageUrl).map{
      case Left(err) => Left((err, None))
      case Right(session) if pageUrl.isDefined && !session.secure => Left((AuthenticationError, None))
      case Right(session) => buildEvaluationContext(sessionId, processCode, url, session)
    }
  }

  def getSubmitGuidanceSession(key: String, processCode: String, pageUrl: Option[String])
                              (implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[GuidanceSession]] =
    sessionService.get(key, processCode, hc.requestId.map(_.value)).map{
      case Left(err) => Left(err)
      // If incoming url equals the most recent page history url proceed, otherwise, the POST is out of sequence (IllegalPageSubmissionError)
      case Right(sp) if pageUrl.fold(true)(url => sp.pageHistory.reverse.headOption.fold(false)(ph => url.equals(ph.url))) =>
        val backlink = pageUrl.fold[Option[String]](None){_ =>
          sp.pageHistory.reverse match {
            case _ :: y :: _ => Some(y.url)
            case _ => None
          }
        }
        Right(sp.copy(backLink = backlink))
      case Right(sp) => Left(IllegalPageSubmissionError)
    }

  def submitPage(ctx: PageEvaluationContext, url: String, validatedAnswer: String, submittedAnswer: String)
                (implicit hc: HeaderCarrier, context: ExecutionContext, messages: Messages): Future[DebuggableRequestOutcome[(Option[String], Labels)]] =
    pageRenderer.renderPagePostSubmit(ctx.page, ctx.labels, validatedAnswer) match {
      case Left((err, updatedLabels)) => Future.successful(Left((err, ctx.debugInformation.map(_.copy(postRenderLabels = Some(updatedLabels))))))
      case Right((optionalNext, labels)) =>
        val requestId: Option[String] = hc.requestId.map(_.value)
        optionalNext.fold[Future[DebuggableRequestOutcome[(Option[String], Labels)]]](Future.successful(Right((None, labels)))){next =>
          logger.debug(s"Next page found at stanzaId: $next")
          sessionService.updateAfterFormSubmission(ctx.sessionId, 
                                                   ctx.processCode, 
                                                   answerStorageId(ctx.labels, url), 
                                                   submittedAnswer, 
                                                   labels, 
                                                   List(next),
                                                   optionalRevertOperations(url, false, labels),
                                                   requestId).map{
            case Left(NotFoundError) =>
              logger.warn(s"TRANSACTION FAULT(Recoverable): saveFormPageState _id=${ctx.sessionId}, url:$url, answer:$validatedAnswer, requestId:${requestId}")
              Left((TransactionFaultError, ctx.debugInformation))
            case Left(err) =>
              logger.error(s"Failed to save updated labels, error = $err")
              Left((InternalServerError, ctx.debugInformation))
            case Right(_) => Right((Some(next), labels))
          }
        }
    }

  def savePageState(ctx: PageContext)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[Unit]] =
    sessionService.updateAfterStandardPage(
      ctx.sessionId,
      ctx.processCode,
      ctx.labels,
      optionalRevertOperations(ctx.page.urlPath, false, ctx.labels),
      hc.requestId.map(_.value)).map{
        case Left(NotFoundError) =>
          logger.warn(s"TRANSACTION FAULT(Recoverable): saveLabels _id=${ctx.sessionId}, requestId: ${hc.requestId.map(_.value)}")
          Left(TransactionFaultError)
        case result => result
      }

  private def messagesFn(k: String, args: Seq[Any])(implicit messages: Messages): String = messages(k, args: _*)

  private def buildEvaluationContext(sessionId: String, processCode: String, url: String, gs: GuidanceSession)
                                    (implicit messages: Messages): DebuggableRequestOutcome[PageEvaluationContext] =
    gs.pageMap.get(url).fold[DebuggableRequestOutcome[PageEvaluationContext]]{
      logger.error(s"Unable to find url $url within cached process ${gs.process.meta.id} using sessionId $sessionId")
      Left((NotFoundError, None))
    }{ pageNext =>
      pageBuilder.buildPage(pageNext.id, gs.process).fold(
        err => {
          logger.error(s"PageBuilder error $err on process ${gs.process.meta.id} with sessionId $sessionId")
          Left((InvalidProcessError, None))
        },
        page => {
          val pageMapById: Map[String, PageDesc] =
            gs.pageMap.map{case (k, pn) => (pn.id, PageDesc(pn, s"${appConfig.baseUrl}/$processCode${k}"))}
          val labels: Labels =
            LabelCache(gs.labels, Map(), gs.flowStack, gs.continuationPool, gs.process.timescales, messagesFn, gs.runMode, encrypter)
          pageRenderer.renderPage(page, labels) match {
            case Left((err, updatedLabels)) =>
              Left((err, Option.when(labels.runMode.equals(Debugging))
                                    (DebugInformation(Some(debugService.mapPage(page, gs.pageMap)), Some(labels), Some(updatedLabels)))))
            case Right((visualStanzas, updatedLabels, dataInput)) =>
              val processTitle: models.ui.Text = TextBuilder.fromPhrase(gs.process.title)(UIContext(updatedLabels, pageMapById, messages))
              Right(
                PageEvaluationContext(
                  page, visualStanzas, dataInput, sessionId, pageMapById, 
                  gs.process.startUrl.map(_ => s"${appConfig.baseUrl}/${processCode}/session-restart"),
                  processTitle, gs.process.meta.id, processCode, updatedLabels, 
                  gs.backLink.map(bl => s"${appConfig.baseUrl}/$bl"), gs.answers.get(answerStorageId(updatedLabels, url)),
                  gs.process.betaPhaseBanner,
                  Option.when(labels.runMode.equals(Debugging))
                             (DebugInformation(Some(debugService.mapPage(page, gs.pageMap)), Some(labels), Some(updatedLabels)))
                )
              )
          }
        })
    }

  private def answerStorageId(labels: Labels, url: String): String = flowPath(labels.flowStack).fold(url)(fp => s"${fp.replaceAll("[ .]", "_")}-$url")

  private def isAuthenticationUrl(url: String): Boolean = url.drop(1).equals(SecuredProcess.SecuredProcessStartUrl)

  // The revertOperations will be None if this the pasphrase page, otherwise it will be Some(Nil) or Some of the LabelCache revertOperations
  // depending on how the value of the ctx
  private def optionalRevertOperations(url: String, ocelotBacklinkBehaviour: Boolean, labels: Labels): Option[List[LabelOperation]] =
    (isAuthenticationUrl(url), ocelotBacklinkBehaviour) match {
      case (true, _) => None
      case (_, true) => Some(labels.revertOperations())
      case (_, false) => Some(Nil)
    }

}
