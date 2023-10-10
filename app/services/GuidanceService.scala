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
import models.{GuidanceSession, PageDesc, PageContext, PageEvaluationContext}
import play.api.Logger
import uk.gov.hmrc.http.HeaderCarrier
import core.models.errors._
import core.models.RequestOutcome
import play.api.i18n.{MessagesApi, Messages}
import scala.concurrent.{ExecutionContext, Future}
import repositories.SessionFSM
import core.models.ocelot.{LabelCache, Labels, Process, Label, flowPath}
import core.models.ocelot.SecuredProcess

@Singleton
class GuidanceService @Inject() (
    appConfig: AppConfig,
    sessionService: SessionService,
    pageBuilder: PageBuilder,
    pageRenderer: PageRenderer,
    spb: SecuredProcessBuilder,
    uiBuilder: UIBuilder,
    transition: SessionFSM,
    messagesApi: MessagesApi
) {
  val logger: Logger = Logger(getClass)

  def sessionRestart(processCode: String, sessionId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[String]] =
    sessionService.reset(sessionId, processCode, hc.requestId.map(_.value)).map{
      case Right(session) =>
        session.pageMap.collectFirst{case (k,v) if v.id == session.process.startPageId => k}
          .fold[RequestOutcome[String]]{
            logger.error(s"Process start pageId (${session.process.startPageId}) missing from retrieved session map" )
            Left(InternalServerError)
          }(Right(_))
      case Left(err) => Left(err)
    }

  def deleteSession(processCode: String, sessionId: String): Future[RequestOutcome[Unit]] = sessionService.delete(sessionId, processCode)

  def getSubmitPageContext(pec: PageEvaluationContext, errStrategy: ErrorStrategy = NoError)(implicit messages: Messages): RequestOutcome[PageContext] =
    pageRenderer.renderPage(pec.page, pec.labels) match {
      case Left(err) =>
        logger.error(s"Execution error on page ${pec.page.id} of processCode ${pec.processCode}")
        Left(err)
      case Right((visualStanzas, labels, dataInput)) =>
        uiBuilder.buildPage(pec.page.url, visualStanzas, errStrategy)(UIContext(labels, pec.pageMapById, messages)).fold(err => Left(err), uiPage =>
          Right(PageContext(pec.copy(dataInput = dataInput), uiPage, labels))
        )
    }

  def getPageContext(processCode: String, url: String, previousPageByLink: Boolean, sessionId: String)
                    (implicit hc: HeaderCarrier, context: ExecutionContext, messages: Messages): Future[RequestOutcome[PageContext]] =
    getPageEvaluationContext(processCode, url, previousPageByLink, sessionId).map{
      case Right(ctx) =>
        uiBuilder.buildPage(ctx.page.url, ctx.visualStanzas.toList)(UIContext(ctx.labels, ctx.pageMapById, messages)).fold(err => Left(err), page =>
          Right(PageContext(ctx, page))
        )
      case Left(err) => Left(err)
    }

  def getCurrentGuidanceSession(processCode: String)(sessionId: String)(implicit context: ExecutionContext): Future[RequestOutcome[GuidanceSession]] =
    sessionService.getNoUpdate(sessionId, processCode)

  def getPageEvaluationContext(processCode: String, url: String, previousPageByLink: Boolean, sessionId: String)
                              (implicit hc: HeaderCarrier, context: ExecutionContext, messages: Messages): Future[RequestOutcome[PageEvaluationContext]] = {
    val pageUrl: Option[String] = if (isAuthenticationUrl(url)) None else Some(s"$processCode$url")
    getPageGuidanceSession(sessionId, processCode, pageUrl, previousPageByLink).map{
      case Left(err) => Left(err)
      case Right(session) if pageUrl.isDefined && !session.secure => Left(AuthenticationError)
      case Right(session) => buildEvaluationContext(sessionId, processCode, url, session)
    }
  }

  def getPageGuidanceSession(key: String, processCode: String, pageUrl: Option[String], previousPageByLink: Boolean)
                            (implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[GuidanceSession]] =
    sessionService.get(key, processCode, hc.requestId.map(_.value)).flatMap{
      case Left(err) => Future.successful(Left(err))
      case Right(sp) =>
      pageUrl.fold[Future[RequestOutcome[GuidanceSession]]](Future.successful(Right(sp))){url =>
        sp.pageMap.get(url.drop(sp.process.meta.processCode.length)).fold[Future[RequestOutcome[GuidanceSession]]]{
          logger.warn(s"Attempt to move to unknown page $url in process ${sp.process.meta.id}, page count = ${sp.pageMap.size}")
          Future.successful(Left(NotFoundError))
        }{pageNext =>
          logger.debug(s"Incoming Page: ${pageNext.id}, $url, current legalPageIds: ${sp.legalPageIds}")
          val requestId: Option[String] = hc.requestId.map(_.value)
          if (sp.legalPageIds.isEmpty || sp.legalPageIds.contains(pageNext.id)){ // Wild card or fixed list of valid page ids
            val firstPageUrl: String = s"${sp.process.meta.processCode}${sp.process.startUrl.getOrElse("")}"
            val (backLink, historyUpdate, flowStackUpdate, labelUpdates) = transition(url, sp.pageHistory, sp.flowStack, previousPageByLink, firstPageUrl)
            val labels: Map[String, Label] = sp.labels ++ labelUpdates.map(l => l.name -> l).toMap
            val legalPageIds = (pageNext.id :: Process.StartStanzaId :: pageNext.linked ++
                                backLink.fold(List.empty[String])(bl => List(sp.pageMap(bl.drop(sp.process.meta.processCode.length)).id))).distinct

            sessionService.updateForNewPage(key, processCode, historyUpdate, flowStackUpdate, labelUpdates, legalPageIds, requestId).map {
              case Left(NotFoundError) =>
                logger.warn(s"TRANSACTION FAULT(Recoverable): saveUpdates _id=$key, requestId: $requestId")
                Left(TransactionFaultError)
              case Left(err) =>
                logger.error(s"Unable to update session data, error = $err")
                Left(err)
              case _ => Right(sp.copy(labels = labels, flowStack = flowStackUpdate.getOrElse(sp.flowStack), backLink = backLink))
            }
          } else {
            logger.warn(s"Attempt to move to illegal page $url, LEGALPIDS ${sp.legalPageIds}")
            Future.successful(Left(ForbiddenError))
          }
        }
      }
    }

  def getSubmitEvaluationContext(processCode: String, url: String, sessionId: String)
                                (implicit hc: HeaderCarrier, context: ExecutionContext, messages: Messages): Future[RequestOutcome[PageEvaluationContext]] = {
    val pageUrl: Option[String] = if (isAuthenticationUrl(url)) None else Some(s"$processCode$url")
    getSubmitGuidanceSession(sessionId, processCode, pageUrl).map{
      case Left(err) => Left(err)
      case Right(session) if pageUrl.isDefined && !session.secure => Left(AuthenticationError)
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
                (implicit hc: HeaderCarrier, context: ExecutionContext, messages: Messages): Future[RequestOutcome[(Option[String], Labels)]] =
    pageRenderer.renderPagePostSubmit(ctx.page, ctx.labels, validatedAnswer) match {
      case Left(err) => Future.successful(Left(err))
      case Right((optionalNext, labels)) =>
        val requestId: Option[String] = hc.requestId.map(_.value)
        optionalNext.fold[Future[RequestOutcome[(Option[String], Labels)]]](Future.successful(Right((None, labels)))){next =>
          logger.debug(s"Next page found at stanzaId: $next")
          sessionService.updateAfterFormSubmission(ctx.sessionId, ctx.processCode, answerStorageId(ctx.labels, url), submittedAnswer, labels, List(next), requestId).map{
            case Left(NotFoundError) =>
              logger.warn(s"TRANSACTION FAULT(Recoverable): saveFormPageState _id=${ctx.sessionId}, url: $url, answer: $validatedAnswer, requestId: ${requestId}")
              Left(TransactionFaultError)
            case Left(err) =>
              logger.error(s"Failed to save updated labels, error = $err")
              Left(InternalServerError)
            case Right(_) => Right((Some(next), labels))
          }
        }
    }

  def savePageState(sessionId: String, processCode: String, labels: Labels)
                   (implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[Unit]] =
    sessionService.updateAfterStandardPage(sessionId, processCode, labels, hc.requestId.map(_.value)).map{
      case Left(NotFoundError) =>
        logger.warn(s"TRANSACTION FAULT(Recoverable): saveLabels _id=$sessionId, requestId: ${hc.requestId.map(_.value)}")
        Left(TransactionFaultError)
      case result => result
    }

  private def messagesFn(k: String, args: Seq[Any])(implicit messages: Messages): String = messages(k, args: _*)

  private def buildEvaluationContext(sessionId: String, processCode: String, url: String, gs: GuidanceSession)
                                    (implicit messages: Messages): RequestOutcome[PageEvaluationContext] =
    gs.pageMap.get(url).fold[RequestOutcome[PageEvaluationContext]]{
      logger.error(s"Unable to find url $url within cached process ${gs.process.meta.id} using sessionId $sessionId")
      Left(NotFoundError)
    }{ pageNext =>
      pageBuilder.buildPage(pageNext.id, gs.process).fold(
        err => {
          logger.error(s"PageBuilder error $err on process ${gs.process.meta.id} with sessionId $sessionId")
          Left(InvalidProcessError)
        },
        page => {
          val pageMapById: Map[String, PageDesc] =
            gs.pageMap.map{case (k, pn) => (pn.id, PageDesc(pn, s"${appConfig.baseUrl}/$processCode${k}"))}
          val labels: Labels =
            LabelCache(gs.labels, Map(), gs.flowStack, gs.continuationPool, gs.process.timescales, messagesFn, gs.runMode)
          pageRenderer.renderPage(page, labels) match {
            case Left(err) => Left(err)
            case Right((visualStanzas, updatedLabels, dataInput)) =>
              val processTitle: models.ui.Text = TextBuilder.fromPhrase(gs.process.title)(UIContext(updatedLabels, pageMapById, messages))
              Right(
                PageEvaluationContext(
                  page, visualStanzas, dataInput, sessionId, pageMapById, gs.process.startUrl.map(_ => s"${appConfig.baseUrl}/${processCode}/session-restart"),
                  processTitle, gs.process.meta.id, processCode, updatedLabels, gs.backLink.map(bl => s"${appConfig.baseUrl}/$bl"), gs.answers.get(answerStorageId(updatedLabels, url)),
                  gs.process.betaPhaseBanner
                )
              )
          }
        })
    }

  private def answerStorageId(labels: Labels, url: String): String = flowPath(labels.flowStack).fold(url)(fp => s"${fp.replaceAll("[ .]", "_")}-$url")

  private def isAuthenticationUrl(url: String): Boolean = url.drop(1).equals(SecuredProcess.SecuredProcessStartUrl)

}
