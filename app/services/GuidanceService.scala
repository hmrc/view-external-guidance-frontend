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

package services

import core.services._
import config.AppConfig
import connectors.GuidanceConnector
import javax.inject.{Inject, Singleton}
import models.{GuidanceSession, PageDesc, PageNext, PageContext, PageEvaluationContext}
import play.api.Logger
import core.models.errors._
import core.models.RequestOutcome
import uk.gov.hmrc.http.HeaderCarrier
import play.api.i18n.{Lang, MessagesApi}
import scala.concurrent.{ExecutionContext, Future}
import repositories.{SessionFSM, SessionRepository}
import core.models.ocelot.{LabelCache, Labels, Process, Label}
import core.models.ocelot.SecuredProcess

@Singleton
class GuidanceService @Inject() (
    appConfig: AppConfig,
    connector: GuidanceConnector,
    sessionRepository: SessionRepository,
    pageBuilder: PageBuilder,
    pageRenderer: PageRenderer,
    spb: SecuredProcessBuilder,
    uiBuilder: UIBuilder,
    transition: SessionFSM,
    messagesApi: MessagesApi
) {
  type Retrieve[A] = String => Future[RequestOutcome[A]]

  val logger: Logger = Logger(getClass)

  def sessionRestart(processCode: String, sessionId: String)(implicit context: ExecutionContext): Future[RequestOutcome[String]] =
    sessionRepository.getResetGuidanceSession(sessionId, processCode).map{
      case Right(ctx) =>
        ctx.pageMap.collectFirst{case (k,v) if v.id == ctx.process.startPageId => k}
          .fold[RequestOutcome[String]]{
            logger.error(s"Process start pageId (${ctx.process.startPageId}) missing from retrieved session map" )
            Left(InternalServerError)
          }(Right(_))
      case Left(err) => Left(err)
    }

  def getCurrentGuidanceSession(sessionId: String): Future[RequestOutcome[GuidanceSession]] = sessionRepository.getGuidanceSessionById(sessionId)

  def getSubmitEvaluationContext(processCode: String, url: String, sessionId: String)
                                (implicit context: ExecutionContext, lang: Lang): Future[RequestOutcome[PageEvaluationContext]] = {
    val pageUrl: Option[String] = if (isAuthenticationUrl(url)) None else Some(s"$processCode$url")
    getSubmitGuidanceSession(sessionId, processCode, pageUrl).map{
      case Left(err) => Left(err)
      case Right(session) if pageUrl.isDefined && !session.secure => Left(AuthenticationError)
      case Right(session) => buildEvaluationContext(sessionId, processCode, url, session)
    }
  }

  def getPageEvaluationContext(processCode: String, url: String, previousPageByLink: Boolean, sessionId: String)
                              (implicit context: ExecutionContext, lang: Lang): Future[RequestOutcome[PageEvaluationContext]] = {
    val pageUrl: Option[String] = if (isAuthenticationUrl(url)) None else Some(s"$processCode$url")
    getPageGuidanceSession(sessionId, processCode, pageUrl, previousPageByLink).map{
      case Left(err) => Left(err)
      case Right(session) if pageUrl.isDefined && !session.secure => Left(AuthenticationError)
      case Right(session) => buildEvaluationContext(sessionId, processCode, url, session)
    }
  }

  def getSubmitGuidanceSession(key: String, processCode: String, pageUrl: Option[String])
                              (implicit context: ExecutionContext): Future[RequestOutcome[GuidanceSession]] =
    sessionRepository.getGuidanceSession(key, processCode).map{
      case Left(err) => Left(err)
      // If incoming url equals the most recent page history url proceed, otherwise, the POST is out of sequence (IllegalPageSubmissionError)
      case Right(sp) if pageUrl.fold(true)(url => sp.pageHistory.reverse.headOption.fold(false)(ph => url.equals(ph.url))) =>
        val backlink = pageUrl.fold[Option[String]](None){_ =>
          sp.pageHistory.reverse match {
            case _ :: y :: _ => Some(y.url)
            case _ => None
          }
        }
        Right(GuidanceSession(sp.process, sp.answers, sp.labels, sp.flowStack, sp.continuationPool, sp.pageMap, sp.legalPageIds, sp.pageUrl, backlink))
      case Right(sp) => Left(IllegalPageSubmissionError)
    }

  def getPageGuidanceSession(key: String, processCode: String, pageUrl: Option[String], previousPageByLink: Boolean)
                            (implicit context: ExecutionContext): Future[RequestOutcome[GuidanceSession]] =
    sessionRepository.getGuidanceSession(key, processCode).flatMap{
      case Left(err) => Future.successful(Left(err))
      case Right(sp) =>
      pageUrl.fold[Future[RequestOutcome[GuidanceSession]]](
        Future.successful(Right(GuidanceSession(sp.process, sp.answers, sp.labels, sp.flowStack, sp.continuationPool, sp.pageMap, Nil, sp.pageUrl, None)))
      ){url =>
        sp.pageMap.get(url.drop(sp.process.meta.processCode.length)).fold[Future[RequestOutcome[GuidanceSession]]]{
          logger.warn(s"Attempt to move to unknown page $url in process ${sp.processId}, page count = ${sp.pageMap.size}")
          Future.successful(Left(NotFoundError))
        }{pageNext =>
          logger.debug(s"Incoming Page: ${pageNext.id}, $url, current legalPageIds: ${sp.legalPageIds}")
          if (sp.legalPageIds.isEmpty || sp.legalPageIds.contains(pageNext.id)){ // Wild card or fixed list of valid page ids
            val firstPageUrl: String = s"${sp.process.meta.processCode}${sp.process.startUrl.getOrElse("")}"
            val (backLink, historyUpdate, flowStackUpdate, labelUpdates) = transition(url, sp, previousPageByLink, firstPageUrl)
            val labels: Map[String, Label] = sp.labels ++ labelUpdates.map(l => l.name -> l).toMap
            val legalPageIds = (pageNext.id :: Process.StartStanzaId :: pageNext.linked ++
                                backLink.fold(List.empty[String])(bl => List(sp.pageMap(bl.drop(sp.process.meta.processCode.length)).id))).distinct
            val session = GuidanceSession(sp.process, sp.answers, labels, flowStackUpdate.getOrElse(sp.flowStack),
                                          sp.continuationPool, sp.pageMap, legalPageIds, sp.pageUrl, backLink)
            sessionRepository.saveUpdates(key, historyUpdate, flowStackUpdate, labelUpdates, legalPageIds).map {
              case Left(err) =>
                logger.error(s"Unable to update session data, error = $err")
                Left(err)
              case _ => Right(session)
            }
          } else {
            logger.warn(s"Attempt to move to illegal page $url, LEGALPIDS ${sp.legalPageIds}")
            Future.successful(Left(ForbiddenError))
          }
        }
      }
    }

  def getPageContext(pec: PageEvaluationContext, errStrategy: ErrorStrategy = NoError)(implicit lang: Lang): RequestOutcome[PageContext] =
    pageRenderer.renderPage(pec.page, pec.labels) match {
      case Left(err) =>
        logger.error(s"Encountered non terminating page error within page ${pec.page.id} of processCode ${pec.processCode}")
        Left(err)
      case Right((visualStanzas, labels, dataInput)) =>
        val uiPage = uiBuilder.buildPage(pec.page.url, visualStanzas, errStrategy)(UIContext(labels, lang, pec.pageMapById, messagesApi))
        Right(PageContext(pec.copy(dataInput = dataInput), uiPage, labels))
    }

  def getPageContext(processCode: String, url: String, previousPageByLink: Boolean, sessionId: String)
                    (implicit context: ExecutionContext, lang: Lang): Future[RequestOutcome[PageContext]] =
    getPageEvaluationContext(processCode, url, previousPageByLink, sessionId).map{
      case Right(ctx) =>
        Right(PageContext(ctx, uiBuilder.buildPage(ctx.page.url, ctx.visualStanzas)(UIContext(ctx.labels, lang, ctx.pageMapById, messagesApi))))
      case Left(err) => Left(err)
    }

  def submitPage(ctx: PageEvaluationContext, url: String, validatedAnswer: String, submittedAnswer: String)
                (implicit context: ExecutionContext): Future[RequestOutcome[(Option[String], Labels)]] =
    pageRenderer.renderPagePostSubmit(ctx.page, ctx.labels, validatedAnswer) match {
      case Left(err) => Future.successful(Left(err))
      case Right((optionalNext, labels)) =>
        optionalNext.fold[Future[RequestOutcome[(Option[String], Labels)]]](Future.successful(Right((None, labels)))){next =>
          logger.debug(s"Next page found at stanzaId: $next")
          sessionRepository.saveFormPageState(ctx.sessionId, url, submittedAnswer, labels, List(next)).map{
            case Left(err) =>
              logger.error(s"Failed to save updated labels, error = $err")
              Left(InternalServerError)
            case Right(_) => Right((Some(next), labels))
          }
        }
    }

  private def buildEvaluationContext(sessionId: String, processCode: String, url: String, gs: GuidanceSession)
                                    (implicit lang: Lang): RequestOutcome[PageEvaluationContext] =
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
          val pageMapById: Map[String, PageDesc] = gs.pageMap.map{case (k, pn) => (pn.id, PageDesc(pn, s"${appConfig.baseUrl}/$processCode${k}"))}
          val labelCache: Labels = LabelCache(gs.labels, Map(), gs.flowStack, gs.continuationPool, gs.process.timescales, messagesApi.preferred(Seq(lang)).apply)
          pageRenderer.renderPage(page, labelCache) match {
            case Left(err) => Left(err)
            case Right((visualStanzas, labels, dataInput)) =>
              Right(
                PageEvaluationContext(
                  page, visualStanzas, dataInput, sessionId, pageMapById, gs.process.startUrl.map(_ => s"${appConfig.baseUrl}/${processCode}/session-restart"),
                  gs.process.title, gs.process.meta.id, processCode, labels, gs.backLink.map(bl => s"${appConfig.baseUrl}/$bl"), gs.answers.get(url)
                )
              )
          }
        })
    }

  def validateUserResponse(ctx: PageEvaluationContext, response: String): Option[String] =
    ctx.dataInput.fold[Option[String]](None)(_.validInput(response))

  def savePageState(sessionId: String, labels: Labels): Future[RequestOutcome[Unit]] = sessionRepository.savePageState(sessionId, labels)

  def retrieveAndCacheScratch(uuid: String, docId: String)
                             (implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[(String,String)]] =
    retrieveAndCache(uuid, docId, map(connector.scratchProcess)(p => spb.secureIfRequired(p.copy(meta = p.meta.copy(id = uuid)))))

  def retrieveAndCachePublished(processCode: String, docId: String)
                               (implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[(String,String)]] =
    retrieveAndCache(processCode, docId, map(connector.publishedProcess)(spb.secureIfRequired))

  def retrieveAndCacheApproval(processId: String, docId: String)
                              (implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[(String,String)]] =
    retrieveAndCache(processId, docId, map(connector.approvalProcess)(spb.secureIfRequired))

  def retrieveAndCacheApprovalByPageUrl(url: String)(processId: String, docId: String)
                              (implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[(String,String)]] =
    retrieveAndCache(processId, docId, connector.approvalProcess).map{
      case Right((_, processCode)) => Right((url, processCode))
      case err @ Left(_) => err
    }

  private def retrieveAndCache(processIdentifier: String, docId: String, retrieveProcessById: Retrieve[Process])(
    implicit context: ExecutionContext
  ): Future[RequestOutcome[(String,String)]] =
    retrieveProcessById(processIdentifier).flatMap {
      case Left(err) =>
        logger.warn(s"Unable to find process using identifier $processIdentifier, received $err")
        Future.successful(Left(err))
      case Right(process) =>
        logger.warn(s"Loaded process ${process.meta.id}, containing ${process.flow.keys.toList.length} stanzas, ${process.phrases.length} phrases")
        pageBuilder.pages(process, process.startPageId).fold(err => {
          logger.warn(s"Failed to parse process with error $err")
          Future.successful(Left(InvalidProcessError))
        },
        pages => {
          if (logger.isDebugEnabled) {
            val urlMap: Map[String, String] = pages.map(p => (p.id, p.url)).toMap
            logger.debug(s"Process id: $processIdentifier, processCode: ${process.meta.processCode}, title: ${process.meta.title}")
            logger.debug(s"PAGE MAP:")
            pages.foreach{pge =>
              logger.debug(s"PAGE: ${pge.id}, ${pge.url}")
              pge.next.foreach(id => logger.debug(s"\tnxt:=> $id, ${urlMap(id)}"))
              pge.linked.foreach(id => logger.debug(s"\tlnk:=> $id, ${urlMap(id)}"))
            }
          }
          sessionRepository.set(docId, process, pages.map(p => p.url -> PageNext(p.id, p.next.toList, p.linked.toList)).toMap).map {
            case Right(_) => Right((pages.head.url, process.meta.processCode))
            case Left(err) =>
              logger.error(s"Failed to store new parsed process in session repository, $err")
              Left(err)
          }
        })
    }

  private def isAuthenticationUrl(url: String): Boolean = url.drop(1).equals(SecuredProcess.SecuredProcessStartUrl)

  private def map[A, B](f: Retrieve[A])(g: A => B)(implicit ec: ExecutionContext): Retrieve[B] =
    id => f(id).map{
      case Right(result) => Right(g(result))
      case Left(err) => Left(err)
    }
}
