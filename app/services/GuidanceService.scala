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
import models.{ProcessContext, PageDesc, PageNext, PageContext, PageEvaluationContext, RequestOperation, GET, POST}
import play.api.Logger
import core.models.errors.{InternalServerError, InvalidProcessError, AuthenticationError}
import core.models.RequestOutcome
import uk.gov.hmrc.http.HeaderCarrier
import play.api.i18n.{Lang, MessagesApi}
import scala.concurrent.{ExecutionContext, Future}
import repositories.SessionRepository
import core.models.ocelot.{LabelCache, Labels, Process}
import core.models.ocelot.SecuredProcess
import core.models.errors.ExpectationFailedError
import core.models.errors.NotFoundError

@Singleton
class GuidanceService @Inject() (
    appConfig: AppConfig,
    connector: GuidanceConnector,
    sessionRepository: SessionRepository,
    pageBuilder: PageBuilder,
    pageRenderer: PageRenderer,
    spb: SecuredProcessBuilder,
    uiBuilder: UIBuilder,
    messagesApi: MessagesApi
) {
  type Retrieve[A] = String => Future[RequestOutcome[A]]

  val logger: Logger = Logger(getClass)

  def sessionRestart(processCode: String, sessionId: String)(implicit context: ExecutionContext): Future[RequestOutcome[String]] =
    sessionRepository.getResetSession(sessionId).map{
      case Right(ctx) if processCode == ctx.process.meta.processCode =>
        ctx.pageMap.collectFirst{case (k,v) if v.id == ctx.process.startPageId => k}
          .fold[RequestOutcome[String]]{
            logger.error(s"Process start pageId (${ctx.process.startPageId}) missing from retrieved session map" )
            Left(InternalServerError)
          }(Right(_))

      case Right(_) =>
        logger.warn(s"Referenced session ( $sessionId ) does not contain a process with processCode $processCode after session reset")
        Left(ExpectationFailedError)
      case Left(NotFoundError) =>
        Left(ExpectationFailedError)
      case Left(_) =>
        Left(InternalServerError)
    }

  def getProcessContext(sessionId: String): Future[RequestOutcome[ProcessContext]] = sessionRepository.getNoUpdate(sessionId)

  def getProcessContext(sessionId: String, processCode: String, url: String, previousPageByLink: Boolean, op: RequestOperation = GET)
                       (implicit context: ExecutionContext): Future[RequestOutcome[ProcessContext]] = {
    val pageUrl: Option[String] = if (isAuthenticationUrl(url)) None else Some(s"$processCode$url")
    (op match {
      case GET => sessionRepository.getUpdateForGET(sessionId, pageUrl, previousPageByLink)
      case POST => sessionRepository.getUpdateForPOST(sessionId, pageUrl)
    }).map{ result =>
      (result, pageUrl) match {
        case (Right(ctx), Some(_)) if !ctx.secure => Left(AuthenticationError)
        case _ => result
      }
    }
  }

  def getPageEvaluationContext(processCode: String, url: String, previousPageByLink: Boolean, sessionId: String, op: RequestOperation = GET)
                              (implicit context: ExecutionContext, lang: Lang): Future[RequestOutcome[PageEvaluationContext]] =
    getProcessContext(sessionId, processCode, url, previousPageByLink, op).map {
      case Right(ctx) if ctx.process.meta.processCode == processCode =>
        ctx.pageMap.get(url).fold[RequestOutcome[PageEvaluationContext]]{
          logger.error(s"Unable to find url $url within cached process ${ctx.process.meta.id} using sessionId $sessionId")
          Left(NotFoundError)
        }{ pageNext =>
          pageBuilder.buildPage(pageNext.id, ctx.process).fold(
            err => {
              logger.error(s"PageBuilder error $err on process ${ctx.process.meta.id} with sessionId $sessionId")
              Left(InvalidProcessError)
            },
            page => {
              val pageMapById: Map[String, PageDesc] =
                ctx.pageMap.map{case (k, pn) => (pn.id, PageDesc(pn, s"${appConfig.baseUrl}/$processCode${k}"))}
              val (visualStanzas, labels, dataInput) =
                pageRenderer.renderPage(page, LabelCache(ctx.labels,
                                                         Map(),
                                                         ctx.flowStack,
                                                         ctx.continuationPool,
                                                         ctx.process.timescales,
                                                         messagesApi.preferred(Seq(lang)).apply))

              Right(
                PageEvaluationContext(
                  page,
                  visualStanzas,
                  dataInput,
                  sessionId,
                  pageMapById,
                  ctx.process.startUrl.map(_ => s"${appConfig.baseUrl}/${processCode}/session-restart"),
                  ctx.process.title,
                  ctx.process.meta.id,
                  processCode,
                  labels,
                  ctx.backLink.map(bl => s"${appConfig.baseUrl}/$bl"),
                  ctx.answers.get(url)
                )
              )
            }
          )
        }
      case Right(_) =>
        logger.warn(s"Referenced session ( $sessionId ) does not contain a process with processCode $processCode")
        Left(ExpectationFailedError)
      case Left(err) =>
        logger.warn(s"Repository returned $err, when attempting retrieve process using id (sessionId) $sessionId")
        Left(err)
    }

  def getPageContext(pec: PageEvaluationContext, errStrategy: ErrorStrategy = NoError)(implicit lang: Lang): PageContext = {
    val (visualStanzas, labels, dataInput) = pageRenderer.renderPage(pec.page, pec.labels)
    val uiPage = uiBuilder.buildPage(pec.page.url, visualStanzas, errStrategy)(UIContext(labels, lang, pec.pageMapById, messagesApi))
    PageContext(pec.copy(dataInput = dataInput), uiPage, labels)
  }

  def getPageContext(processCode: String, url: String, previousPageByLink: Boolean, sessionId: String)
                    (implicit context: ExecutionContext, lang: Lang): Future[RequestOutcome[PageContext]] =
    getPageEvaluationContext(processCode, url, previousPageByLink, sessionId).map{
      case Right(ctx) =>
        Right(PageContext(ctx, uiBuilder.buildPage(ctx.page.url, ctx.visualStanzas)(UIContext(ctx.labels, lang, ctx.pageMapById, messagesApi))))
      case Left(err) => Left(err)
    }

  def submitPage(ctx: PageEvaluationContext, url: String, validatedAnswer: String, submittedAnswer: String)
                (implicit context: ExecutionContext): Future[RequestOutcome[(Option[String], Labels)]] = {
    val (optionalNext, labels) = pageRenderer.renderPagePostSubmit(ctx.page, ctx.labels, validatedAnswer)
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
