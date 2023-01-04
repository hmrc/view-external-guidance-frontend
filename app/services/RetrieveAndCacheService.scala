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
import connectors.GuidanceConnector
import javax.inject.{Inject, Singleton}
import play.api.Logger
import core.models.errors._
import core.models.RequestOutcome
import uk.gov.hmrc.http.HeaderCarrier
import scala.concurrent.{ExecutionContext, Future}
import repositories.SessionRepository
import models.PageNext
import core.models.ocelot.{Process, RunMode, PageReview, Scratch, Approval, Published, Page}

@Singleton
class RetrieveAndCacheService @Inject() (
    connector: GuidanceConnector,
    sessionRepository: SessionRepository,
    pageBuilder: PageBuilder,
    spb: SecuredProcessBuilder
)(implicit ec: ExecutionContext) {
  type Retrieve[A] = String => Future[RequestOutcome[A]]

  val logger: Logger = Logger(getClass)

  def retrieveAndCacheScratch(uuid: String, docId: String)
                             (implicit hc: HeaderCarrier): Future[RequestOutcome[(String,String)]] =
    retrieveAndCache(uuid, docId, map(connector.scratchProcess)(p => spb.secureIfRequired(p.copy(meta = p.meta.copy(id = uuid)))), Scratch)

  def retrieveAndCachePublished(processCode: String, docId: String)
                               (implicit hc: HeaderCarrier): Future[RequestOutcome[(String,String)]] =
    retrieveAndCache(processCode, docId, map(connector.publishedProcess)(spb.secureIfRequired), Published)

  def retrieveAndCacheApproval(processId: String, docId: String)
                              (implicit hc: HeaderCarrier): Future[RequestOutcome[(String,String)]] =
    retrieveAndCache(processId, docId, map(connector.approvalProcess)(spb.secureIfRequired), Approval)

  def retrieveAndCacheApprovalByPageUrl(url: String)(processId: String, docId: String)
                              (implicit hc: HeaderCarrier): Future[RequestOutcome[(String,String)]] =
    retrieveAndCache(processId, docId, connector.approvalProcess, PageReview, Some(url))

  def retrieveOnlyPublished(processCode: String)(implicit hc: HeaderCarrier): Future[RequestOutcome[(Process, Seq[Page])]] =
    retrieve(processCode, map(connector.publishedProcess)(spb.secureIfRequired))

  def retrieveOnlyApproval(processCode: String)(implicit hc: HeaderCarrier): Future[RequestOutcome[(Process, Seq[Page])]] =
    retrieve(processCode, map(connector.approvalProcessByProcessCode)(spb.secureIfRequired))

  private def retrieve(processCode: String, retrieveProcessById: Retrieve[Process]): Future[RequestOutcome[(Process, Seq[Page])]] =
    retrieveProcessById(processCode).map {
      case Left(err) =>
        logger.warn(s"Unable to find process using identifier $processCode, received $err")
        Left(err)
      case Right(process) =>
        logger.warn(s"Loaded process ${process.meta.id}, containing ${process.flow.keys.toList.length} stanzas, ${process.phrases.length} phrases")
        pageBuilder.pages(process, process.startPageId).fold(err => {
            logger.warn(s"Unable to parse process with identifier $processCode, received $err")
            Left(InvalidProcessError)
          },
          pages => Right((process, pages))
        )
    }

  private def retrieveAndCache(processCode: String, docId: String, retrieveProcessById: Retrieve[Process], runMode: RunMode, url: Option[String] = None): Future[RequestOutcome[(String,String)]] =
    retrieve(processCode, retrieveProcessById).flatMap {
      case Left(err) =>
        logger.warn(s"Unable to process using identifier $processCode, received $err")
        Future.successful(Left(err))
      case Right((process, pages)) =>
        //
        // Left intentially commented out
        //
        // if (logger.isDebugEnabled) {
        //   val urlMap: Map[String, String] = pages.map(p => (p.id, p.url)).toMap
        //   logger.debug(s"Process id: $processCode, processCode: ${process.meta.processCode}, title: ${process.meta.title}")
        //   logger.debug(s"PAGE MAP:")
        //   pages.foreach{pge =>
        //     logger.debug(s"PAGE: ${pge.id}, ${pge.url}")
        //     pge.next.foreach(id => logger.debug(s"\tnxt:=> $id, ${urlMap(id)}"))
        //     pge.linked.foreach(id => logger.debug(s"\tlnk:=> $id, ${urlMap(id)}"))
        //   }
        // }

        val pageMap: Map[String, PageNext] = pages.map(p => p.url -> PageNext(p.id, p.next.toList, p.linked.toList)).toMap
        val startPageUrl: String = url.getOrElse(pages.head.url)
        val startPageId: Option[String] = pageMap.get(startPageUrl).map(_.id)
        sessionRepository.create(docId, runMode, process, pageMap, startPageId.toList).map{
          case Right(_) => Right((startPageUrl, process.meta.processCode))
          case Left(err) =>
            logger.error(s"Failed to store new parsed process in session repository, $err")
            Left(err)
        }
      }

  private def map[A, B](f: Retrieve[A])(g: A => B)(implicit ec: ExecutionContext): Retrieve[B] =
    id => f(id).map{
      case Right(result) => Right(g(result))
      case Left(err) => Left(err)
    }
}
