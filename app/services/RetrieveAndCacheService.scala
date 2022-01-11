/*
 * Copyright 2022 HM Revenue & Customs
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
import core.models.ocelot.Process
import models.PageNext

@Singleton
class RetrieveAndCacheService @Inject() (
    connector: GuidanceConnector,
    sessionRepository: SessionRepository,
    pageBuilder: PageBuilder,
    spb: SecuredProcessBuilder
) {
  type Retrieve[A] = String => Future[RequestOutcome[A]]

  val logger: Logger = Logger(getClass)

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
          sessionRepository.create(docId, process, pages.map(p => p.url -> PageNext(p.id, p.next.toList, p.linked.toList)).toMap).map{
            case Right(_) => Right((pages.head.url, process.meta.processCode))
            case Left(err) =>
              logger.error(s"Failed to store new parsed process in session repository, $err")
              Left(err)
          }
        })
    }

  private def map[A, B](f: Retrieve[A])(g: A => B)(implicit ec: ExecutionContext): Retrieve[B] =
    id => f(id).map{
      case Right(result) => Right(g(result))
      case Left(err) => Left(err)
    }
}
