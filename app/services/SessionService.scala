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

import play.api.Logging
import config.AppConfig
import javax.inject.{Inject, Singleton}
import models.GuidanceSession
import core.models.RequestOutcome
import scala.concurrent.{ExecutionContext, Future}
import repositories.{PageHistory, Session, SessionRepository, ProcessCacheRepository}
import models.PageNext
import core.models.ocelot.{Process, RunMode, Label, Labels, FlowStage}
import core.models.ocelot.Debugging

@Singleton
class SessionService @Inject() (appConfig: AppConfig, sessionRepository: SessionRepository, processCacheRepository: ProcessCacheRepository) extends Logging {

  def create(id: String, runMode: RunMode, process: Process, pageMap: Map[String, PageNext], legalPageIds: List[String])(implicit ec: ExecutionContext): Future[RequestOutcome[Unit]] = {
    logger.warn(s"Attempting to create a session sessionId: $id and processId: ${process.meta.id}")
    sessionRepository.create(id, process.meta, runMode, legalPageIds).flatMap{
      case Right(_) => processCacheRepository.create(process, pageMap, runMode).map{
          case Right(_) => Right(())
          case Left(err) => Left(err)
        }
      case Left(err) => Future.successful(Left(err))
    }
  }

  def delete(key: String, processCode: String): Future[RequestOutcome[Unit]] = sessionRepository.delete(key, processCode)

  def getNoUpdate(key: String, processCode: String)(implicit context: ExecutionContext): Future[RequestOutcome[GuidanceSession]] = {
    sessionRepository.getNoUpdate(key, processCode).flatMap{
      case Right(session) => guidanceSession(session)
      case Left(err) => Future.successful(Left(err))
    }
  }

  def get(key: String, processCode: String, requestId: Option[String])(implicit context: ExecutionContext): Future[RequestOutcome[GuidanceSession]] = {
    sessionRepository.get(key, processCode, requestId).flatMap{
      case Right(session) => guidanceSession(session)
      case Left(err) => Future.successful(Left(err))
    }
  }

  def reset(key: String, processCode: String, requestId: Option[String])(implicit context: ExecutionContext): Future[RequestOutcome[GuidanceSession]] = {
    sessionRepository.reset(key, processCode, requestId).flatMap{
      case Right(session) => guidanceSession(session)
      case Left(err) => Future.successful(Left(err))
    }
  }

  def updateForNewPage(key: String, processCode: String, pageHistory: Option[List[PageHistory]], flowStack: Option[List[FlowStage]],
                       labelUpdates: List[Label], legalPageIds: List[String], requestId: Option[String]): Future[RequestOutcome[Unit]] =
    sessionRepository.updateForNewPage(key, processCode, pageHistory, flowStack, labelUpdates, legalPageIds, requestId)

  def updateAfterStandardPage(key: String, processCode: String, labels: Labels, requestId: Option[String]): Future[RequestOutcome[Unit]] =
    sessionRepository.updateAfterStandardPage(key, processCode, labels, requestId)

  def updateAfterFormSubmission(key: String, processCode: String, answerId: String, answer: String, labels: Labels, nextLegalPageIds: List[String],
                                requestId: Option[String]): Future[RequestOutcome[Unit]] =
    sessionRepository.updateAfterFormSubmission(key, processCode, answerId, answer, labels, nextLegalPageIds, requestId)

  private[services] def guidanceSession(session: Session)(implicit context: ExecutionContext): Future[RequestOutcome[GuidanceSession]] = {
    val processId = if (session.runMode.equals(Some(Debugging))) s"${session.processId}${processCacheRepository.DebugIdSuffix}" else session.processId
    processCacheRepository.get(processId, session.processVersion, session.timescalesVersion, session.ratesVersion).map{
      case Right(cachedProcess) => Right(GuidanceSession(session, cachedProcess.process, cachedProcess.pageMap))
      case Left(err) => Left(err)
    }
  }

}
