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

// $COVERAGE-OFF$

package repositories

import config.AppConfig
import com.google.inject.{Inject, Singleton}
import play.api.libs.json.{Format, Json}
import core.models.ocelot._
import core.models.ocelot.stanzas.Stanza
import core.models.errors._
import core.models.RequestOutcome
import models.{PageNext, GuidanceSession}
import java.util.concurrent.TimeUnit
import play.api.Logger
import java.time.{Instant}
import org.mongodb.scala._
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Sorts._
import org.mongodb.scala.model.Updates.combine
import org.mongodb.scala.model._
import uk.gov.hmrc.mongo._
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}
import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats.Implicits._

case class SessionKey(id: String, processCode: String)

object SessionKey {
  implicit lazy val format: Format[SessionKey] = Json.format[SessionKey]
}

final case class Session(_id: SessionKey,
                         processId: String,
                         process: Process,
                         labels: Map[String, Label],
                         flowStack: List[FlowStage],
                         continuationPool: Map[String, Stanza],
                         pageMap: Map[String, PageNext],
                         answers: Map[String, String],
                         pageHistory: List[PageHistory],
                         legalPageIds: List[String],
                         requestId: Option[String],
                         lastAccessed: Instant) {
  lazy val pageUrl: Option[String] = pageHistory.reverse.headOption.map(_.url.drop(process.meta.processCode.length))
}

object Session {
  def apply(key: SessionKey,
            processId: String,
            process: Process,
            legalPageIds: List[String],
            pageMap: Map[String, PageNext] = Map(),
            lastAccessed: Instant = Instant.now): Session =
    Session(key, processId, process, Map(), Nil, Map(), pageMap, Map(), Nil, legalPageIds, None, lastAccessed)

  implicit lazy val format: Format[Session] = Json.format[Session]
}

trait SessionRepositoryConstants {
  val FlowStackKey: String = "flowStack"
  val ContinuationPoolKey: String = "continuationPool"
  val AnswersKey: String = "answers"
  val PageHistoryKey: String = "pageHistory"
  val LabelsKey: String = "labels"
  val LegalPageIdsKey: String = "legalPageIds"
  val RequestId: String = "requestId"
  val LastAccessedIndexName = "lastAccessedIndex"
  val ExpiryAfterOptionName = "expireAfterSeconds"
  val TtlExpiryFieldName = "lastAccessed"
}

trait SessionRepository extends SessionRepositoryConstants {
  def create(key: String, process: Process, pageMap: Map[String, PageNext], legalPageIds: List[String]): Future[RequestOutcome[Unit]]
  def getById(key: String, processCode: String): Future[RequestOutcome[GuidanceSession]]
  def get(key: String, processCode: String, requestId: Option[String]): Future[RequestOutcome[Session]]
  def reset(key: String, processCode: String, requestId: Option[String]): Future[RequestOutcome[GuidanceSession]]
  def updateForNewPage(key: String, processCode: String, pageHistory: Option[List[PageHistory]], flowStack: Option[List[FlowStage]],
                       labelUpdates: List[Label], legalPageIds: List[String], requestId: Option[String]): Future[RequestOutcome[Unit]]
  def updateAfterStandardPage(key: String, processCode: String, labels: Labels, requestId: Option[String]): Future[RequestOutcome[Unit]]
  def updateAfterFormSubmission(key: String, processCode: String, url: String, answer: String, labels: Labels, nextLegalPageIds: List[String],
                                requestId: Option[String]): Future[RequestOutcome[Unit]]
}

object DefaultSessionRepository extends SessionRepositoryConstants

@Singleton
class DefaultSessionRepository @Inject() (config: AppConfig, component: MongoComponent)(implicit ec: ExecutionContext)
  extends PlayMongoRepository[Session](
    collectionName = "view-external-guidance-session",
    mongoComponent = component,
    domainFormat = Session.format,
    indexes = Seq(IndexModel(ascending(DefaultSessionRepository.TtlExpiryFieldName),
                             IndexOptions()
                              .name(DefaultSessionRepository.LastAccessedIndexName)
                              .unique(false)
                              .expireAfter(config.timeoutInSeconds, TimeUnit.SECONDS))),
    extraCodecs = Seq(Codecs.playFormatCodec(SessionKey.format)),
    replaceIndexes = true // Ensure an updated timeout from config is used
  ) with SessionRepository {
  val logger: Logger = Logger(getClass)

  def create(key: String, process: Process, pageMap: Map[String, PageNext], legalPageIds: List[String]): Future[RequestOutcome[Unit]] =
    collection.findOneAndReplace(equal("_id", SessionKey(key, process.meta.processCode)),
                                 Session(SessionKey(key, process.meta.processCode), process.meta.id, process, legalPageIds, pageMap, Instant.now),
                                 FindOneAndReplaceOptions().upsert(true))
    .toFutureOption
    .map{
      case _ =>
      logger.warn(s"Session repo creation (key $key) complete for ${process.meta.id}, ${process.meta.processCode}, page count ${pageMap.size}")
      Right(())
    }
    .recover {
      case ex: MongoCommandException if ex.getErrorCode == 11000 =>
        logger.error(s"Duplicate key Error ${ex.getErrorMessage} while trying to persist process=${process.meta.id} to session repo using _id=$key")
        Left(DuplicateKeyError)
      case lastError =>
        logger.error(s"Error $lastError while trying to persist process=${process.meta.id} to session repo using _id=$key")
        Left(DatabaseError)
    }

  def getById(key:String, processCode: String): Future[RequestOutcome[GuidanceSession]] =
    collection
      .find(equal("_id", SessionKey(key, processCode)))
      .headOption()
      .map{
        case None =>  Left(SessionNotFoundError)
        case Some(sp) => Right(GuidanceSession(sp.process,sp.answers,sp.labels,sp.flowStack,sp.continuationPool,sp.pageMap,sp.legalPageIds,sp.pageUrl,None))
      }
      .recover {
        case lastError =>
        logger.error(s"Error $lastError occurred in method get(key: String) attempting to retrieve session $key")
        Left(DatabaseError)
      }

  def get(key: String, processCode: String, requestId: Option[String]): Future[RequestOutcome[Session]] =
    collection.findOneAndUpdate(
      equal("_id", SessionKey(key, processCode)),
      requestId.fold(Updates.set(TtlExpiryFieldName, Instant.now())){rId =>
        combine(
          Updates.set(TtlExpiryFieldName, Instant.now()),
          Updates.set(RequestId, rId)
        )
      }
    )
    .toFutureOption()
    .map{
      case None =>
        logger.warn(s"Attempt to retrieve cached process from session repo with _id=$key returned no result")
        Left(SessionNotFoundError)
      case Some(session) =>
        Right(session)
    }.recover { case lastError =>
      logger.error(s"Error $lastError while trying to retrieve process from session repo with _id=$key")
      Left(DatabaseError)
    }

  def reset(key: String, processCode: String, requestId: Option[String]): Future[RequestOutcome[GuidanceSession]] =
    collection.findOneAndUpdate(
      equal("_id", SessionKey(key, processCode)),
      combine((List(
        Updates.set(TtlExpiryFieldName, Instant.now()),
        Updates.set(LegalPageIdsKey, List[String]()),
        Updates.set(FlowStackKey, List[FlowStage]()),
        Updates.set(PageHistoryKey, List[PageHistory]()),
        Updates.set(ContinuationPoolKey, Map[String, Stanza]()),
        Updates.set(s"${AnswersKey}./${SecuredProcess.SecuredProcessStartUrl}", ""),
        Updates.set(LabelsKey, Map[String, Label]())) ++ requestId.toList.map(rId => Updates.set(RequestId, rId))).toArray: _*)
    )
    .toFutureOption()
    .map{
      case None =>
        logger.warn(s"Attempt to retrieve cached process from session repo with _id=$key returned no result")
        Left(SessionNotFoundError)
      case Some(sp) =>
        Right(GuidanceSession(sp.process,sp.answers,sp.labels,sp.flowStack,sp.continuationPool,sp.pageMap,Nil,sp.pageUrl,None))
    }.recover { case lastError =>
      logger.error(s"Error $lastError while trying to retrieve reset process from session repo with _id=$key")
      Left(DatabaseError)
    }

  def updateAfterFormSubmission( key: String,
                                 processCode: String,
                                 url: String,
                                 answer: String,
                                 labels: Labels,
                                 nextLegalPageIds: List[String],
                                 requestId: Option[String]): Future[RequestOutcome[Unit]] =
    collection.findOneAndUpdate(
      requestId.fold(equal("_id", SessionKey(key, processCode)))(rId => and(equal("_id", SessionKey(key, processCode)), equal(RequestId, rId))),
      combine((List(
        Updates.set(TtlExpiryFieldName, Instant.now()),
        Updates.set(FlowStackKey, Codecs.toBson(labels.flowStack)),
        Updates.set(s"${AnswersKey}.$url", Codecs.toBson(answer)),
        Updates.set(LegalPageIdsKey, Codecs.toBson(nextLegalPageIds))) ++
        labels.poolUpdates.toList.map(l => Updates.set(s"${ContinuationPoolKey}.${l._1}", Codecs.toBson(l._2))) ++
        labels.updatedLabels.values.map(l => Updates.set(s"${LabelsKey}.${l.name}", Codecs.toBson(l)))).toArray: _*
      )
    )
    .toFutureOption()
    .map{
      case None => Left(NotFoundError)
      case _ => Right({})
    }
    .recover{ case lastError =>
      logger.error(s"Error $lastError while trying to update question answers and labels within session repo with _id=$key, url: $url, answer: $answer")
      Left(DatabaseError)
    }

  def updateAfterStandardPage(key: String, processCode: String, labels: Labels, requestId: Option[String]): Future[RequestOutcome[Unit]] =
    collection.findOneAndUpdate(
      requestId.fold(equal("_id", SessionKey(key, processCode)))(rId => and(equal("_id", SessionKey(key, processCode)), equal(RequestId, rId))),
      combine((
        (labels.poolUpdates.toList.map(l => Updates.set(s"${ContinuationPoolKey}.${l._1}", Codecs.toBson(l._2))) ++
         labels.updatedLabels.values.map(l => Updates.set(s"${LabelsKey}.${l.name}", Codecs.toBson(l)))).toArray :+
         Updates.set(FlowStackKey, Codecs.toBson(labels.flowStack)) : _*)
      )
    )
    .toFutureOption()
    .map{
      case None => Left(NotFoundError)
      case _ => Right({})
    }
    .recover { case lastError =>
      logger.error(s"Error $lastError while trying to update labels within session repo with _id=$key")
      Left(DatabaseError)
    }

  def updateForNewPage(key: String,
                       processCode: String,
                       pageHistory: Option[List[PageHistory]],
                       flowStack: Option[List[FlowStage]],
                       labelUpdates: List[Label],
                       legalPageIds: List[String],
                       requestId: Option[String]): Future[RequestOutcome[Unit]] =
    collection.findOneAndUpdate(
      requestId.fold(equal("_id", SessionKey(key, processCode)))(rId => and(equal("_id", SessionKey(key, processCode)), equal(RequestId, rId))),
      combine((List(
        Updates.set(TtlExpiryFieldName, Instant.now()), Updates.set(LegalPageIdsKey, Codecs.toBson(legalPageIds))) ++
        pageHistory.fold[List[Bson]](Nil)(ph => List(Updates.set(PageHistoryKey, Codecs.toBson(ph)))) ++
        labelUpdates.map(l => Updates.set(s"${LabelsKey}.${l.name}", Codecs.toBson(l))) ++
        flowStack.fold[List[Bson]](Nil)(stack => List(Updates.set(FlowStackKey, Codecs.toBson(stack))))).toArray: _*
      )
    )
    .toFutureOption()
    .map{
      case None => Left(NotFoundError)
      case _ => Right({})
    }
    .recover { case lastError =>
      logger.error(s"Error $lastError while trying to savePageHistory to session repo with _id=$key")
      Left(DatabaseError)
    }
}
