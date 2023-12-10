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

// $COVERAGE-OFF$

package repositories

import config.AppConfig
import com.google.inject.{Inject, Singleton}
import play.api.libs.json.{Format, Json}
import core.models.ocelot._
import core.models.ocelot.stanzas.{PopulatedStanza, Stanza}
import core.models.errors._
import core.models.RequestOutcome
import models.PageNext
import java.util.concurrent.TimeUnit
import play.api.Logger
import java.time.{Instant}
import org.mongodb.scala._
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Sorts._
import org.mongodb.scala.model.Updates.combine
import org.mongodb.scala.model._
import org.mongodb.scala.result.DeleteResult
import uk.gov.hmrc.mongo._
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}
import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats.Implicits._

case class SessionKey(id: String, processCode: String)

object SessionKey {
  implicit lazy val format: Format[SessionKey] = Json.format[SessionKey]
}

final case class Session(
    _id: SessionKey,
   runMode: Option[RunMode],
   processId: String,
   labels: Map[String, Label],
   flowStack: List[FlowStage],
   continuationPool: Map[String, Stanza],
   answers: Map[String, String],
   pageHistory: List[PageHistory],
   legalPageIds: List[String],
   requestId: Option[String],
   lastAccessed: Instant, // expiry time
   processVersion: Long
)

object Session {
  def apply(key: SessionKey,
            runMode: RunMode,
            processId: String,
            processVersion: Long,
            legalPageIds: List[String],
            lastAccessed: Instant = Instant.now): Session =
    Session(key, Some(runMode), processId, Map(), Nil, Map(), Map(), Nil, legalPageIds, None, lastAccessed, processVersion)

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
  val TtlExpiryFieldName = "lastAccessed"
}

trait SessionRepository extends SessionRepositoryConstants {
  def create(id: String, meta: Meta, runMode: RunMode, legalPageIds: List[String]): Future[RequestOutcome[Unit]]
  def delete(key: String, processCode: String): Future[RequestOutcome[Unit]]
  def getNoUpdate(key: String, processCode: String): Future[RequestOutcome[Session]]
  def get(key: String, processCode: String, requestId: Option[String]): Future[RequestOutcome[Session]]
  def reset(key: String, processCode: String, requestId: Option[String]): Future[RequestOutcome[Session]]
  def updateForNewPage(key: String, processCode: String, pageHistory: Option[List[PageHistory]], flowStack: Option[List[FlowStage]],
                       labelUpdates: List[Label], legalPageIds: List[String], requestId: Option[String]): Future[RequestOutcome[Unit]]
  def updateAfterStandardPage(key: String, processCode: String, labels: Labels, requestId: Option[String]): Future[RequestOutcome[Unit]]
  def updateAfterFormSubmission(key: String, processCode: String, answerId: String, answer: String, labels: Labels, nextLegalPageIds: List[String],
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

  def create(id: String, meta: Meta, runMode: RunMode, legalPageIds: List[String]): Future[RequestOutcome[Unit]] =
    collection.findOneAndReplace(equal("_id", SessionKey(id, meta.processCode)),
                                 Session(SessionKey(id, meta.processCode), runMode, meta.id, meta.lastUpdate, legalPageIds, Instant.now),
                                 FindOneAndReplaceOptions().upsert(true))
    .toFutureOption()
    .map{
      case _ =>
      logger.warn(s"Session repo creation; key:($id, ${meta.processCode}) processVersion: ${meta.lastUpdate} complete")
      Right(())
    }
    .recover {
      case ex: MongoCommandException if ex.getErrorCode == 11000 =>
        // Appears two concurrent findOneAndReplace() finding no underlying doc, would then both try to insert with the second triggering a duplicate key err
        logger.error(s"Duplicate key Error ${ex.getErrorMessage} while trying to persist process=${id} to session repo using _id=($id, ${meta.processCode})")
        Left(DuplicateKeyError)
      case lastError =>
        logger.error(s"Error $lastError while trying to persist process=${id} to session repo using _id=($id, ${meta.processCode})")
        Left(DatabaseError)
    }

  def delete(key: String, processCode: String): Future[RequestOutcome[Unit]] =
    collection
      .deleteOne(equal("_id", SessionKey(key, processCode)))
      .toFutureOption()
      .map {
        case Some(result: DeleteResult) if result.getDeletedCount > 0 => Right(())
        case Some(result: DeleteResult) =>
          logger.warn(s"Attempt to delete session for $key and $processCode failed as no such session exists")
          Left(NotFoundError)
        case None =>
          logger.error(s"Attempt to delete session for $key and $processCode failed")
          Left(DatabaseError)
      }
      .recover {
        case error =>
          logger.error(s"Attempt to delete session for $key and $processCode failed with error : ${error.getMessage}")
          Left(DatabaseError)
      }

  def getNoUpdate(key:String, processCode: String): Future[RequestOutcome[Session]] =
    collection
      .find(equal("_id", SessionKey(key, processCode)))
      .headOption()
      .map{
        case None =>  Left(SessionNotFoundError)
        case Some(session) => Right(session)
      }
      .recover {
        case lastError =>
        logger.error(s"Error $lastError occurred in method getNoUpdate attempting to retrieve session ($key, $processCode)")
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
      case Some(session) => Right(session)
    }.recover { case lastError =>
      logger.error(s"Error $lastError while trying to retrieve process from session repo with _id=$key")
      Left(DatabaseError)
    }

  def reset(key: String, processCode: String, requestId: Option[String]): Future[RequestOutcome[Session]] =
    collection.findOneAndUpdate(
      equal("_id", SessionKey(key, processCode)),
      combine((List(
        Updates.set(TtlExpiryFieldName, Instant.now()),
        Updates.set(LegalPageIdsKey, List[String]()),
        Updates.set(FlowStackKey, List[FlowStage]()),
        Updates.set(PageHistoryKey, List[PageHistory]()),
        Updates.set(ContinuationPoolKey, Map[String, PopulatedStanza]()),
        Updates.set(s"${AnswersKey}./${SecuredProcess.SecuredProcessStartUrl}", ""),
        Updates.set(LabelsKey, Map[String, Label]())) ++ requestId.toList.map(rId => Updates.set(RequestId, rId))).toIndexedSeq: _*)
    )
    .toFutureOption()
    .map{
      case None =>
        logger.warn(s"Attempt to retrieve cached process from session repo with _id=$key returned no result")
        Left(SessionNotFoundError)
      case Some(sp) => Right(sp)
    }.recover { case lastError =>
      logger.error(s"Error $lastError while trying to retrieve reset process from session repo with _id=$key")
      Left(DatabaseError)
    }

  def updateAfterFormSubmission( key: String,
                                 processCode: String,
                                 answerId: String,
                                 answer: String,
                                 labels: Labels,
                                 nextLegalPageIds: List[String],
                                 requestId: Option[String]): Future[RequestOutcome[Unit]] =
    collection.findOneAndUpdate(
      requestId.fold(equal("_id", SessionKey(key, processCode)))(rId => and(equal("_id", SessionKey(key, processCode)), equal(RequestId, rId))),
      combine((List(
        Updates.set(TtlExpiryFieldName, Instant.now()),
        Updates.set(FlowStackKey, Codecs.toBson(labels.flowStack)),
        Updates.set(s"${AnswersKey}.$answerId", Codecs.toBson(answer)),
        Updates.set(LegalPageIdsKey, Codecs.toBson(nextLegalPageIds))) ++
        labels.poolUpdates.toList.map(l => Updates.set(s"${ContinuationPoolKey}.${l._1}", Codecs.toBson(l._2))) ++
        labels.updatedLabels.values.map(l => Updates.set(s"${LabelsKey}.${l.name}", Codecs.toBson(l)))).toIndexedSeq: _*
      )
    )
    .toFutureOption()
    .map{
      case None => Left(NotFoundError)
      case _ => Right({})
    }
    .recover{ case lastError =>
      logger.error(s"Error $lastError while trying to update question answers and labels within session repo with _id=$key, answerId: $answerId, answer: $answer")
      Left(DatabaseError)
    }

  def updateAfterStandardPage(key: String, processCode: String, labels: Labels, requestId: Option[String]): Future[RequestOutcome[Unit]] =
    collection.findOneAndUpdate(
      requestId.fold(equal("_id", SessionKey(key, processCode)))(rId => and(equal("_id", SessionKey(key, processCode)), equal(RequestId, rId))),
      combine((
        (labels.poolUpdates.toList.map(l => Updates.set(s"${ContinuationPoolKey}.${l._1}", Codecs.toBson(l._2))) ++
         labels.updatedLabels.values.map(l => Updates.set(s"${LabelsKey}.${l.name}", Codecs.toBson(l)))).toIndexedSeq :+
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
        flowStack.fold[List[Bson]](Nil)(stack => List(Updates.set(FlowStackKey, Codecs.toBson(stack))))).toIndexedSeq: _*
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
