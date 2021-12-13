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

// $COVERAGE-OFF$

package repositories

import config.AppConfig
import com.google.inject.{Inject, Singleton}
import play.api.libs.json.{Format, Json, Writes}
import core.models.ocelot._
import core.models.ocelot.stanzas.Stanza
import core.models.errors._
import core.models.MongoDateTimeFormats
import core.models.RequestOutcome
import models.{PageNext, GuidanceSession}
import play.modules.reactivemongo.ReactiveMongoComponent
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import java.time.Instant
import uk.gov.hmrc.mongo.ReactiveRepository
import uk.gov.hmrc.mongo.json.ReactiveMongoFormats
import scala.concurrent.{ExecutionContext, Future}
import reactivemongo.api.indexes.IndexType
import reactivemongo.api.indexes.Index
import reactivemongo.bson.BSONInteger

case class SessionKey(id: String, processCode: String)

object SessionKey {
  implicit lazy val format: Format[SessionKey] = Json.format[SessionKey]
}

final case class Session(id: SessionKey,
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
            pageMap: Map[String, PageNext] = Map(),
            lastAccessed: Instant = Instant.now()): Session =
    Session(key, processId, process, Map(), Nil, Map(), pageMap, Map(), Nil, Nil, None, lastAccessed)

  implicit val dateFormat: Format[Instant] = MongoDateTimeFormats.instantFormats
  implicit lazy val format: Format[Session] = ReactiveMongoFormats.mongoEntity { Json.format[Session] }
}

trait SessionRepository {
  def getGuidanceSessionById(key: String, processCode: String): Future[RequestOutcome[GuidanceSession]]
  def getGuidanceSession(key: String, processCode: String, requestId: Option[String]): Future[RequestOutcome[Session]]
  def getResetGuidanceSession(key: String, processCode: String, requestId: Option[String]): Future[RequestOutcome[GuidanceSession]]
  def set(key: String, process: Process, pageMap: Map[String, PageNext]): Future[RequestOutcome[Unit]]
  def saveFormPageState(key: String, processCode: String, url: String, answer: String, labels: Labels, nextLegalPageIs: List[String], requestId: Option[String]): Future[RequestOutcome[Unit]]
  def savePageState(key: String, processCode: String, labels: Labels, requestId: Option[String]): Future[RequestOutcome[Unit]]
  def saveUpdates(key: String, processCode: String, pageHistory: Option[List[PageHistory]], flowStack: Option[List[FlowStage]],
                  labelUpdates: List[Label], legalPageIds: List[String], requestId: Option[String]): Future[RequestOutcome[Unit]]
}

@Singleton
class DefaultSessionRepository @Inject() (config: AppConfig, component: ReactiveMongoComponent)(implicit ec: ExecutionContext)
  extends ReactiveRepository[Session, SessionKey](
    collectionName = "view-external-guidance-session",
    mongo = component.mongoConnector.db,
    domainFormat = Session.format,
    idFormat = SessionKey.format) with SessionRepository {

  val LastAccessedIndexName = "lastAccessedIndex"
  val ExpiryAfterOptionName = "expireAfterSeconds"
  val TtlExpiryFieldName = "lastAccessed"
  val FlowStackKey: String = "flowStack"
  val ContinuationPoolKey: String = "continuationPool"
  val AnswersKey: String = "answers"
  val PageHistoryKey: String = "pageHistory"
  val LabelsKey: String = "labels"
  val LegalPageIdsKey: String = "legalPageIds"
  val RequestId: String = "requestId"
  private type FieldAttr = (String, Json.JsValueWrapper)

  override def ensureIndexes(implicit ec: ExecutionContext): Future[Seq[Boolean]] =
    // If current configuration includes an update to the expiry period of the TTL index, drop the current index to allow its re-creation
    collection.indexesManager.list().flatMap { indexes =>
      indexes
        .filter(idx =>
          idx.name.contains(LastAccessedIndexName) &&
          idx.options.getAs[BSONInteger](ExpiryAfterOptionName).fold(false)(_.as[Int] != config.timeoutInSeconds)
        )
        .map { _ =>
          logger.warn(s"Dropping $LastAccessedIndexName ready for re-creation, due to configured timeout change")
          collection.indexesManager.drop(LastAccessedIndexName).map(ret => logger.info(s"Drop of $LastAccessedIndexName index returned $ret"))
        }

      super.ensureIndexes
    }

  override def indexes: Seq[Index] = {
    logger.info(s"SessionRepository TTL set to ${config.timeoutInSeconds} seconds")
    Seq(
      Index(
        Seq(TtlExpiryFieldName -> IndexType.Ascending),
        name = Some(LastAccessedIndexName),
        options = BSONDocument(ExpiryAfterOptionName -> config.timeoutInSeconds)
      )
    )
  }

  def set(key: String, process: Process, pageMap: Map[String, PageNext]): Future[RequestOutcome[Unit]] =
    findAndUpdate(
      Json.obj("_id" -> SessionKey(key, process.meta.processCode)),
      Json.obj("$set" -> Session(SessionKey(key, process.meta.processCode), process.meta.id, process, pageMap, Instant.now)),
      upsert = true
    )
    .map{_ =>
      logger.warn(s"Session repo creation (key $key) complete for ${process.meta.id}, ${process.meta.processCode}, page count ${pageMap.size}")
      Right(())
    }
    .recover {
      case lastError =>
        logger.error(s"Error $lastError while trying to persist process=${process.meta.id} to session repo using _id=$key")
        Left(DatabaseError)
    }

  def getGuidanceSessionById(key:String, processCode: String): Future[RequestOutcome[GuidanceSession]] =
    find("_id" -> SessionKey(key, processCode)).map {
      case Nil =>  Left(NotFoundError)
      case sp :: _ => Right(GuidanceSession(sp.process,sp.answers,sp.labels,sp.flowStack,sp.continuationPool,sp.pageMap,sp.legalPageIds,sp.pageUrl,None))
    }.recover {
      case lastError =>
      logger.error(s"Error $lastError occurred in method get(key: String) attempting to retrieve session $key")
      Left(DatabaseError)
    }

  def getGuidanceSession(key: String, processCode: String, requestId: Option[String]): Future[RequestOutcome[Session]] =
    findAndUpdate(
      Json.obj("_id" -> SessionKey(key, processCode)),
      Json.obj("$set" -> Json.obj((List(toFieldPair(TtlExpiryFieldName, Json.obj(toFieldPair("$date", Instant.now().toEpochMilli)))) ++
                                        requestId.toList.map(rId => toFieldPair(RequestId, rId))).toArray: _*)),
      fetchNewObject = false // Session returned by findAndUpdate() is intentionally that prior to the update!!
    ).map { r =>
      r.result[Session]
      .fold[RequestOutcome[Session]] {
        logger.warn(s"Attempt to retrieve cached process from session repo with _id=$key returned no result, lastError ${r.lastError}")
        Left(SessionNotFoundError)
      }(sp => Right(sp))
    }.recover { case lastError =>
      logger.error(s"Error $lastError while trying to retrieve process from session repo with _id=$key")
      Left(DatabaseError)
    }

  def getResetGuidanceSession(key: String, processCode: String, requestId: Option[String]): Future[RequestOutcome[GuidanceSession]] =
    findAndUpdate(
      Json.obj("_id" -> SessionKey(key, processCode)),
      Json.obj(
        "$set" -> Json.obj(
          (List(
            toFieldPair(TtlExpiryFieldName, Json.obj(toFieldPair("$date", Instant.now().toEpochMilli))),
            toFieldPair(LegalPageIdsKey, List[String]()),
            toFieldPair(FlowStackKey, List[FlowStage]()),
            toFieldPair(PageHistoryKey, List[PageHistory]()),
            toFieldPair(ContinuationPoolKey, Map[String, Stanza]()),
            toFieldPair(s"${AnswersKey}./${SecuredProcess.SecuredProcessStartUrl}", ""),
            toFieldPair(LabelsKey, Map[String, Label]())) ++ requestId.toList.map(rId => toFieldPair(RequestId, rId))).toArray: _*
        )
      ),
      fetchNewObject = true
    ).map { r =>
      r.result[Session].fold[RequestOutcome[GuidanceSession]] {
      logger.warn(s"Attempt to retrieve cached reset process from session repo with _id=$key returned no result, lastError ${r.lastError}")
      Left(SessionNotFoundError)
      }(sp => Right(GuidanceSession(sp.process,sp.answers,sp.labels,sp.flowStack,sp.continuationPool,sp.pageMap,Nil,sp.pageUrl,None)) )
    }.recover { case lastError =>
      logger.error(s"Error $lastError while trying to retrieve reset process from session repo with _id=$key")
      Left(DatabaseError)
    }

  def saveFormPageState(key: String, processCode: String, url: String, answer: String, labels: Labels, nextLegalPageIds: List[String], requestId: Option[String]): Future[RequestOutcome[Unit]] =
    findAndUpdate(
      Json.obj("_id" -> SessionKey(key, processCode)),
      Json.obj(
        "$set" -> Json.obj(
          (List(
            toFieldPair(TtlExpiryFieldName, Json.obj(toFieldPair("$date", Instant.now().toEpochMilli))),
            toFieldPair(FlowStackKey, labels.flowStack),
            toFieldPair(s"${AnswersKey}.$url", answer),
            toFieldPair(LegalPageIdsKey, nextLegalPageIds)) ++
            labels.poolUpdates.toList.map(l => toFieldPair(s"${ContinuationPoolKey}.${l._1}", l._2)) ++
            labels.updatedLabels.values.map(l => toFieldPair(s"${LabelsKey}.${l.name}", l))).toArray: _*
        )
      )
    ).map(result =>result.result[Session].fold[RequestOutcome[Unit]](Left(NotFoundError))(sp => Right({})))
     .recover{ case lastError =>
      logger.error(s"Error $lastError while trying to update question answers and labels within session repo with _id=$key, url: $url, answer: $answer")
      Left(DatabaseError)
    }

  def savePageState(key: String, processCode: String, labels: Labels, requestId: Option[String]): Future[RequestOutcome[Unit]] =
    findAndUpdate(
      Json.obj("_id" -> SessionKey(key, processCode)),
      Json.obj("$set" -> Json.obj(
        (labels.poolUpdates.toList.map(l => toFieldPair(s"${ContinuationPoolKey}.${l._1}", l._2)) ++
         labels.updatedLabels.values.map(l => toFieldPair(s"${LabelsKey}.${l.name}", l))).toArray :+ toFieldPair(FlowStackKey, labels.flowStack) : _*)
      )
    ).map (result => result.result[Session].fold[RequestOutcome[Unit]](Left(NotFoundError))(sp => Right({})))
     .recover { case lastError =>
      logger.error(s"Error $lastError while trying to update labels within session repo with _id=$key")
      Left(DatabaseError)
    }

  def saveUpdates(key: String,
                  processCode: String,
                  pageHistory: Option[List[PageHistory]],
                  flowStack: Option[List[FlowStage]],
                  labelUpdates: List[Label],
                  legalPageIds: List[String],
                  requestId: Option[String]): Future[RequestOutcome[Unit]] =
    findAndUpdate(
      Json.obj("_id" -> SessionKey(key, processCode)),
      Json.obj(
        "$set" -> Json.obj(
            (List(
              toFieldPair(TtlExpiryFieldName, Json.obj(toFieldPair("$date", Instant.now().toEpochMilli))), toFieldPair(LegalPageIdsKey, legalPageIds)) ++
              pageHistory.fold[List[FieldAttr]](Nil)(ph => List(toFieldPair(PageHistoryKey, ph))) ++
              labelUpdates.map(l => toFieldPair(s"${LabelsKey}.${l.name}", l)) ++
              flowStack.fold[List[FieldAttr]](Nil)(stack => List(toFieldPair(FlowStackKey, stack)))).toArray: _*
        )
      )
    ).map { result => result.result[Session].fold[RequestOutcome[Unit]](Left(NotFoundError))(sp => Right({}))}
     .recover { case lastError =>
      logger.error(s"Error $lastError while trying to savePageHistory to session repo with _id=$key")
      Left(DatabaseError)
    }

  private def toFieldPair[A](name: String, value: A)(implicit w: Writes[A]): FieldAttr = name -> Json.toJsFieldJsValueWrapper(value)
}
