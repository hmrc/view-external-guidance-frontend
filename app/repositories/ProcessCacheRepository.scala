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
import core.models.errors._
import core.models.RequestOutcome
import models.PageNext
import models.admin.CachedProcessSummary
import java.util.concurrent.TimeUnit
import play.api.Logging
import java.time.Instant
import java.time.temporal.ChronoUnit
import org.mongodb.scala._
import org.mongodb.scala.result.UpdateResult
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Sorts._
import org.mongodb.scala.model._
import org.mongodb.scala.model.Updates.combine
import uk.gov.hmrc.mongo._
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}
import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats.Implicits._  

case class CacheKey(id: String, processVersion: Long, timescalesVersion: Option[Long], ratesVersion: Option[Long])

object CacheKey{
  implicit lazy val format: Format[CacheKey] = Json.format[CacheKey]
}

final case class CachedProcess(
  _id: CacheKey,
  process: Process,
  pageMap: Map[String, PageNext],
  expiryTime: Instant
)

object CachedProcess {
  implicit lazy val format: Format[CachedProcess] = Json.format[CachedProcess]
}

trait ProcessCacheRepositoryConstants {
  val LastAccessedIndexName = "lastAccessedIndex"
  val TtlExpiryFieldName = "expiryTime"
  val ProcessFieldName = "process"
  val PageMapFieldName = "pageMap"
}

trait ProcessCacheRepository extends ProcessCacheRepositoryConstants {
  def create(process: Process, pageMap: Map[String, PageNext], runMode: RunMode): Future[RequestOutcome[Unit]]
  def get(id: String, processVersion: Long, timescalesVersion: Option[Long], ratesVersion: Option[Long]): Future[RequestOutcome[CachedProcess]]
  def listSummaries(): Future[RequestOutcome[List[CachedProcessSummary]]]
}

object DefaultProcessCacheRepository extends ProcessCacheRepositoryConstants

@Singleton
class DefaultProcessCacheRepository @Inject() (config: AppConfig, component: MongoComponent)(implicit ec: ExecutionContext)
  extends PlayMongoRepository[CachedProcess](
    collectionName = "view-external-guidance-process",
    mongoComponent = component,
    domainFormat = CachedProcess.format,
    indexes = Seq(IndexModel(ascending(DefaultProcessCacheRepository.TtlExpiryFieldName),
                             IndexOptions()
                              .name(DefaultProcessCacheRepository.LastAccessedIndexName)
                              .unique(false)
                              .expireAfter(0, TimeUnit.SECONDS))),
    extraCodecs = Seq(Codecs.playFormatCodec(CacheKey.format)),
    replaceIndexes = true // Ensure an updated timeout from config is used
  ) with ProcessCacheRepository with Logging {

  implicit lazy val cachedProcessSummaryformat: Format[CachedProcessSummary] = Json.format[CachedProcessSummary]

  def create(process: Process, pageMap: Map[String, PageNext], runMode: RunMode): Future[RequestOutcome[Unit]] = {
    collection.updateOne(equal("_id", CacheKey(process.meta.id, process.meta.lastUpdate, process.meta.timescalesVersion, process.meta.ratesVersion)),
                                combine(List(
                                  Updates.set(TtlExpiryFieldName, expiryInstant(runMode, Instant.now)),
                                  Updates.set(ProcessFieldName, Codecs.toBson(process)),
                                  Updates.set(PageMapFieldName, Codecs.toBson(pageMap))
                                ).toIndexedSeq: _*),
                                UpdateOptions().upsert(true))
    .toFutureOption()
    .map{
      case Some(result: UpdateResult) if result.wasAcknowledged => 
        logger.warn(s"Session repo creation _id=(${process.meta.id}, ${process.meta.lastUpdate}) complete, page count ${pageMap.size}")
        Right(())
      case other =>
        logger.error(s"Session repo creation _id=(${process.meta.id}, ${process.meta.lastUpdate}) failed with $other")
        Left(DatabaseError)
    }
    .recover {
      case ex: MongoCommandException if ex.getErrorCode == 11000 =>
        logger.error(s"Duplicate key Error ${ex.getErrorMessage} while trying to persist process=${process.meta.id} to session repo")
        Left(DuplicateKeyError)
      case lastError =>
        logger.error(s"Error $lastError while trying to persist process=(${process.meta.id}, ${process.meta.lastUpdate}) to session repo")
        Left(DatabaseError)
    }
  }

  def get(id: String, processVersion: Long, timescalesVersion: Option[Long], ratesVersion: Option[Long]): Future[RequestOutcome[CachedProcess]] =
    collection.find(equal("_id", CacheKey(id, processVersion, timescalesVersion, ratesVersion)))
    .headOption()
    .map{
      case Some(cachedProcess) => Right(cachedProcess)
      case None =>
        logger.warn(s"Attempt to retrieve cached process from ProcessCache repo with _id=($id, $processVersion), returned no result")
        Left(CachedProcessNotFoundError)
    }.recover { case lastError =>
      logger.error(s"Error $lastError while trying to retrieve cached process from ProcessCache repo with _id=($id, $processVersion)")
      Left(DatabaseError)
    }

  def listSummaries(): Future[RequestOutcome[List[CachedProcessSummary]]] =
    collection
      .find()
      .collect()
      .toFutureOption()
      .map{
        case Some(result) => 
          Right(result.map(r => CachedProcessSummary(r._id.id, 
                                                     r._id.processVersion, 
                                                     r._id.timescalesVersion, 
                                                     r._id.ratesVersion, 
                                                     r.process.meta.title, 
                                                     r.expiryTime)).toList)
        case _ => Right(Nil)
      }
      .recover {
        case error =>
          logger.error(s"Attempt to retrieve approval process summaries failed with error : ${error.getMessage}")
          Left(DatabaseError)
      }

  private[repositories] def expiryInstant(runMode: RunMode, when: Instant): Instant =
    runMode match {
      case Scratch => when.plus(config.processCacheScratchTimeoutHours, ChronoUnit.HOURS)
      case _ => when.plus(config.processCacheTimeoutHours, ChronoUnit.HOURS)
    }
}
