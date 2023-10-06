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
import java.util.concurrent.TimeUnit
import play.api.Logging
import java.time.{Instant}
import org.mongodb.scala._
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Sorts._
import org.mongodb.scala.model._
import uk.gov.hmrc.mongo._
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}
import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats.Implicits._

case class CacheKey(id: String, version: Long)

object CacheKey{
  implicit lazy val format: Format[CacheKey] = Json.format[CacheKey]
}

final case class CachedProcess(
  _id: CacheKey,
  process: Process,
  pageMap: Map[String, PageNext],
  lastAccessed: Instant
)

object CachedProcess {
  implicit lazy val format: Format[CachedProcess] = Json.format[CachedProcess]
}

trait ProcessCacheRepositoryConstants {
  val LastAccessedIndexName = "lastAccessedIndex"
  val TtlExpiryFieldName = "lastAccessed"
}

trait ProcessCacheRepository extends ProcessCacheRepositoryConstants {
  def create(process: Process, pageMap: Map[String, PageNext]): Future[RequestOutcome[CachedProcess]]
  def get(id: String, version: Long): Future[RequestOutcome[CachedProcess]]
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
                              .expireAfter(config.processCacheTimeoutHours, TimeUnit.HOURS))),
    extraCodecs = Seq(Codecs.playFormatCodec(CacheKey.format)),
    replaceIndexes = true // Ensure an updated timeout from config is used
  ) with ProcessCacheRepository with Logging {

  def create(process: Process, pageMap: Map[String, PageNext]): Future[RequestOutcome[CachedProcess]] =
    collection.findOneAndReplace(equal("_id", CacheKey(process.meta.id, process.meta.lastUpdate)),
                                 CachedProcess(CacheKey(process.meta.id, process.meta.lastUpdate), process, pageMap, Instant.now),
                                 FindOneAndReplaceOptions().upsert(true))
    .toFutureOption()
    .map{
      case Some(cachedProcess) =>
        logger.warn(s"Session repo creation _id=(${process.meta.id}, ${process.meta.lastUpdate}) complete for ${process.meta.id}, ${process.meta.processCode}, page count ${pageMap.size}")
        Right(cachedProcess)
      case None =>
        logger.error(s"Session repo creation _id=(${process.meta.id}, ${process.meta.lastUpdate}) failed")
        Left(DatabaseError)
    }
    .recover {
      case ex: MongoCommandException if ex.getErrorCode == 11000 =>
        logger.error(s"Duplicate key Error ${ex.getErrorMessage} while trying to persist process=${process.meta.id} to session repo using _id=(${process.meta.id}, ${process.meta.lastUpdate})")
        Left(DuplicateKeyError)
      case lastError =>
        logger.error(s"Error $lastError while trying to persist process=${process.meta.id} to session repo using _id=(${process.meta.id}, ${process.meta.lastUpdate})")
        Left(DatabaseError)
    }

  def get(id: String, lastUpdate: Long): Future[RequestOutcome[CachedProcess]] =
    collection.find(equal("_id", CacheKey(id, lastUpdate)))
    .headOption()
    .map{
      case Some(cachedProcess) => Right(cachedProcess)
      case None =>
        logger.warn(s"Attempt to retrieve cached process from ProcessCache repo with _id=($id, $lastUpdate), returned no result")
        Left(CachedProcessNotFoundError)
    }.recover { case lastError =>
      logger.error(s"Error $lastError while trying to retrieve cached process from ProcessCache repo with _id=($id, $lastUpdate)")
      Left(DatabaseError)
    }

}
