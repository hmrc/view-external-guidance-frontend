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

package controllers.apis

import config.AppConfig
import play.api.libs.json.{Format, Json}
import play.api.mvc._
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import repositories.ProcessCacheRepository
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext
import play.api.Logging
import models.admin.CachedProcessSummary
import core.models.errors.CachedProcessNotFoundError

@Singleton
class AdminApiController @Inject() (appConfig: AppConfig, processCacheRepository: ProcessCacheRepository, mcc: MessagesControllerComponents) 
  extends FrontendController(mcc) with Logging {

  implicit val ec: ExecutionContext = mcc.executionContext
  implicit lazy val config: AppConfig = appConfig
  implicit lazy val cachedProcessSummaryFormats: Format[CachedProcessSummary] = Json.format[CachedProcessSummary]

  def listActiveProcessSummaries(): Action[AnyContent] = Action.async {_ =>
    processCacheRepository.listSummaries().map{
      case Right(list: List[CachedProcessSummary]) => Ok(Json.toJson(list))
      case Left(err) => 
        logger.error(s"Unable to retrieve list of active process summaries, error : $err")
        InternalServerError(s"Unable to retrieve list of active process summaries, error : $err")
    }
  }

  def getActive(id: String, version: Long, timescalesVersion: Option[Long], ratesVersion: Option[Long]): Action[AnyContent] = Action.async { _ =>    
    processCacheRepository.get(id, version, timescalesVersion, ratesVersion).map {
      case Right(cachedProcess) => Ok(Json.toJson(cachedProcess.process))
      case Left(CachedProcessNotFoundError) =>
        logger.error(s"Unable to retrieve active process ($id, $version)")
        NotFound(s"Unable to retrieve active process ($id, $version), err = $CachedProcessNotFoundError")
      case Left(err) =>
        logger.error(s"Unable to retrieve active process ($id, $version), err = $err")
        InternalServerError(s"Unable to retrieve active process ($id, $version), err = $err")
    }
  }
}
