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

package mocks

import repositories.{CachedProcess, ProcessCacheRepository}
import models.PageNext
import core.models.ocelot._
import core.models.RequestOutcome
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import scala.concurrent.Future

trait MockProcessCacheRepository extends MockFactory {

  val mockProcessCacheRepository: ProcessCacheRepository = mock[ProcessCacheRepository]

  object MockSessionRepository {

    def create(process: Process, pageMap: Map[String, PageNext]): CallHandler[Future[RequestOutcome[CachedProcess]]] =
      (mockProcessCacheRepository
        .create(_: Process, _: Map[String, PageNext]))
        .expects(process, pageMap)

    def get(id: String, lastUpdate: Long): CallHandler[Future[RequestOutcome[CachedProcess]]] =
      (mockProcessCacheRepository
        .get(_: String, _: Long))
        .expects(id, lastUpdate)

  }
}
