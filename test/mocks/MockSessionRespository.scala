/*
 * Copyright 2020 HM Revenue & Customs
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

import repositories.{ProcessContext, SessionRepository}
import models.ocelot.Process
import models.RequestOutcome
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import scala.concurrent.Future

trait MockSessionRepository extends MockFactory {

  val mockSessionRepository: SessionRepository = mock[SessionRepository]

  object MockSessionRepository {

    def get(key: String, pageUrl: String): CallHandler[Future[RequestOutcome[ProcessContext]]] =
      (mockSessionRepository
        .get(_: String, _: String))
        .expects(key, pageUrl)

    def set(key: String, process: Process): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionRepository
        .set(_: String, _: Process))
        .expects(key, process)

    def saveAnswerToQuestion(key: String, url: String, answer: String): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionRepository
        .saveAnswerToQuestion(_: String, _: String, _: String))
        .expects(key, url, answer)

    def get(key: String): CallHandler[Future[RequestOutcome[ProcessContext]]] =
      (mockSessionRepository
        .get(_: String))
        .expects(key)

    def removeSession(key: String): CallHandler[Future[RequestOutcome[Unit]]] =
      (mockSessionRepository
        .removeSession(_: String))
      .expects(key)

  }

}
