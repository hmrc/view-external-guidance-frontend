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

package controllers

import play.api.mvc._
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import core.models.errors._
import core.models.RequestOutcome
import play.api.Logger
import scala.concurrent.Future

trait SessionFrontendController {
  this: FrontendController =>
  val logger: Logger

  protected def withExistingSession[T](block: String => Future[RequestOutcome[T]])(implicit request: Request[_]): Future[RequestOutcome[T]] =
    hc.sessionId.fold[Future[RequestOutcome[T]]] {
      logger.warn(s"Session Id missing from request when required, requestId: ${hc.requestId.map(_.value).getOrElse("")}, URI: ${request.target.uriString}")
      Future.successful(Left(ExpectationFailedError))
    } { sessionId =>
      logger.info(s"WithExisting sessionId: ${sessionId.value}, requestId: ${hc.requestId.map(_.value).getOrElse("")}, URI: ${request.target.uriString}")
      block(sessionId.value)
    }
}
