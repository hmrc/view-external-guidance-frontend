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

package connectors.httpParsers

import core.models.errors.{InternalServerError, InvalidProcessError, NotFoundError}
import core.models.ocelot._
import core.models.RequestOutcome
import play.api.Logger
import play.api.http.Status._
import uk.gov.hmrc.http.HttpReads

object GetProcessHttpParser extends HttpParser {
  val logger: Logger = Logger(getClass)

  implicit val getProcessHttpReads: HttpReads[RequestOutcome[Process]] = {
    case (_, _, response) if response.status == OK =>
      response.validateJson[Process] match {
        case Some(result) => Right(result)
        case None =>
          logger.error("Unable to parse successful response when requesting process")
          Left(InternalServerError)
      }
    case (_, _, response) if response.status == NOT_FOUND =>
      Left(NotFoundError)
    case (_, _, response) if response.status == BAD_REQUEST =>
      Left(InvalidProcessError)
    case unknown =>
      logger.info(s"unexpected $unknown response received when requesting process")
      Left(InternalServerError)
  }

}
