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

package controllers.actions

import javax.inject.Inject
import play.api.mvc.request.Cell
import play.api.mvc.request.RequestAttrKey
import play.api.mvc._
import uk.gov.hmrc.http.SessionKeys
import play.api.Logger
import scala.concurrent.{ExecutionContext, Future}

trait SessionIdAction extends ActionBuilder[Request, AnyContent] {
  val EgNewSessionIdName: String = "EG_NEW_SESSIONID"
}

//
// This action is part of a work-around fix to an issue found with the Boostrap SessionTimeoutFilter.
// The first request after a Session timeout is processed by the SessionTimeoutFilter by removing all
// session keys (other than those in a configured list) from the request before it is handled by the
// application code. Similarly it clears any non-reserved keys from the Result returned by
// the application code when the controller action completes.
// The work around adds a new key to the configured reserve list which is not deleted by the
// SessiomTimeoutFilter. This key (EG_NEW_SESSIONID), is created when a process view begins and is added
// to the result which is redirected to another action within the Guidance controller which is wrapped
// in this SessionIdAction. The SessionIdAction takes the value of the EG_NEW_SESSIONID and creates
// a new session id key (SessionKeys.sessionId) in the request before passing control to the
// application action. The EG_NEW_SESSIONID is deleted from the updated request.
//
class SessionIdActionImpl @Inject()(val parser: BodyParsers.Default)
                               (implicit val executionContext: ExecutionContext)
                               extends SessionIdAction {

  val logger: Logger = Logger(getClass)

  override def invokeBlock[A](request: Request[A], block:Request[A] => Future[Result]): Future[Result] = {
    val egNewSessionIdLog = s"$EgNewSessionIdName ${request.session.data.get(EgNewSessionIdName)}"
    val sessionIdLog = s"${request.session.data.get(SessionKeys.sessionId)}"

    logger.warn(s"SessionIdAction sessionId = $sessionIdLog, $egNewSessionIdLog")
    request.session.data.get(EgNewSessionIdName).fold(block(request)){egNewId =>
      val updatedSession = Session((request.session.data -- List(SessionKeys.sessionId, EgNewSessionIdName)) ++ List(SessionKeys.sessionId -> egNewId))
      val updatedRequest = Request(request.addAttr[Cell[Session]](RequestAttrKey.Session, Cell(updatedSession)), request.body)
      block(updatedRequest).map(res => res.addingToSession(SessionKeys.sessionId -> egNewId)(updatedRequest))
    }
  }
}
