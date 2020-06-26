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

package controllers.actions

import javax.inject.Inject
import play.api.mvc.request.Cell
import play.api.mvc.request.RequestAttrKey
import play.api.mvc._
import uk.gov.hmrc.http.SessionKeys
import play.api.Logger
import scala.concurrent.{ExecutionContext, Future}

trait SessionIdAction extends ActionBuilder[Request, AnyContent] with ActionFunction[Request, Request] {
  val EgNewSessionIdName: String = "EG_NEW_SESSIONID"
}

class SessionIdActionImpl @Inject()(val parser: BodyParsers.Default)
                               (implicit val executionContext: ExecutionContext) 
                               extends SessionIdAction {

  val logger: Logger = Logger(getClass)

  override def invokeBlock[A](request: Request[A], block:Request[A] => Future[Result]): Future[Result] = {

    logger.info(s"SessionIdAction Before: sessionId = ${request.session.data.get(SessionKeys.sessionId)}, $EgNewSessionIdName ${request.session.data.get(EgNewSessionIdName)}")

    request.session.data.get(EgNewSessionIdName).fold(block(request)){egNewId =>
      val updatedSession = Session((request.session.data -- List(SessionKeys.sessionId, EgNewSessionIdName)) ++ List((SessionKeys.sessionId -> egNewId)))
      block(Request(request.addAttr[Cell[Session]](RequestAttrKey.Session, Cell(updatedSession)), request.body)).map{ res =>
        res.withSession(updatedSession)
      }
    }

  }
}
