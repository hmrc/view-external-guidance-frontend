/*
 * Copyright 2022 HM Revenue & Customs
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

import play.api.http.Status
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.HeaderCarrier
import scala.concurrent.{ExecutionContext, Future}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import uk.gov.hmrc.http.SessionKeys

class SessionIdActionSpec extends base.ViewSpec  with GuiceOneAppPerSuite {

  case class Harness(action: SessionIdAction)(block:Request[_] => Result) {
    def onPageLoad(): Action[AnyContent] = action{req => block(req)}
  }

  trait Test {

    val path: String = "/path"
    implicit val hc: HeaderCarrier = HeaderCarrier()
    implicit val ec: ExecutionContext = app.injector.instanceOf[ExecutionContext]
    val sessionId = s"session-${java.util.UUID.randomUUID.toString}"
    val egSessionId = s"session-${java.util.UUID.randomUUID.toString}"
    lazy val sessionIdAction = new SessionIdActionImpl(app.injector.instanceOf[BodyParsers.Default])

    lazy val target = Harness(sessionIdAction) _
  }

  "SessionIdAction" should {

    "Pass on requests containing only sessionid" in new Test {

      val result: Future[Result] = target{ request => Results.Ok.withSession(request.session)}
                                    .onPageLoad()(FakeRequest("GET", path).withSession(SessionKeys.sessionId -> sessionId))
      session(result).get(SessionKeys.sessionId) shouldBe Some(sessionId)
      status(result) shouldBe Status.OK
    }

    "Convert EG_NEW_SESSIONID into a sessionId" in new Test {

      target{ request =>
        request.session.data.get(sessionIdAction.EgNewSessionIdName) shouldBe None
        request.session.data.get(SessionKeys.sessionId) shouldBe Some(egSessionId)
        Results.Ok.withSession(request.session)
      }.onPageLoad()(FakeRequest("GET", path).withSession(sessionIdAction.EgNewSessionIdName -> egSessionId))
      
    }

    "Add nothing if neither sessionId or EG_NEW_SESSIONID found" in new Test {

      target{ request =>
        request.session.data.get(sessionIdAction.EgNewSessionIdName) shouldBe None
        request.session.data.get(SessionKeys.sessionId) shouldBe None
        Results.Ok.withSession(request.session)
      }.onPageLoad()(FakeRequest("GET", path))
      
    }

  }

}
