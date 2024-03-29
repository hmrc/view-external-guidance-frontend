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

package views.admin

import base.{BaseSpec, ViewFns, ViewSpec}
import core.models.ocelot._
import core.models.ocelot.stanzas._
import models.admin._
import play.api.i18n.{Messages, MessagesApi}
import play.api.test.FakeRequest
import views.html.admin.stanza_table

class StanzaTableSpec extends BaseSpec with ViewSpec with ViewFns {

  private trait Test {
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    val stable = injector.instanceOf[stanza_table]
    implicit val request: FakeRequest[_] = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(request)

    val ppm = ProcessPageStructure("1", "/url", Some("BLAH"), Seq(KeyedStanza("1", PageStanza("/usr",Seq("end"), false))), Seq.empty, Seq.empty, Seq.empty)
  }

  "render_stanza" must {

    "Display a stanza table" in new Test {
      asDocument(stable(ppm, true)).toString.contains("Stanzas of page '1'") shouldBe true
    }
  }

}
