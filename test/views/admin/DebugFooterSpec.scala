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

package views.html.admin

import base.{BaseSpec, ViewFns, ViewSpec}
import core.models.ocelot._
import core.models.ocelot.stanzas._
import play.api.i18n.{Messages, MessagesApi}
import play.api.test.FakeRequest
import core.models.admin._
import views.html.admin.debug_footer_tabs

class DebugFooterSpec extends BaseSpec with ViewSpec with ViewFns {

  private trait Test {

    implicit val labels: Labels = LabelCache()
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    val instruction: Instruction = Instruction(Phrase("Instruction", "Instruction"), Seq("3"), None, false)
    val questionStanza: Question = Question(Phrase("Which?","Which?"), Seq(Phrase("yes","yes"),Phrase("no","no")), Seq("4","5"), None, false)

    val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                                        KeyedStanza("1", instruction),
                                        KeyedStanza("3", questionStanza))


    val processPageStructure: ProcessPageStructure = ProcessPageStructure(
      "id",
      "/start", 
      Some("The title"),
      stanzas,
      Seq(LinkedPage("4", "/blah", Some("Blah blah")), LinkedPage("5", "/Otherblah", Some("Other Blah blah"))),
      Seq(),
      Seq("start")
    )
    val debugInfo = DebugInformation(processPageStructure, labels, labels.update("A", "23").update("B", "46").updateList("C", List("1","2","3")))

    val debugFooter = injector.instanceOf[debug_footer_tabs]
    val pageStructure = injector.instanceOf[page_structure]
    implicit val request = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(request)

  }

  "debug_footer_tabs" must {
    "displays first url" in new Test {
      asDocument(debugFooter(debugInfo)).toString.contains("/start") shouldBe true
    }
    "displays second url" in new Test {
      asDocument(debugFooter(debugInfo)).toString.contains("/Otherblah") shouldBe true
    }
    "support Question" in new Test {
      asDocument(debugFooter(debugInfo)).toString.contains("Question") shouldBe true
    }
  }
  
  "page_structure" must {
    "Contain Question" in new Test {
      asDocument(pageStructure(processPageStructure)).toString.contains("Question") shouldBe true
    }
  }
}
