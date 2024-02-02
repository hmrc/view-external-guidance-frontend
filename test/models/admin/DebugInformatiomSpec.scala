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

package models

import base.BaseSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.MessagesApi
import services.SecuredProcessBuilder
import core.models.admin._
import core.models.ocelot._
import core.models.ocelot.stanzas._

class DebugInformatiomSpec extends BaseSpec with GuiceOneAppPerSuite {

  val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val securedProcessBuilder = new SecuredProcessBuilder(messagesApi)

  trait Test extends ProcessJson {
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
  }

  "DebugInformation" must {
    "Build list of DebugLabelRow" in new Test {
      debugInfo.labels.length shouldBe 3
      debugInfo.labels(0) shouldBe DebugLabelRow("A", "Scalar", None, Some("23"))
      debugInfo.labels(1) shouldBe DebugLabelRow("B", "Scalar", None, Some("46"))
      debugInfo.labels(2) shouldBe DebugLabelRow("C", "List", None, Some("1,2,3"))
    }

  }
}
