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

package services

import base.BaseSpec
import core.models.ocelot._
import core.models.ocelot.stanzas._
import mocks.MockAppConfig
import play.api.i18n.{Messages, MessagesApi}
import play.api.libs.json._

class DebugServiceSpec extends BaseSpec with ProcessJson {
  val debugService: DebugService = new DebugService(MockAppConfig)
  val messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
  implicit val messages: Messages = messagesApi.preferred(Seq())
  val meta: Meta = Json.parse(prototypeMetaSection).as[Meta]

  trait Test {
    val pageId1 = Process.StartStanzaId
    val pageId2 = "4"
    val pageId3 = "6"
    val pageId4 = "9"
    val pageId5 = "11"
    val pageId6 = "14"
    val pageId7 = "17"
    val pageIds = Seq(pageId1, pageId2, pageId3, pageId4, pageId5, pageId6, pageId7)

    val answers =
      Seq(Phrase(Vector("Some Text 1", "Welsh: Some Text 1")),
          Phrase(Vector("Some Text 2", "Welsh: Some Text 2")),
          Phrase(Vector("Some Text 3", "Welsh: Some Text 3")))

    val answerDestinations = Seq("4", "5", "6")
    val questionPhrase: Phrase = Phrase(Vector("Some Text [label:X]", "Welsh: Some Text [label:X]"))
    val questionHintString = "A hint!!"
    val questionWithHintPhrase: Phrase = Phrase(Vector(s"Some Text[hint:${questionHintString}]", s"Welsh: Some Text[hint:${questionHintString}]"))
  }

  "DebugService" must {

    "Detertmine page title" in new Test {
      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(ScalarType, "X", "9")), Seq("2"), true)),
                        KeyedStanza("2", Instruction(Phrase("Hello", "Hello"), Seq("3"), None, false)),
                        KeyedStanza("3", ValueStanza(List(Value(ScalarType, "X", "9")), Seq("1"), true)),
                        KeyedStanza("4", Question(questionPhrase, answers, Seq("23","23","23"), None, false)),
                        KeyedStanza("end", EndStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)

      debugService.pageTitle(page) shouldBe Some(questionPhrase.english)
    }
  }
}
