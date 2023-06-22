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
import org.jsoup.nodes.Element
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest
import play.twirl.api.Html
import views.html.admin.render_stanza

import scala.collection.JavaConverters._

class RenderStanzaSpec extends BaseSpec with ViewSpec with ViewFns {

  private trait Test {
    implicit val labels: Labels = LabelCache()
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    implicit val request = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(request)

    val currencyInput = CurrencyInput(Seq("1"), Phrase("Input", "Input"), None, "label", None, false)

  }

  "render_stanza" must {

    "support all Input stanza types" in new Test {
      asDocument(render_stanza(currencyInput, Seq.empty)).toString.contains("help") shouldBe true
    }
    "support PageStanza" in new Test {
      asDocument(render_stanza(PageStanza("/usr",Seq("1"), false), Seq.empty)).toString.contains("url") shouldBe true
    }
    "support Instruction" in new Test {
      asDocument(render_stanza(Instruction(Phrase("Input", "Input"), Seq("1"), None, false), Seq.empty)).toString.contains("text") shouldBe true
    }
    "support all Callout stanza types" in new Test {
      asDocument(render_stanza(ErrorCallout(Phrase("Input", "Input"), Seq("1"), false), Seq.empty)).toString.contains("text") shouldBe true
    }
    "support Calculation" in new Test {
      asDocument(render_stanza(Calculation(Seq("1"), Seq(AddOperation("l","op","r"))), Seq.empty)).toString.contains("next") shouldBe true
    }
    "support ValueStanza" in new Test {
      asDocument(render_stanza(ValueStanza(List(Value(ScalarType, "l", "0")), Seq("1"), false), Seq.empty)).toString.contains("next") shouldBe true
    }
  }
}
