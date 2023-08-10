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

package views

import base.{ViewFns, ViewSpec}
import core.models.ocelot.{LabelCache, Labels}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest
import play.twirl.api.Html
import views.html.session_blocked

class SessionBlockedSpec extends ViewSpec with ViewFns with GuiceOneAppPerSuite {

  private trait Test {
    implicit val labels: Labels = LabelCache()
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    val session_blocked = injector.instanceOf[session_blocked]
    implicit val request = FakeRequest("GET", "/")
  }

  private trait EnTest extends Test {
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))
  }

  private trait CyTest extends Test {
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))
  }

  "Session blocked Page" must {

    "Should display English by default" in new EnTest {

      val html: Html = session_blocked("processCode", "/guidance/processCode", None)
      val doc = asDocument(html)

      val heading = doc.getElementsByClass("govuk-heading-xl")
      heading.text shouldBe "There has been a problem"

      val startAgain = doc.getElementById("startAgain")
      elementAttrs(startAgain)("href") shouldBe "/guidance/processCode"
    }

    "Should display English with url param lang switch" in new EnTest {

      val html: Html = session_blocked("processCode", "/guidance/processCode", Some("en"))
      val doc = asDocument(html)

      val heading = doc.getElementsByClass("govuk-heading-xl")
      heading.text shouldBe "There has been a problem"

      val startAgain = doc.getElementById("startAgain")
      elementAttrs(startAgain)("href") shouldBe "/guidance/processCode?lang=en"
    }

    "Should display Welsh with url param lang switch" in new CyTest {

      val html: Html = session_blocked("processCode", "/guidance/processCode", Some("cy"))
      val doc = asDocument(html)

      val heading = doc.getElementsByClass("govuk-heading-xl")
      heading.text shouldBe "Mae problem wedi codi"

      val startAgain = doc.getElementById("startAgain")
      elementAttrs(startAgain)("href") shouldBe "/guidance/processCode?lang=cy"
    }

  }

}
