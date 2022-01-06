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

package views.components

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.Injector
import play.api.i18n.{Messages, MessagesApi, Lang}
import play.api.test.FakeRequest
import org.jsoup.nodes.{Document, Element}
import scala.collection.JavaConverters._
import base.{ViewSpec, ViewFns}
import uk.gov.hmrc.govukfrontend.views.html.components._

class GovukFooterSpec extends ViewSpec with ViewFns with GuiceOneAppPerSuite {

  trait Test {
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    val footerLinks = app.injector.instanceOf[FooterLinks]
    val footer = app.injector.instanceOf[views.html.components.govukFooter]
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))
    implicit val fakeRequest = FakeRequest("GET", "/")
  }

  "rendered english footer" should {

    "contain correct English footer link texts" in new Test {

      val doc: Document = asDocument(footer(Footer(meta = Some(Meta(items = Some(footerLinks.items))))))
      val footerElement: Element = doc.getElementsByTag("footer").first
      val listElement: Element = footerElement.getElementsByTag("ul").first
      val footerItems: List[String] = listElement.getElementsByTag("a").asScala.map(_.text).toList

      footerItems.contains(messages("footer.links.help_page.text")) shouldBe true
      footerItems.contains(messages("footer.links.cookies.text")) shouldBe true
      footerItems.contains(messages("footer.links.accessibility.text")) shouldBe true
      footerItems.contains(messages("footer.links.privacy_policy.text")) shouldBe true
      footerItems.contains(messages("footer.links.terms_and_conditions.text")) shouldBe true
    }

    "contain correct English footer licence texts" in new Test {
      val doc: Document = asDocument(footer(Footer(meta = Some(Meta(items = Some(footerLinks.items))))))
      val footerElement: Element = doc.getElementsByTag("footer").first
      val licence: Element = footerElement.getElementsByClass("govuk-footer__licence-description").first

      licence.text.contains(messages("footer.license.text1")) shouldBe true
      licence.text.contains(messages("footer.license.text2")) shouldBe true
      licence.text.contains(messages("footer.license.text3")) shouldBe true
    }

    "contain correct English copyright text" in new Test {
      val doc: Document = asDocument(footer(Footer(meta = Some(Meta(items = Some(footerLinks.items))))))
      val footerElement: Element = doc.getElementsByTag("footer").first
      val copyright: Element = footerElement.getElementsByClass("govuk-footer__copyright-logo").first
      copyright.text.contains(messages("footer.license.copyright")) shouldBe true
    }
  }
}
