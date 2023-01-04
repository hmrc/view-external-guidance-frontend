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
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest
import play.twirl.api.Html
import views.html.runtime_error_template
import org.jsoup.nodes.Element
import scala.collection.JavaConverters._

class GuidanceErrorSpec extends ViewSpec with ViewFns with GuiceOneAppPerSuite {

  private trait Test {
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    val guidanceErrorPage = injector.instanceOf[runtime_error_template]

    implicit val request = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(request)

    val processCode = "processCode"
    val heading = Messages("guidance.error.heading", processCode)
    val pageTitle = Messages("guidance.error.title", processCode)
  }

  "Guidance Error Page" must {

    "Display the correct error message for a single error" in new Test {
      val errorList: List[String] = List(Messages("guidance.error.nonterminating_loop", 4))
      val solutionsList: List[List[String]] = List(List(Messages("guidance.error.nonterminating_loop.soln")))
      val html: Html = guidanceErrorPage(pageTitle, heading, "123", errorList, solutionsList)
      val doc = asDocument(html)

      val paragraphs: List[String] = doc.getElementsByTag("p").asScala.toList.map(_.text)

      doc.getElementsByTag("h1").text shouldBe heading
      doc.getElementsByTag("h3").text shouldBe Messages("guidance.error.solns_heading")

      paragraphs.size shouldBe 2
      paragraphs(0) shouldBe errorList(0)
      paragraphs(1) shouldBe solutionsList(0)(0)
    }
  }

  "Display the correct error messages for single error and solution" in new Test {
    val errorList: List[String] = List(Messages("guidance.error.unsupported_ui_pattern", 4))
    val solutionsList: List[List[String]] = List(List(Messages("guidance.error.unsupported_ui_pattern.soln"),
                                                      Messages("guidance.error.unsupported_ui_pattern.sol1"),
                                                      Messages("guidance.error.unsupported_ui_pattern.soln2"),
                                                      Messages("guidance.error.unsupported_ui_pattern.soln3"),
                                                      Messages("guidance.error.unsupported_ui_pattern.soln4")))
    val doc = asDocument(guidanceErrorPage(pageTitle, heading, "123", errorList, solutionsList))

    val paragraphs: List[String] = doc.getElementsByTag("p").asScala.toList.map(_.text)

    doc.getElementsByTag("h1").text shouldBe heading
    doc.getElementsByTag("h3").text shouldBe Messages("guidance.error.solns_heading")

    paragraphs(0) shouldBe errorList(0)
    paragraphs(1) shouldBe solutionsList(0)(0)
    paragraphs(2) shouldBe solutionsList(0)(1)
    paragraphs(3) shouldBe solutionsList(0)(2)
    paragraphs(4) shouldBe solutionsList(0)(3)
  }

  "Display the correct error messages for multiple errors and single solution" in new Test {
    val errorList: List[String] = List(Messages("guidance.error.unsupported_operation",2,"DivideOperation","[label:X]","[label:Y]"),
                                       Messages("guidance.error.unsupported_operation",3,"MultiplyOperation","[label:X]","[label:Y]"),
                                       Messages("guidance.error.unsupported_operation",4,"AddOperation","[label:X]","[label:Y]"))
    val solutionsList: List[List[String]] = List(List(Messages("guidance.error.unsupported_operation.soln")))

    val doc = asDocument(guidanceErrorPage(pageTitle, heading, "123", errorList, solutionsList))

    doc.getElementsByTag("h1").text shouldBe heading
    doc.getElementsByTag("h3").text shouldBe Messages("guidance.error.solns_heading")

    val paragraphs: List[String] = doc.getElementsByTag("p").asScala.toList.map(_.text)

    paragraphs(0) shouldBe errorList(0)
    paragraphs(1) shouldBe errorList(1)
    paragraphs(2) shouldBe errorList(2)
    paragraphs(3) shouldBe solutionsList(0)(0)
  }

}
