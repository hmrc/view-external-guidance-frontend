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

package views

import base.{ViewFns, ViewSpec}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest
import play.twirl.api.Html
import views.html.runtime_error_template

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
      val errorList: List[String] = List("NonTerminatingPageError: Guidance contains non-terminating loop which includes stanza 4")
      val solutionsList: List[List[String]] = List(List("NonTerminatingPageError: Ensure the page terminates for all input values (if any)"))
      val html: Html = guidanceErrorPage(pageTitle, heading, "123", errorList, solutionsList)
      val doc = asDocument(html)

      val errorParagraph = doc.getElementsByTag("p").text
      val solutionParagraph = doc.getElementsByTag("p").text

      errorParagraph contains "NonTerminatingPageError: Guidance contains non-terminating loop which includes stanza 4"
      solutionParagraph contains "UnsupportedOperationError: Typically this error occurs where one or both operands are references to labels which have not been assigned a value."

    }
  }

  "Display the correct error message for a single error with multiple solutions" in new Test {
    val errorList: List[String] = List("UnsupportedUiPatternError: Unrecognised RowStanza UI pattern including stanza ''{0}''")
    val solutionsList: List[List[String]] = List(List("UnsupportedUiPatternError: RowStanzas can be used as part of a GDS \"Summary List\", \"Table\" or \"Check you answers\" pattern.",
    "Pattern rules :-",
    "1. Check your answers page. Three columns, column 1 cells must not be bold and every column 3 is a link.",
    "2. Summary list. Two columns and column 1 cells must not be bold",
    "3. Table. At least one column and two rows. The cells of row 1 are headings, all of which must be bold. The Row stanzas which make up the table must be stacked to a Callout of type SubSection which will become the overall title of the table"))
    val html: Html = guidanceErrorPage(pageTitle, heading, "123", errorList, solutionsList)
    val doc = asDocument(html)

    val errorParagraph = doc.getElementsByTag("p").text
    val solutionParagraph = doc.getElementsByTag("p").text

    errorParagraph contains "NonTerminatingPageError: Guidance contains non-terminating loop which includes stanza 4"
    solutionParagraph contains "Pattern rules :-"
    solutionParagraph contains "1. Check your answers page. Three columns, column 1 cells must not be bold and every column 3 is a link."
    solutionParagraph contains "2. Summary list. Two columns and column 1 cells must not be bold"
    solutionParagraph contains "3. Table. At least one column and two rows. The cells of row 1 are headings, all of which must be bold. The Row stanzas which make up the table must be stacked to a Callout of type SubSection which will become the overall title of the table\""
  }

}
