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

package mocks

import core.models.ocelot.stanzas.{DataInput, VisualStanza}
import core.models.ocelot.{Labels, Page}
import services.{PageRenderer, RenderOutcome}
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import org.scalatest.TestSuite
import play.api.i18n.Messages

trait MockPageRenderer extends TestSuite with MockFactory {
  val mockPageRenderer: PageRenderer = mock[PageRenderer]

  object MockPageRenderer {

    def renderPage(page: Page, labels: Labels): CallHandler[RenderOutcome[(List[VisualStanza], Labels, Option[DataInput])]] =
      (mockPageRenderer
        .renderPage(_: Page, _: Labels)(_: Messages))
        .expects(page, *, *)

    def renderPagePostSubmit(page: Page, labels: Labels, answer: String): CallHandler[RenderOutcome[(Option[String], Labels)]] =
      (mockPageRenderer
        .renderPagePostSubmit(_: Page, _: Labels, _: String)(_: Messages))
        .expects(page, *, answer, *)

  }

}
