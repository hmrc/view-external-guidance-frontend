/*
 * Copyright 2020 HM Revenue & Customs
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

import models.ocelot.stanzas.{DataInput, VisualStanza}
import models.ocelot.{Page, Labels}
import services.PageRenderer
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory

trait MockPageRenderer extends MockFactory {

  val mockPageRenderer: PageRenderer = mock[PageRenderer]

  object MockPageRenderer {

    def renderPage(page: Page, labels: Labels): CallHandler[(Seq[VisualStanza], Labels, Option[DataInput])] =
      (mockPageRenderer
        .renderPage(_: Page, _: Labels))
        .expects(page, *)

    def renderPagePostSubmit(page: Page, labels: Labels, answer: String): CallHandler[(Option[String], Labels)] =
      (mockPageRenderer
        .renderPagePostSubmit(_: Page, _: Labels, _: String))
        .expects(page, *, answer)

  }

}
