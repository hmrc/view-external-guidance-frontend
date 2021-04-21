/*
 * Copyright 2021 HM Revenue & Customs
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

import core.models.ocelot.{Page, Process}
import core.services.PageBuilder
import core.models.ocelot.errors.GuidanceError
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory

trait MockPageBuilder extends MockFactory {

  val mockPageBuilder: PageBuilder = mock[PageBuilder]

  object MockPageBuilder {

    def buildPage(key: String, process: Process): CallHandler[Either[GuidanceError, Page]] =
      (mockPageBuilder
        .buildPage(_: String, _: Process))
        .expects(key, process)

    def pages(process: Process): CallHandler[Either[List[GuidanceError], Seq[Page]]] =
      (mockPageBuilder
        .pages(_: Process, _: String))
        .expects(process, *)

  }

}
