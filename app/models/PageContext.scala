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

package models

import core.models.ocelot.{Page, LabelCache, Labels}
import core.models.ocelot.stanzas.{VisualStanza,DataInput}

case class PageEvaluationContext(page: Page,
                                 visualStanzas: Seq[VisualStanza],
                                 dataInput: Option[DataInput],
                                 sessionId: String,
                                 pageMapById: Map[String, PageDesc],
                                 processStartUrl: Option[String],
                                 processTitle: ui.Text,
                                 processId: String,
                                 processCode: String,
                                 labels: Labels = LabelCache(),
                                 backLink: Option[String] = None,
                                 answer: Option[String] = None)


case class PageContext(page: ui.Page,
                       visualStanzas: Seq[VisualStanza],
                       dataInput: Option[DataInput],
                       sessionId: String,
                       processStartUrl: Option[String],
                       processTitle: ui.Text,
                       processId: String,
                       processCode: String,
                       labels: Labels = LabelCache(),
                       backLink: Option[String] = None,
                       answer: Option[String] = None)

object PageContext {
  def apply(pec: PageEvaluationContext, page: ui.Page): PageContext = PageContext(pec, page, pec.labels)
  def apply(pec: PageEvaluationContext, page: ui.Page, labels: Labels): PageContext =
    PageContext(
      page,
      pec.visualStanzas,
      pec.dataInput,
      pec.sessionId,
      pec.processStartUrl,
      pec.processTitle,
      pec.processId,
      pec.processCode,
      labels,
      pec.backLink,
      pec.answer
    )
}
