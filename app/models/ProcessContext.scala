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

import core.models.ocelot.stanzas.Stanza
import core.models.ocelot.{FlowStage, SecuredProcess, Process, Label}
import play.api.libs.json.{Json, OFormat}

case class ProcessContext(process: Process,
                          answers: Map[String, String],
                          labels: Map[String, Label],
                          flowStack: List[FlowStage],
                          continuationPool: Map[String, Stanza],
                          pageMap: Map[String, PageNext],
                          legalPageIds: List[String],
                          backLink: Option[String]) {
  val secure: Boolean = process.flow.get(SecuredProcess.PassPhrasePageId).fold(true){_ =>
    labels.get(SecuredProcess.PassPhraseResponseLabelName).fold(false)(lbl => lbl.english.headOption == process.passPhrase)
  }
}

case class PageNext(id: String, next: List[String])
object PageNext {
  implicit val formats: OFormat[PageNext] = Json.format[PageNext]
}

case class PageDesc(id: String, url: String, next: List[String])
object PageDesc {
  def apply(pn: PageNext, url: String): PageDesc = PageDesc(pn.id, url, pn.next)
  implicit val formats: OFormat[PageDesc] = Json.format[PageDesc]
}
