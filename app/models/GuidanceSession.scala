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

package models

import core.models.ocelot.stanzas.Stanza
import core.models.ocelot.{FlowStage, SecuredProcess, Process, Label, RunMode, Published}
import play.api.libs.json.{Json, OFormat}

case class GuidanceSession(process: Process,
                           answers: Map[String, String],
                           labels: Map[String, Label],
                           flowStack: List[FlowStage],
                           continuationPool: Map[String, Stanza],
                           pageMap: Map[String, PageNext],
                           legalPageIds: List[String],
                           currentPageUrl: Option[String],
                           backLink: Option[String],
                           runMode: RunMode,
                           pageHistory: List[PageHistory]) {
  val secure: Boolean = process.flow.get(SecuredProcess.PassPhrasePageId).fold(true)(_ => passphraseResponseValid)
  lazy private val passphraseResponseValid: Boolean = 
    process.encryptedPassPhrase.fold{
      // No encrypted passphrase
      process.passPhrase.fold(true){_ =>
        // Found plaintext passphraser, check plaintext response
        labels.get(SecuredProcess.PassPhraseResponseLabelName).fold(false){lbl => 
          lbl.english.headOption == process.passPhrase
        }
      }
    }{_ => 
      // Found encrypted passphrase, check encrypted response
      labels.get(SecuredProcess.EncryptedPassphraseResponseLabelName).fold(false){lbl => 
        lbl.english.headOption == process.encryptedPassPhrase
      }
    }
}

object GuidanceSession {
  def apply(sp: Session, process: Process, pageMap: Map[String, PageNext]): GuidanceSession =
    GuidanceSession(process,
                    sp.answers,
                    sp.labels,
                    sp.flowStack,
                    sp.continuationPool,
                    pageMap,
                    sp.legalPageIds,
                    sp.pageHistory.reverse.headOption.map(_.url.drop(process.meta.processCode.length)),
                    None,
                    sp.runMode.getOrElse(Published),
                    sp.pageHistory)
}

case class PageNext(id: String, next: List[String] = Nil, linked: List[String] = Nil, title: Option[String] = None, url: Option[String] = None)
object PageNext {
  implicit val formats: OFormat[PageNext] = Json.format[PageNext]
}

case class PageDesc(id: String, url: String, next: List[String] = Nil)
object PageDesc {
  def apply(pn: PageNext, url: String): PageDesc = PageDesc(pn.id, url, pn.next)
  implicit val formats: OFormat[PageDesc] = Json.format[PageDesc]
}
