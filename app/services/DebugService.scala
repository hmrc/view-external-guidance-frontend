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

package services

import play.api.Logging
import config.AppConfig
import javax.inject.{Inject, Singleton}
import models.admin._
import models.PageNext
import core.models.ocelot.stanzas._
import core.models.ocelot.{Page, SecuredProcess}

@Singleton
class DebugService @Inject() (appConfig: AppConfig) extends Logging {

  def mapPage(page: Page, pageMap: Map[String, PageNext]): ProcessPageStructure = {
    val pageNextById: Map[String, PageNext] = pageMap.map{case (url, pn) => (pn.id, pn)}
    val nexts = page.next.distinct.map(n => LinkedPage(n, pageNextById(n).url.getOrElse(""), pageNextById(n).title))
    val linked = page.linked.distinct.map(l => LinkedPage(l, pageNextById(l).url.getOrElse(""), pageNextById(l).title))
    val linkedFrom = pageMap.values
                            .filter(p => p.linked.contains(page.id) || p.next.contains(page.id))
                            .map(_.id)
                            .filterNot(_.equals(SecuredProcess.PassPhrasePageId)).toSeq
    ProcessPageStructure(page.id, page.url, pageTitle(page), page.keyedStanzas, nexts, linked, linkedFrom)
  }

  def pageTitle(page: Page): Option[String] =
    page.stanzas.collectFirst{
      case i: Input => i.name.english
      case i: Question => i.text.english
      case i: Sequence => i.text.english
      case c: TitleCallout => c.text.english
      case yc: YourCallCallout => yc.text.english
    }

}
