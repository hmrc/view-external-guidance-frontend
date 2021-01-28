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

package services

import core.services.PageBuilder
import base.BaseSpec
import core.models.ocelot._
import core.models.ocelot.stanzas.Stanza
import play.api.libs.json._
import play.api.i18n.MessagesApi
import play.api.inject.Injector
import org.scalatestplus.play.guice.GuiceOneAppPerSuite

class SecuredProcessBuilderSpec extends BaseSpec with ProcessJson with GuiceOneAppPerSuite {
  def injector: Injector = app.injector
  val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]

  val meta: Meta = Json.parse(prototypeMetaSection).as[Meta]
  val flow: Map[String, Stanza] = Json.parse(prototypeFlowSection).as[Map[String, Stanza]]
  val phrases: Vector[Phrase] = Json.parse(prototypePhrasesSection).as[Vector[Phrase]]
  val links: Vector[Link] = Json.parse(prototypeLinksSection).as[Vector[Link]]

  val process: Process = prototypeJson.as[Process]
  val passphraseProcess: Process = validOnePageProcessWithPassPhrase.as[Process]
  val protectedProcess: Process = validOnePageProcessWithPassPhrase.as[Process]
  val securedProcessBuilder = new SecuredProcessBuilder(messagesApi)
  val pageBuilder = new PageBuilder()

  "SecuredProcessBuilder" should {
    "create a secured process with an additional passphrase page" in {
      pageBuilder.pages(passphraseProcess, passphraseProcess.startPageId).fold(_ => fail, pages =>
        pages.length shouldBe 1
      )
      val securedProcess = securedProcessBuilder.secureIfRequired(passphraseProcess)
      pageBuilder.pages(securedProcess, securedProcess.startPageId).fold(_ => fail, pages => {
        pages.length shouldBe 2
        pages.find(_.id == SecuredProcess.PassPhrasePageId).fold(fail){ page =>
          page.url.drop(1) shouldBe SecuredProcess.SecuredProcessStartUrl
          page.next.contains(Process.StartStanzaId) shouldBe true
        }
      })
    }

    "create a secured process with an additional entry in phrases section" in {
      passphraseProcess.phrases.length shouldBe 9
      val securedProcess = securedProcessBuilder.secureIfRequired(passphraseProcess)
      securedProcess.phrases.length shouldBe 10
    }

  }

  "Process passphrase" must {
    "detect passphrase when present" in {
      protectedProcess.passPhrase shouldBe Some("A not so memorable phrase")
    }

    "Not detect passphrase when not present" in {
      process.passPhrase shouldBe None
    }

    "return the process start url with an unsecured process from startUrl" in {
      process.startUrl shouldBe Some("/start")
    }

    "return the process start url with a secured process from startUrl" in {
      securedProcessBuilder.secureIfRequired(protectedProcess).startUrl shouldBe Some("/feeling-bad")
    }

    "return StartStanzaId with an unsecured process from startPageId" in {
      process.startPageId shouldBe Process.StartStanzaId
    }

    "return PassPhrasePageId with a secured process from startPageId" in {
      securedProcessBuilder.secureIfRequired(protectedProcess).startPageId shouldBe SecuredProcess.PassPhrasePageId
    }

  }
}