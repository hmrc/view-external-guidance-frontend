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

import core.models.ocelot._
import base.BaseSpec
import play.api.i18n.MessagesApi
import play.api.inject.Injector
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import services.SecuredProcessBuilder

class ProcessContextSpec extends BaseSpec with GuiceOneAppPerSuite {

  def injector: Injector = app.injector
  val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val securedProcessBuilder = new SecuredProcessBuilder(messagesApi)

  trait Test extends ProcessJson {
    val securedProcess: Process =
      securedProcessBuilder.secureIfRequired(validOnePageProcessWithPassPhrase.as[Process])
    val labelsWithPassPhrase =
      Map(SecuredProcess.PassPhraseResponseLabelName -> ScalarLabel(SecuredProcess.PassPhraseResponseLabelName,
                                                                    List(securedProcess.passPhrase.getOrElse(""))))
  }

  "ProcessContext" must {
    "Report secure for a standard Process" in new Test {
      val pc: ProcessContext =
        ProcessContext(validOnePageJson.as[Process],Map("/start" -> "0"),Map(),Nil,Map(),Map(),Nil,None)
      pc.secure shouldBe true
    }

    "Report not secure for a passphrase protected Process with no passphrase stored" in new Test {
      val passPhrasePc: ProcessContext =
        ProcessContext(securedProcess,Map("/start" -> "0"),Map(),Nil,Map(),Map(),Nil,None)
      passPhrasePc.secure shouldBe false
    }

    "Report secure for a passphrase protected Process with stored passphrase" in new Test {
      val securePassPhrasePc: ProcessContext =
        ProcessContext(securedProcess,Map("/start" -> "0"),labelsWithPassPhrase,Nil,Map(),Map(),Nil,None)
      securePassPhrasePc.secure shouldBe true
    }
  }
}
