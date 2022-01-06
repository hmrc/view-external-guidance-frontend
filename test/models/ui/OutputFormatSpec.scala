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

package models.ui

import play.api.inject.Injector
import play.api.i18n.{Lang, Messages, MessagesApi}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite

import base.BaseSpec

class OutputFormatSpec extends BaseSpec with GuiceOneAppPerSuite {

  trait Test {

    private def injector: Injector = app.injector

    val messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
  }

  trait EnglishTest extends Test {
    val messages: Messages = messagesApi.preferred(Seq(Lang("en")))
  }

  trait WelshTest extends Test {
    val messages: Messages = messagesApi.preferred(Seq(Lang("cy")))
  }

  "Output formats" should {
    "identify as numeric for numeric formats" in {
      Currency.isNumeric shouldBe true
      CurrencyPoundsOnly.isNumeric shouldBe true
      Number.isNumeric shouldBe true
    }

    "identify as not numeric for non-numeric formats" in {
      Txt.isNumeric shouldBe false
      DateStandard.isNumeric shouldBe false
    }

    "correctly format dates in english" in new EnglishTest {

      DateStandard.asString(Some("01/01/2000"), messages) shouldBe "1 January 2000"
      DateStandard.asString(Some("28/02/1988"), messages) shouldBe "28 February 1988"
      DateStandard.asString(Some("03/03/1900"), messages) shouldBe "3 March 1900"
      DateStandard.asString(Some("4/4/2020"), messages) shouldBe "4 April 2020"
      DateStandard.asString(Some("05/05/2000"), messages) shouldBe "5 May 2000"
      DateStandard.asString(Some("04/06/1950"), messages) shouldBe "4 June 1950"
      DateStandard.asString(Some("7/7/2021"), messages) shouldBe "7 July 2021"
      DateStandard.asString(Some("19/08/2008"), messages) shouldBe "19 August 2008"
      DateStandard.asString(Some("9/9/1999"), messages) shouldBe "9 September 1999"
      DateStandard.asString(Some("10/10/2020"), messages) shouldBe "10 October 2020"
      DateStandard.asString(Some("1/11/2011"), messages) shouldBe "1 November 2011"
      DateStandard.asString(Some("15/12/2014"), messages) shouldBe "15 December 2014"

    }

    "correctly format dates in welsh" in new WelshTest {

      DateStandard.asString(Some("01/01/2000"), messages) shouldBe "1 Ionawr 2000"
      DateStandard.asString(Some("28/02/1988"), messages) shouldBe "28 Chwefror 1988"
      DateStandard.asString(Some("03/03/1900"), messages) shouldBe "3 Mawrth 1900"
      DateStandard.asString(Some("4/4/2020"), messages) shouldBe "4 Ebrill 2020"
      DateStandard.asString(Some("05/05/2000"), messages) shouldBe "5 Mai 2000"
      DateStandard.asString(Some("04/06/1950"), messages) shouldBe "4 Mehefin 1950"
      DateStandard.asString(Some("7/7/2021"), messages) shouldBe "7 Gorffennaf 2021"
      DateStandard.asString(Some("19/08/2008"), messages) shouldBe "19 Awst 2008"
      DateStandard.asString(Some("9/9/1999"), messages) shouldBe "9 Medi 1999"
      DateStandard.asString(Some("10/10/2020"), messages) shouldBe "10 Hydref 2020"
      DateStandard.asString(Some("1/11/2011"), messages) shouldBe "1 Tachwedd 2011"
      DateStandard.asString(Some("15/12/2014"), messages) shouldBe "15 Rhagfyr 2014"

    }
  }
}
