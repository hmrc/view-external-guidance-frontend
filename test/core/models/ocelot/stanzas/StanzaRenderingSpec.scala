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

package core.models.ocelot.stanzas

import base.{WelshLanguage, BaseSpec, EnglishLanguage}
import core.models.ocelot._
import services._
import play.api.inject.Injector
import play.api.i18n.{Messages, MessagesApi}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite

class StanzaRenderingSpec extends BaseSpec with GuiceOneAppPerSuite {

  private def injector: Injector = app.injector
  val messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

  trait EnglishTest extends EnglishLanguage{
    implicit val messages: Messages = messagesApi.preferred(Seq(lang))
  }

  trait WelshTest extends WelshLanguage {
    implicit val messages: Messages = messagesApi.preferred(Seq(lang))
  }

  val labels: Labels = LabelCache(List(ScalarLabel("X", List("3.56"), List("3.56"))))

  val unexpandedText = "X values [label:X], [label:X:currency], [label:X:currencyPoundsOnly]"
  val expandedText = "X values 3.56, £3.56, £3"
  val phrase = Phrase(unexpandedText, unexpandedText)
  val expEn = Phrase(expandedText,unexpandedText)
  val expCy = Phrase(unexpandedText,expandedText)

  "Label references" must {
    "expand in English text of Instructions" in new EnglishTest {
      Instruction(phrase, Seq("1"), None, false).rendered(TextBuilder.expandLabels(labels)) shouldBe Instruction(expEn, Seq("1"), None, false)
    }

    "expand in English text of Callouts" in new EnglishTest {
      TitleCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe TitleCallout(expEn, Seq("1"), false)
      SubTitleCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe SubTitleCallout(expEn, Seq("1"), false)
      SectionCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe SectionCallout(expEn, Seq("1"), false)
      SubSectionCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe SubSectionCallout(expEn, Seq("1"), false)
      LedeCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe LedeCallout(expEn, Seq("1"), false)
      ErrorCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe ErrorCallout(expEn, Seq("1"), false)
      ValueErrorCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe ValueErrorCallout(expEn, Seq("1"), false)
      TypeErrorCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe TypeErrorCallout(expEn, Seq("1"), false)
      ImportantCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe ImportantCallout(expEn, Seq("1"), false)
      YourCallCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe YourCallCallout(expEn, Seq("1"), false)
      NumberedListItemCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe NumberedListItemCallout(expEn, Seq("1"), false)
      NumberedCircleListItemCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe NumberedCircleListItemCallout(expEn, Seq("1"), false)
      NoteCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe NoteCallout(expEn, Seq("1"), false)
    }

    "expand in English text of Inputs" in new EnglishTest {
      NumberInput(Seq("1"), phrase, Some(phrase),"label", Some(phrase), false).rendered(TextBuilder.expandLabels(labels)) shouldBe
        NumberInput(Seq("1"), expEn, Some(expEn),"label", Some(expEn), false)
      TextInput(Seq("1"), phrase, Some(phrase),"label", Some(phrase), false).rendered(TextBuilder.expandLabels(labels)) shouldBe
        TextInput(Seq("1"), expEn, Some(expEn),"label", Some(expEn), false)
      CurrencyInput(Seq("1"), phrase, Some(phrase),"label", Some(phrase), false).rendered(TextBuilder.expandLabels(labels)) shouldBe
        CurrencyInput(Seq("1"), expEn, Some(expEn),"label", Some(expEn), false)
      CurrencyPoundsOnlyInput(Seq("1"), phrase, Some(phrase),"label", Some(phrase), false).rendered(TextBuilder.expandLabels(labels)) shouldBe
        CurrencyPoundsOnlyInput(Seq("1"), expEn, Some(expEn),"label", Some(expEn), false)
      DateInput(Seq("1"), phrase, Some(phrase),"label", Some(phrase), false).rendered(TextBuilder.expandLabels(labels)) shouldBe
        DateInput(Seq("1"), expEn, Some(expEn),"label", Some(expEn), false)
    }

    "expand in English text of Rows" in new EnglishTest {
      Row(Seq(phrase, phrase), Seq("1")).rendered(TextBuilder.expandLabels(labels)) shouldBe Row(Seq(expEn, expEn), Seq("1"))
    }

    "expand in English text of Questions" in new EnglishTest {
      Question(phrase, Seq(phrase, phrase), Seq("1"), None, false).rendered(TextBuilder.expandLabels(labels)) shouldBe Question(expEn, Seq(expEn, expEn), Seq("1"), None, false)
    }

    "expand in English text of Sequences" in new EnglishTest {
      Sequence(phrase, Seq("1"), Seq(phrase, phrase), Some(phrase), None, false).rendered(TextBuilder.expandLabels(labels)) shouldBe Sequence(expEn, Seq("1"), Seq(expEn, expEn), Some(expEn), None, false)
    }

    "expand in Welsh text of Instructions" in new WelshTest {
      Instruction(phrase, Seq("1"), None, false).rendered(TextBuilder.expandLabels(labels)) shouldBe Instruction(expCy, Seq("1"), None, false)
    }

    "expand in Welsh text of Callouts" in new WelshTest {
      TitleCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe TitleCallout(expCy, Seq("1"), false)
      SubTitleCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe SubTitleCallout(expCy, Seq("1"), false)
      SectionCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe SectionCallout(expCy, Seq("1"), false)
      SubSectionCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe SubSectionCallout(expCy, Seq("1"), false)
      LedeCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe LedeCallout(expCy, Seq("1"), false)
      ErrorCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe ErrorCallout(expCy, Seq("1"), false)
      ValueErrorCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe ValueErrorCallout(expCy, Seq("1"), false)
      TypeErrorCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe TypeErrorCallout(expCy, Seq("1"), false)
      ImportantCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe ImportantCallout(expCy, Seq("1"), false)
      YourCallCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe YourCallCallout(expCy, Seq("1"), false)
      NumberedListItemCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe NumberedListItemCallout(expCy, Seq("1"), false)
      NumberedCircleListItemCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe NumberedCircleListItemCallout(expCy, Seq("1"), false)
      NoteCallout(phrase, Seq("1"), false).rendered(TextBuilder.expandLabels(labels)) shouldBe NoteCallout(expCy, Seq("1"), false)
    }

    "expand in Welsh text of Inputs" in new WelshTest {
      NumberInput(Seq("1"), phrase, Some(phrase),"label", Some(phrase), false).rendered(TextBuilder.expandLabels(labels)) shouldBe
        NumberInput(Seq("1"), expCy, Some(expCy),"label", Some(expCy), false)
      TextInput(Seq("1"), phrase, Some(phrase),"label", Some(phrase), false).rendered(TextBuilder.expandLabels(labels)) shouldBe
        TextInput(Seq("1"), expCy, Some(expCy),"label", Some(expCy), false)
      CurrencyInput(Seq("1"), phrase, Some(phrase),"label", Some(phrase), false).rendered(TextBuilder.expandLabels(labels)) shouldBe
        CurrencyInput(Seq("1"), expCy, Some(expCy),"label", Some(expCy), false)
      CurrencyPoundsOnlyInput(Seq("1"), phrase, Some(phrase),"label", Some(phrase), false).rendered(TextBuilder.expandLabels(labels)) shouldBe
        CurrencyPoundsOnlyInput(Seq("1"), expCy, Some(expCy),"label", Some(expCy), false)
      DateInput(Seq("1"), phrase, Some(phrase),"label", Some(phrase), false).rendered(TextBuilder.expandLabels(labels)) shouldBe
        DateInput(Seq("1"), expCy, Some(expCy),"label", Some(expCy), false)
    }

    "expand in Welsh text of Rows" in new WelshTest {
      Row(Seq(phrase, phrase), Seq("1")).rendered(TextBuilder.expandLabels(labels)) shouldBe Row(Seq(expCy, expCy), Seq("1"))
    }

    "expand in Welsh text of Questions" in new WelshTest {
      Question(phrase, Seq(phrase, phrase), Seq("1"), None, false).rendered(TextBuilder.expandLabels(labels)) shouldBe Question(expCy, Seq(expCy, expCy), Seq("1"), None, false)
    }

    "expand in Welsh text of Sequences" in new WelshTest {
      Sequence(phrase, Seq("1"), Seq(phrase, phrase), Some(phrase), None, false).rendered(TextBuilder.expandLabels(labels)) shouldBe Sequence(expCy, Seq("1"), Seq(expCy, expCy), Some(expCy), None, false)
    }

  }
}
