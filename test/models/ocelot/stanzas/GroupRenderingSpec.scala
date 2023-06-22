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

package models.ocelot.stanzas

import base.{BaseSpec, EnglishLanguage, WelshLanguage}
import core.models.ocelot._
import core.models.ocelot.stanzas._
import play.api.i18n.{Messages, MessagesApi}
import services._

class GroupRenderingSpec extends BaseSpec {

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
    "expand in English text of ImportantGroup" in new EnglishTest {
      val ic = ImportantCallout(phrase, Seq("1"), false)
      val expIc = ImportantCallout(expEn, Seq("1"), false)
      ImportantGroup(Seq("1"), Seq(ic, ic), false).rendered(TextBuilder.expandLabels(labels)) shouldBe ImportantGroup(Seq("1"), Seq(expIc, expIc), false)
    }

    "expand in English text of InstructionGroup" in new EnglishTest {
      val i = Instruction(phrase, Seq("1"), None, false)
      val expI = Instruction(expEn, Seq("1"), None, false)
      InstructionGroup(Seq("1"), Seq(i, i), false).rendered(TextBuilder.expandLabels(labels)) shouldBe InstructionGroup(Seq("1"), Seq(expI, expI), false)
    }

    "expand in English text of NoteGroup" in new EnglishTest {
      val n = NoteCallout(phrase, Seq("1"), false)
      val expN = NoteCallout(expEn, Seq("1"), false)
      NoteGroup(Seq("1"), Seq(n, n), false).rendered(TextBuilder.expandLabels(labels)) shouldBe NoteGroup(Seq("1"), Seq(expN, expN), false)
    }

    "expand in English text of NumberedCircleList" in new EnglishTest {
      val cli = NumberedCircleListItemCallout(phrase, Seq("1"), false)
      val expCli = NumberedCircleListItemCallout(expEn, Seq("1"), false)
      NumberedCircleList(Seq("1"), Seq(cli, cli), false).rendered(TextBuilder.expandLabels(labels)) shouldBe NumberedCircleList(Seq("1"), Seq(expCli, expCli), false)
    }

    "expand in English text of NumberedList" in new EnglishTest {
      val li = NumberedListItemCallout(phrase, Seq("1"), false)
      val expLi = NumberedListItemCallout(expEn, Seq("1"), false)
      NumberedList(Seq("1"), Seq(li, li), false).rendered(TextBuilder.expandLabels(labels)) shouldBe NumberedList(Seq("1"), Seq(expLi, expLi), false)
    }

    "expand in English text of RequiredErrorGroup" in new EnglishTest {
      val err = ErrorCallout(phrase, Seq("1"), false)
      val expErr = ErrorCallout(expEn, Seq("1"), false)
      RequiredErrorGroup(Seq("1"), Seq(err, err), false).rendered(TextBuilder.expandLabels(labels)) shouldBe RequiredErrorGroup(Seq("1"), Seq(expErr, expErr), false)
    }

    "expand in English text of YourCallGroup" in new EnglishTest {
      val yc = YourCallCallout(phrase, Seq("1"), false)
      val expYc = YourCallCallout(expEn, Seq("1"), false)
      YourCallGroup(Seq("1"), Seq(yc, yc), false).rendered(TextBuilder.expandLabels(labels)) shouldBe YourCallGroup(Seq("1"), Seq(expYc, expYc), false)
    }

    "expand in English text of RowGroup" in new EnglishTest {
      val rw = Row(Seq(phrase, phrase), Seq("1"))
      val expRW = Row(Seq(expEn, expEn), Seq("1"))
      RowGroup(Seq("1"), Seq(rw, rw), false).rendered(TextBuilder.expandLabels(labels)) shouldBe RowGroup(Seq("1"), Seq(expRW, expRW), false)
    }

    "expand in English text of StackedGroup" in new EnglishTest {
      val i = Instruction(phrase, Seq("1"), None, false)
      val expI = Instruction(expEn, Seq("1"), None, false)
      val ni = NumberInput(Seq("1"), phrase, Some(phrase),"label", Some(phrase), false)
      val expNi = NumberInput(Seq("1"), expEn, Some(expEn),"label", Some(expEn), false)
      val q = Question(phrase, Seq(phrase, phrase), Seq("1"), None, false)
      val expQ = Question(expEn, Seq(expEn, expEn), Seq("1"), None, false)
      val yc = YourCallCallout(phrase, Seq("1"), false)
      val expYc = YourCallCallout(expEn, Seq("1"), false)
      StackedGroup(Seq("1"), Seq(i, ni, q, yc), false).rendered(TextBuilder.expandLabels(labels)) shouldBe StackedGroup(Seq("1"), Seq(expI,expNi,expQ,expYc), false)
    }

    "expand in Welsh text of ImportantGroup" in new WelshTest {
      val ic = ImportantCallout(phrase, Seq("1"), false)
      val expIc = ImportantCallout(expCy, Seq("1"), false)
      ImportantGroup(Seq("1"), Seq(ic, ic), false).rendered(TextBuilder.expandLabels(labels)) shouldBe ImportantGroup(Seq("1"), Seq(expIc, expIc), false)
    }

    "expand in Welsh text of InstructionGroup" in new WelshTest {
      val i = Instruction(phrase, Seq("1"), None, false)
      val expI = Instruction(expCy, Seq("1"), None, false)
      InstructionGroup(Seq("1"), Seq(i, i), false).rendered(TextBuilder.expandLabels(labels)) shouldBe InstructionGroup(Seq("1"), Seq(expI, expI), false)
    }

    "expand in Welsh text of NoteGroup" in new WelshTest {
      val n = NoteCallout(phrase, Seq("1"), false)
      val expN = NoteCallout(expCy, Seq("1"), false)
      NoteGroup(Seq("1"), Seq(n, n), false).rendered(TextBuilder.expandLabels(labels)) shouldBe NoteGroup(Seq("1"), Seq(expN, expN), false)
    }

    "expand in Welsh text of NumberedCircleList" in new WelshTest {
      val cli = NumberedCircleListItemCallout(phrase, Seq("1"), false)
      val expCli = NumberedCircleListItemCallout(expCy, Seq("1"), false)
      NumberedCircleList(Seq("1"), Seq(cli, cli), false).rendered(TextBuilder.expandLabels(labels)) shouldBe NumberedCircleList(Seq("1"), Seq(expCli, expCli), false)
    }

    "expand in Welsh text of NumberedList" in new WelshTest {
      val li = NumberedListItemCallout(phrase, Seq("1"), false)
      val expLi = NumberedListItemCallout(expCy, Seq("1"), false)
      NumberedList(Seq("1"), Seq(li, li), false).rendered(TextBuilder.expandLabels(labels)) shouldBe NumberedList(Seq("1"), Seq(expLi, expLi), false)
    }

    "expand in Welsh text of RequiredErrorGroup" in new WelshTest {
      val err = ErrorCallout(phrase, Seq("1"), false)
      val expErr = ErrorCallout(expCy, Seq("1"), false)
      RequiredErrorGroup(Seq("1"), Seq(err, err), false).rendered(TextBuilder.expandLabels(labels)) shouldBe RequiredErrorGroup(Seq("1"), Seq(expErr, expErr), false)
    }

    "expand in Welsh text of YourCallGroup" in new WelshTest {
      val yc = YourCallCallout(phrase, Seq("1"), false)
      val expYc = YourCallCallout(expCy, Seq("1"), false)
      YourCallGroup(Seq("1"), Seq(yc, yc), false).rendered(TextBuilder.expandLabels(labels)) shouldBe YourCallGroup(Seq("1"), Seq(expYc, expYc), false)
    }

    "expand in Welsh text of RowGroup" in new WelshTest {
      val rw = Row(Seq(phrase, phrase), Seq("1"))
      val expRW = Row(Seq(expCy, expCy), Seq("1"))
      RowGroup(Seq("1"), Seq(rw, rw), false).rendered(TextBuilder.expandLabels(labels)) shouldBe RowGroup(Seq("1"), Seq(expRW, expRW), false)
    }

    "expand in Welsh text of StackedGroup" in new WelshTest {
      val i = Instruction(phrase, Seq("1"), None, false)
      val expI = Instruction(expCy, Seq("1"), None, false)
      val ni = NumberInput(Seq("1"), phrase, Some(phrase),"label", Some(phrase), false)
      val expNi = NumberInput(Seq("1"), expCy, Some(expCy),"label", Some(expCy), false)
      val q = Question(phrase, Seq(phrase, phrase), Seq("1"), None, false)
      val expQ = Question(expCy, Seq(expCy, expCy), Seq("1"), None, false)
      val yc = YourCallCallout(phrase, Seq("1"), false)
      val expYc = YourCallCallout(expCy, Seq("1"), false)
      StackedGroup(Seq("1"), Seq(i, ni, q, yc), false).rendered(TextBuilder.expandLabels(labels)) shouldBe StackedGroup(Seq("1"), Seq(expI,expNi,expQ,expYc), false)
    }

  }
}
