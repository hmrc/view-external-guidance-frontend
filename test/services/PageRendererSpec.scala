/*
 * Copyright 2020 HM Revenue & Customs
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

import base.BaseSpec
import models.ocelot.stanzas._
import models.ocelot._
import play.api.libs.json._
import utils.StanzaHelper


class PageRendererSpec extends BaseSpec with ProcessJson with StanzaHelper {

  // Define instance of class used in testing
  val pageBuilder = new PageBuilder()
  val renderer: PageRenderer = new PageRenderer()

  val meta: Meta = Json.parse(prototypeMetaSection).as[Meta]

  case object DummyStanza extends Stanza {
    override val next: Seq[String] = Seq("1")
  }

  trait Test {
    val pageId1 = Process.StartStanzaId
    val pageId2 = "4"
    val pageId3 = "6"
    val pageId4 = "9"
    val pageId5 = "11"
    val pageId6 = "14"
    val pageId7 = "17"
    val pageIds = Seq(pageId1, pageId2, pageId3, pageId4, pageId5, pageId6, pageId7)

    private val simpleFlow = Map(
      pageId1 -> PageStanza("/start", Seq("1"), false),
      "1" -> InstructionStanza(3, Seq("2"), None, false),
      "2" -> QuestionStanza(1, Seq(2, 1), Seq(pageId2, pageId4), None, false),
      pageId2 -> PageStanza("/this4", Seq("5"), false),
      "5" -> InstructionStanza(1, Seq("end"), Some(2), false),
      pageId3 -> PageStanza("/this6", Seq("7"), false),
      "7" -> InstructionStanza(2, Seq("8"), None, false),
      "8" -> QuestionStanza(1, Seq(2, 3), Seq(pageId4, pageId6), None, false),
      pageId4 -> PageStanza("/this9", Seq("16"), false),
      "16" -> InstructionStanza(3, Seq("10"), None, false),
      "10" -> InstructionStanza(2, Seq("end"), None, false),
      pageId5 -> PageStanza("/this11", Seq("12"), false),
      "12" -> InstructionStanza(0, Seq("13"), None, false),
      "13" -> QuestionStanza(1, Seq(2, 3), Seq(pageId6, pageId2), None, false),
      pageId6 -> PageStanza("/this14", Seq("15"), false),
      "15" -> InstructionStanza(0, Seq("end"), None, false),
      pageId7 -> PageStanza("/this15", Seq("18"), false),
      "18" -> InstructionStanza(0, Seq("end"), None, false),
      "end" -> EndStanza
    )

    private val phrases = Vector[Phrase](
      Phrase(Vector("Some Text", "Welsh, Some Text")),
      Phrase(Vector(s"Some Text1 [link:Link to stanza 17:$pageId7]", s"Welsh, Some Text1 [link:Link to stanza 17:$pageId7]")),
      Phrase(Vector(s"Some [link:PageId3:$pageId3] Text2", s"Welsh, Some [link:PageId3:$pageId3] Text2")),
      Phrase(Vector(s"Some [link:Link to stanza 11:$pageId5] Text3", s"Welsh, Some [link:Link to stanza 11:$pageId5] Text3"))
    )

    private val links = Vector(Link(0, pageId3, "", false), Link(1, pageId6, "", false), Link(2, Process.StartStanzaId, "Back to the start", false))

    val processWithLinks = Process(metaSection, simpleFlow, phrases, links)
    val answers =
      Seq(Phrase(Vector("Some Text 1", "Welsh, Some Text 1")),
          Phrase(Vector("Some Text 2", "Welsh, Some Text 2")),
          Phrase(Vector("Some Text 3", "Welsh, Some Text 3")))

    val answerDestinations = Seq("4", "5", "6")
    val questionPhrase: Phrase = Phrase(Vector("Some Text", "Welsh, Some Text"))
    val questionHintString = "A hint!!"
    val questionWithHintPhrase: Phrase = Phrase(Vector(s"Some Text[hint:${questionHintString}]", s"Welsh, Some Text[hint:${questionHintString}]"))

    val question: models.ocelot.stanzas.Question = Question(questionPhrase, answers, answerDestinations, None, false)
  }

  "PageRenderer" must {
    "Determine the correct sequence of stanzas within a page with no user input" in new Test {
      val instructionStanza = InstructionStanza(3, Seq("5"), None, false)
      val callout1 = Callout(Error, Phrase(Vector("Some Text", "Welsh, Some Text")), Seq("3"), false)
      val callout2 = Callout(Section, Phrase(Vector("Some Text", "Welsh, Some Text")), Seq("4"), false)

      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", callout1),
                        KeyedStanza("3", callout2),
                        KeyedStanza("4", instructionStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, Seq("5"))

      val (visualStanzas, labels, dataInput) = renderer.renderPage(page, LabelCache())

      visualStanzas shouldBe List(callout1, callout2, instructionStanza)

      dataInput shouldBe None

      labels.updatedLabels.keys.toList.length shouldBe 0
    }

    "Determine the correct sequence of stanzas within the final page of guidance" in new Test {
      val instructionStanza = InstructionStanza(3, Seq("5"), None, false)
      val callout1 = Callout(Error, Phrase(Vector("Some Text", "Welsh, Some Text")), Seq("3"), false)
      val callout2 = Callout(Section, Phrase(Vector("Some Text", "Welsh, Some Text")), Seq("4"), false)

      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", callout1),
                        KeyedStanza("3", callout2),
                        KeyedStanza("4", instructionStanza),
                        KeyedStanza("5", EndStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, Seq("5"))

      val (visualStanzas, labels, dataInput) = renderer.renderPage(page, LabelCache())

      visualStanzas shouldBe List(callout1, callout2, instructionStanza, EndStanza)

      dataInput shouldBe None

      labels.updatedLabels.keys.toList.length shouldBe 0
    }

    "Determine the correct sequence of stanzas within a Question page" in new Test {
      val instructionStanza = InstructionStanza(3, Seq("3"), None, false)
      val questionStanza = Question(questionPhrase, answers, answerDestinations, None, false)
      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(Scalar, "X", "4")), Seq("22"), true)),
                        KeyedStanza("22", Choice(ChoiceStanza(Seq("2","3"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("2", instructionStanza),
                        KeyedStanza("3", questionStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)

      val (visualStanzas, labels, dataInput) = renderer.renderPage(page, LabelCache())
      visualStanzas shouldBe List(instructionStanza, questionStanza)

      dataInput shouldBe Some(questionStanza)

      labels.updatedLabels.keys.toList.length shouldBe 1
    }

    "Determine the correct sequence of stanzas within a Question page involving Choice" in new Test {
      val instructionStanza = InstructionStanza(3, Seq("3"), None, false)
      val questionStanza = Question(questionPhrase, answers, answerDestinations, None, false)
      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(Scalar, "X", "9")), Seq("22"), true)),
                        KeyedStanza("22", Choice(ChoiceStanza(Seq("2","3"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("2", instructionStanza),
                        KeyedStanza("3", questionStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)

      val (visualStanzas, labels, dataInput) = renderer.renderPage(page, LabelCache())
      visualStanzas shouldBe List(questionStanza)
      dataInput shouldBe Some(questionStanza)
      labels.updatedLabels.keys.toList.length shouldBe 1
    }

    "Evaluate the user input stanza to determine the id of the next page" in new Test {

      val instructionStanza = InstructionStanza(3, Seq("3"), None, false)
      val questionStanza = Question(questionPhrase, answers, answerDestinations, None, false)
      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(Scalar, "X", "9")), Seq("22"), true)),
                        KeyedStanza("22", Choice(ChoiceStanza(Seq("2","3"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("2", instructionStanza),
                        KeyedStanza("3", questionStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)
      val labels = LabelCache()

      renderer.renderPagePostSubmit(page, labels, "0").fold(fail){ case (next, newLabels) =>
        next shouldBe answerDestinations(0)
        newLabels.updatedLabels shouldBe Map("X" -> Label("X",Some("9"),None))
      }

      renderer.renderPagePostSubmit(page, labels, "1").fold(fail){ case (next, newLabels) =>
        next shouldBe answerDestinations(1)
        newLabels.updatedLabels shouldBe Map("X" -> Label("X",Some("9"),None))
      }

      renderer.renderPagePostSubmit(page, labels, "2").fold(fail){ case (next, newLabels) =>
        next shouldBe answerDestinations(2)
        newLabels.updatedLabels shouldBe Map("X" -> Label("X",Some("9"),None))
      }

      renderer.renderPagePostSubmit(page, labels, "0").fold(fail){ case (next, newLabels) =>
        next shouldBe answerDestinations(0)
        newLabels.updatedLabels shouldBe Map("X" -> Label("X",Some("9"),None))
      }

      renderer.renderPagePostSubmit(page, labels, "1").fold(fail){ case (next, newLabels) =>
        next shouldBe answerDestinations(1)
        newLabels.updatedLabels shouldBe Map("X" -> Label("X",Some("9"),None))
      }

      renderer.renderPagePostSubmit(page, labels, "2").fold(fail){ case (next, newLabels) =>
        next shouldBe answerDestinations(2)
        newLabels.updatedLabels shouldBe Map("X" -> Label("X",Some("9"),None))
      }

    }

    "Evaluate the stanzas after user input stanza to determine the id of the next page" in new Test {

      val instructionStanza = InstructionStanza(3, Seq("3"), None, false)
      val questionStanza = Question(questionPhrase, answers, Seq("23","23","23"), None, false)
      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(Scalar, "X", "9")), Seq("22"), true)),
                        KeyedStanza("22", Choice(ChoiceStanza(Seq("2","3"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("2", instructionStanza),
                        KeyedStanza("3", questionStanza),
                        KeyedStanza("23", ValueStanza(List(Value(Scalar, "X", "4")), Seq("24"), true)),
                        KeyedStanza("24", Choice(ChoiceStanza(Seq("25","5"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false)))
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)
      val labels = LabelCache()

      renderer.renderPagePostSubmit(page, labels, "0").fold(fail){ case (next, newLabels) =>
        next shouldBe "25"
        newLabels.updatedLabels shouldBe Map("X" -> Label("X",Some("4"),None))
      }
    }

    "Evaluate the stanzas after user input stanza when question answer is end" in new Test {

      val instructionStanza = InstructionStanza(3, Seq("3"), None, false)
      val questionStanza = Question(questionPhrase, answers, Seq("23","23","23"), None, false)
      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(Scalar, "X", "9")), Seq("22"), true)),
                        KeyedStanza("22", Choice(ChoiceStanza(Seq("2","3"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("2", instructionStanza),
                        KeyedStanza("3", questionStanza),
                        KeyedStanza("23", ValueStanza(List(Value(Scalar, "X", "4")), Seq("24"), true)),
                        KeyedStanza("24", Choice(ChoiceStanza(Seq("end","5"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("end", EndStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)
      val labels = LabelCache()

      renderer.renderPagePostSubmit(page, labels, "0").fold(fail){ case (next, newLabels) =>
        next shouldBe "end"
        newLabels.updatedLabels shouldBe Map("X" -> Label("X",Some("4"),None))
      }
    }

    "Evaluate the stanzas after user input stanza when question which indicate a return to the same page (guidance deteceted error)" in new Test {

      val instructionStanza = InstructionStanza(3, Seq("3"), None, false)
      val questionStanza = Question(questionPhrase, answers, Seq("23","23","23"), None, false)
      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(Scalar, "X", "9")), Seq("22"), true)),
                        KeyedStanza("22", Choice(ChoiceStanza(Seq("2","3"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("2", instructionStanza),
                        KeyedStanza("3", questionStanza),
                        KeyedStanza("23", ValueStanza(List(Value(Scalar, "X", "467")), Seq("24"), true)),
                        KeyedStanza("24", Choice(ChoiceStanza(Seq("end","1"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("end", EndStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)
      val labels = LabelCache()

      renderer.renderPagePostSubmit(page, labels, "0").fold(succeed)(_ => fail)

    }

    "Return correct result when an attempt is made to renderPagePostSubmit on a page without any user input" in new Test {

      val instructionStanza = InstructionStanza(3, Seq("23"), None, false)

      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(Scalar, "X", "9")), Seq("22"), true)),
                        KeyedStanza("22", Choice(ChoiceStanza(Seq("2","23"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("2", instructionStanza),
                        KeyedStanza("23", ValueStanza(List(Value(Scalar, "X", "56")), Seq("24"), true)),
                        KeyedStanza("24", Choice(ChoiceStanza(Seq("25","34"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false)))
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)
      val labels = LabelCache()

      renderer.renderPagePostSubmit(page, labels, "0").fold(fail){case (next, newLabels) =>
        next shouldBe "34"
        newLabels.updatedLabels shouldBe Map("X" -> Label("X",Some("56"),None))
      }
    }

  }

}
