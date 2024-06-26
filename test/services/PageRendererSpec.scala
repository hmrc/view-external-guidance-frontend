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

import base.BaseSpec
import core.models.ocelot._
import core.models.ocelot.errors._
import core.models.ocelot.stanzas._
import core.services._
import mocks.MockAppConfig
import core.models.errors.Error
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.libs.json._

class PageRendererSpec extends BaseSpec with ProcessJson {

  // Define instance of class used in testing
  val pageBuilder = new PageBuilder(new LabelledData(new Timescales(new DefaultTodayProvider), new Rates()))
  val renderer: PageRenderer = new PageRenderer(MockAppConfig)
  val messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
  implicit val messages: Messages = messagesApi.preferred(Seq())
  val meta: Meta = Json.parse(prototypeMetaSection).as[Meta]

  trait Test {
    val pageId1 = Process.StartStanzaId
    val pageId2 = "4"
    val pageId3 = "6"
    val pageId4 = "9"
    val pageId5 = "11"
    val pageId6 = "14"
    val pageId7 = "17"
    val pageIds = Seq(pageId1, pageId2, pageId3, pageId4, pageId5, pageId6, pageId7)

    val answers =
      Seq(Phrase(Vector("Some Text 1", "Welsh: Some Text 1")),
          Phrase(Vector("Some Text 2", "Welsh: Some Text 2")),
          Phrase(Vector("Some Text 3", "Welsh: Some Text 3")))

    val answerDestinations = Seq("4", "5", "6")
    val questionPhrase: Phrase = Phrase(Vector("Some Text [label:X]", "Welsh: Some Text [label:X]"))
    val questionHintString = "A hint!!"
    val questionWithHintPhrase: Phrase = Phrase(Vector(s"Some Text[hint:${questionHintString}]", s"Welsh: Some Text[hint:${questionHintString}]"))

    val question: core.models.ocelot.stanzas.Question = Question(questionPhrase, answers, answerDestinations, None, false)

    implicit val ctx: UIContext = UIContext(LabelCache(), Map(), messages)

    def renderPagePostSubmit(p: Page, l: Labels, a: String): (Option[String], Labels) = {
      renderer.renderPagePostSubmit(p, l, a).fold(_ => fail(), res => res)
    }
    def renderPage(p: Page, l: Labels): (Seq[VisualStanza], Labels, Option[DataInput]) = {
      renderer.renderPage(p, l).fold(_ => fail(), res => res)
    }

    def testRender(pge: Page, id: String, lbls: Labels): Unit = {
      renderer.renderPagePostSubmit(pge, lbls, id) match {
        case Right((nxt, newLabels)) =>
          nxt.fold(fail()){ next =>
            next shouldBe answerDestinations(id.toInt)
            newLabels.updatedLabels shouldBe lbls.updatedLabels
          }
        case Left(_) => fail()
      }
    }

  }

  "PageRenderer" must {

    "Detect non-terminating page by enforcing a max number of stanzas per page before input" in new Test {
      val nonTerminatingPageError = Error(Error.ExecutionError, List(NonTerminatingPageError), Some(Published), Some("2"))
      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(ScalarType, "X", "9")), Seq("2"), true)),
                        KeyedStanza("2", Instruction(Phrase("Hello", "Hello"), Seq("3"), None, false)),
                        KeyedStanza("3", ValueStanza(List(Value(ScalarType, "X", "9")), Seq("1"), true)),
                        KeyedStanza("4", Question(questionPhrase, answers, Seq("23","23","23"), None, false)),
                        KeyedStanza("end", EndStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)

      renderer.renderPage(page, LabelCache()) match {
        case Left((err, _)) if err == nonTerminatingPageError => succeed
        case _ => fail()
      }
    }

    "Detect non-terminating page by enforcing a max number of stanzas per page after input" in new Test {
      val nonTerminatingPageError = Error(Error.ExecutionError, List(NonTerminatingPageError), Some(Published), Some("3"))
      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", Instruction(Phrase("Hello", "Hello"), Seq("4"), None, false)),
                        KeyedStanza("4", Question(questionPhrase, answers, Seq("3","3","3"), None, false)),
                        KeyedStanza("3", ValueStanza(List(Value(ScalarType, "X", "9")), Seq("2"), true)),
                        KeyedStanza("2", ValueStanza(List(Value(ScalarType, "X", "0")), Seq("3"), true)),
                        KeyedStanza("end", EndStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)

      renderer.renderPagePostSubmit(page, LabelCache(), "0") match {
        case Left((err, _)) if err == nonTerminatingPageError => succeed
        case _ => fail()
      }
    }

    "Detect visual stanza after input stanza" in new Test {
      val programmingError = Error(Error.ExecutionError, List(ProgrammingError("Visual stanzas found after input")), Some(Published), Some("6"))

      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
        KeyedStanza("1", Instruction(Phrase("Hello", "Hello"), Seq("5"), None, false)),
        KeyedStanza("5", ValueStanza(List(Value(ScalarType, "input1", "9")), Seq("4"), true)),
        KeyedStanza("4", Question(questionPhrase, answers, Seq("6","6","6"), None, false)),
        KeyedStanza("6", Instruction(Phrase("Goodbye", "Goodbye"), Seq("end"), None, false)),
        KeyedStanza("end", EndStanza)
      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)

      renderer.renderPagePostSubmit(page, LabelCache(), "0") match {
        case Left((err, _)) if err == programmingError => succeed
        case res => fail(res.toString)
      }
    }

    "Detect unsupported operations before input" in new Test {
      val operations: Seq[CalcOperation] = Seq(
        CalcOperation("[label:input1]", Addition, "10", "output1")
      )
      val unsupportedOpError =
        Error(Error.ExecutionError,
              List(UnsupportedOperationError("AddOperation", None, Some("10"), "[label:input1]",  "10")), 
              Some(Published), 
              Some("3"))

      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
        KeyedStanza("1", Instruction(Phrase("Hello", "Hello"), Seq("3"), None, false)),
        KeyedStanza("4", Question(questionPhrase, answers, Seq("end","end","end"), None, false)),
        KeyedStanza("3", Calculation(CalculationStanza(operations, Seq("4"), stack = false))),
        KeyedStanza("end", EndStanza)
      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)

      renderer.renderPage(page, LabelCache()) match {
        case Left((err, _)) if err == unsupportedOpError => succeed
        case res => fail()
      }
    }

    "Detect unsupported operations after input" in new Test {
      val operations: Seq[CalcOperation] = Seq(CalcOperation("[label:input1]", Addition, "10", "output1"))
      val unsupportedOpError =
        Error(Error.ExecutionError,
              List(UnsupportedOperationError("AddOperation",None, Some("10"), "[label:input1]",  "10")), 
              Some(Published), 
              Some("3"))

      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", Instruction(Phrase("Hello", "Hello"), Seq("4"), None, false)),
                        KeyedStanza("4", Question(questionPhrase, answers, Seq("3","3","3"), None, false)),
                        KeyedStanza("3", Calculation(CalculationStanza(operations, Seq("end"), stack = false))),
                        KeyedStanza("end", EndStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)

      renderer.renderPagePostSubmit(page, LabelCache(), "0") match {
        case Left((err, _)) if err == unsupportedOpError => succeed
        case res => fail()
      }
    }

    "Return an error when the page contains a non-supported stanza type" in new Test {
      val instructionStanza = Instruction(Phrase("Hello", "Hello"), Seq("5"), None, false)
      val callout1 = ErrorCallout(Phrase(Vector("Some Text", "Welsh: Some Text")), Seq("3"), false)
      val callout2 = SectionCallout(Phrase(Vector("Some Text", "Welsh: Some Text")), Seq("4"), false)
      val extraPage = PageStanza("/oops", Seq("6"), false)

      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
        KeyedStanza("1", callout1),
        KeyedStanza("3", callout2),
        KeyedStanza("4", instructionStanza),
        KeyedStanza("5", extraPage),
        KeyedStanza("6", EndStanza)
      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, Seq("5"))

      val programmingError = Error(Error.ExecutionError, List(ProgrammingError("Unknown stanza without Evaluate")), Some(Published), Some("5"))

      renderer.renderPage(page, LabelCache()) match {
        case Left((err, _)) if err == programmingError => succeed
        case res => fail(res.toString)
      }

    }

    "Determine the correct sequence of stanzas within a page with no user input" in new Test {
      val instruction = Instruction(Phrase("Hello", "Hello"), Seq("5"), None, false)
      val callout1 = ErrorCallout(Phrase(Vector("Some Text", "Welsh: Some Text")), Seq("3"), false)
      val callout2 = SectionCallout(Phrase(Vector("Some Text", "Welsh: Some Text")), Seq("4"), false)

      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", callout1),
                        KeyedStanza("3", callout2),
                        KeyedStanza("4", instruction)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, Seq("5"))

      val (visualStanzas, labels, dataInput) = renderPage(page, LabelCache())

      visualStanzas shouldBe List(callout1, callout2, instruction)

      dataInput shouldBe None

      labels.updatedLabels.keys.toList.length shouldBe 0
    }

    "Determine the correct sequence of stanzas within the final page of guidance" in new Test {
      val instructionStanza = Instruction(Phrase("Hello", "Hello"), Seq("5"), None, false)
      val callout1 = ErrorCallout(Phrase(Vector("Some Text", "Welsh: Some Text")), Seq("3"), false)
      val callout2 = SectionCallout(Phrase(Vector("Some Text", "Welsh: Some Text")), Seq("4"), false)

      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", callout1),
                        KeyedStanza("3", callout2),
                        KeyedStanza("4", instructionStanza),
                        KeyedStanza("5", EndStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, Seq("5"))

      val (visualStanzas, labels, dataInput) = renderPage(page, LabelCache())

      visualStanzas shouldBe List(callout1, callout2, instructionStanza)

      dataInput shouldBe None

      labels.updatedLabels.keys.toList.length shouldBe 0
    }

    "Determine the correct sequence of stanzas within a Question page" in new Test {
      val instructionStanza = Instruction(Phrase("Hello", "Hello"), Seq("3"), None, false)
      val qPhrase: Phrase = Phrase(Vector("Some Text [label:X]", "Welsh: Some Text [label:X]"))
      val expQPhrase: Phrase = Phrase(Vector("Some Text 4", "Welsh: Some Text [label:X]"))
      val questionStanza = Question(qPhrase, answers, answerDestinations, None, false)
      val resultQuestion = Question(expQPhrase, answers, answerDestinations, None, false)
      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(ScalarType, "X", "4")), Seq("22"), true)),
                        KeyedStanza("22", Choice(ChoiceStanza(Seq("2","3"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("2", instructionStanza),
                        KeyedStanza("3", questionStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)

      val (visualStanzas, labels, dataInput) = renderPage(page, LabelCache())
      visualStanzas shouldBe List(instructionStanza, resultQuestion)

      dataInput shouldBe Some(resultQuestion)

      labels.updatedLabels.keys.toList.length shouldBe 1
    }

    "Determine the correct sequence of stanzas within a Question page involving Choice" in new Test {
      val instructionStanza = Instruction(Phrase("Hello [label:X]", "Hello [label:X]"), Seq("3"), None, false)
      val qPhrase: Phrase = Phrase(Vector("Some Text [label:X]", "Welsh: Some Text [label:X]"))
      val expQPhrase: Phrase = Phrase(Vector("Some Text 9", "Welsh: Some Text [label:X]"))
      val questionStanza = Question(qPhrase, answers, answerDestinations, None, false)
      val resultQuestion = Question(expQPhrase, answers, answerDestinations, None, false)

      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(ScalarType, "X", "9")), Seq("22"), true)),
                        KeyedStanza("22", Choice(ChoiceStanza(Seq("2","3"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("2", instructionStanza),
                        KeyedStanza("3", questionStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)

      val (visualStanzas, labels, dataInput) = renderPage(page, LabelCache())
      visualStanzas shouldBe List(resultQuestion)
      dataInput shouldBe Some(resultQuestion)
      labels.updatedLabels.keys.toList.length shouldBe 1
    }

    "Evaluate the user input stanza to determine the id of the next page" in new Test {

      val instructionStanza = Instruction(Phrase("Hello", "Hello"), Seq("3"), None, false)
      val questionStanza = Question(questionPhrase, answers, answerDestinations, Some("TaxRefund"), false)
      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(ScalarType, "X", "9")), Seq("22"), true)),
                        KeyedStanza("22", Choice(ChoiceStanza(Seq("2","3"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("2", instructionStanza),
                        KeyedStanza("3", questionStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)

      testRender(
        page,
        "0",
        LabelCache(
        Map(),
        Map("X" -> ScalarLabel("X",List("9")), "TaxRefund" -> ScalarLabel("TaxRefund",List(answers.head.english), List(answers.head.welsh)))
        )
      )
      testRender(
        page,
        "1",
        LabelCache(
          Map(),
          Map("X" -> ScalarLabel("X",List("9")), "TaxRefund" -> ScalarLabel("TaxRefund",List(answers(1).english), List(answers(1).welsh)))
        )
      )
      testRender(
        page,
        "2",
        LabelCache(
          Map(),
          Map("X" -> ScalarLabel("X",List("9")), "TaxRefund" -> ScalarLabel("TaxRefund",List(answers(2).english), List(answers(2).welsh)))
        )
      )
    }

    "Evaluate the stanzas after user input stanza to determine the id of the next page" in new Test {

      val instructionStanza = Instruction(Phrase("Hello", "Hello"), Seq("3"), None, false)
      val questionStanza = Question(questionPhrase, answers, Seq("23","23","23"), None, false)
      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(ScalarType, "X", "9")), Seq("22"), true)),
                        KeyedStanza("22", Choice(ChoiceStanza(Seq("2","3"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("2", instructionStanza),
                        KeyedStanza("3", questionStanza),
                        KeyedStanza("23", ValueStanza(List(Value(ScalarType, "X", "4")), Seq("24"), true)),
                        KeyedStanza("24", Choice(ChoiceStanza(Seq("25","5"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false)))
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)
      val labels = LabelCache()

      val (next, newLabels) = renderPagePostSubmit(page, labels, "0")
      next shouldBe Some("25")
      newLabels.updatedLabels shouldBe Map("X" -> ScalarLabel("X",List("4")))
    }

    "Evaluate the stanzas after Question stanza and confirm setting of associated label" in new Test {

      val questionLabel = "ChosenAnswer"
      val instructionStanza = Instruction(Phrase("Hello", "Hello"), Seq("3"), None, false)
      val questionStanza = Question(questionPhrase, answers, Seq("23","23","23"), Some(questionLabel), false)
      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(ScalarType, "X", "9")), Seq("22"), true)),
                        KeyedStanza("22", Choice(ChoiceStanza(Seq("2","3"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("2", instructionStanza),
                        KeyedStanza("3", questionStanza),
                        KeyedStanza("23", ValueStanza(List(Value(ScalarType, "X", "4")), Seq("24"), true)),
                        KeyedStanza("24", Choice(ChoiceStanza(Seq("25","5"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false)))
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)
      val labels = LabelCache()

      val (next, newLabels) = renderPagePostSubmit(page, labels, "0")
      next shouldBe Some("25")

      newLabels.updatedLabels.get(questionLabel).isEmpty shouldBe false
      newLabels.displayValue(questionLabel)(Lang("en")) shouldBe Some(answers(0).english)
      newLabels.displayValue(questionLabel)(Lang("cy")) shouldBe Some(answers(0).welsh)
    }

    "Evaluate the stanzas after user input stanza when question answer is end" in new Test {

      val instructionStanza = Instruction(Phrase("Hello", "Hello"), Seq("3"), None, false)
      val questionStanza = Question(questionPhrase, answers, Seq("23","23","23"), None, false)
      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(ScalarType, "X", "9")), Seq("22"), true)),
                        KeyedStanza("22", Choice(ChoiceStanza(Seq("2","3"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("2", instructionStanza),
                        KeyedStanza("3", questionStanza),
                        KeyedStanza("23", ValueStanza(List(Value(ScalarType, "X", "4")), Seq("24"), true)),
                        KeyedStanza("24", Choice(ChoiceStanza(Seq("end","5"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("end", EndStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)
      val labels = LabelCache()

      val (next, newLabels) = renderPagePostSubmit(page, labels, "0")
      next shouldBe None
      newLabels.updatedLabels shouldBe Map("X" -> ScalarLabel("X",List("4")))
    }

    "Evaluate the stanzas after user input stanza when question which indicate a return to the same page (guidance deteceted error)" in new Test {

      val instructionStanza = Instruction(Phrase("Hello", "Hello"), Seq("3"), None, false)
      val questionStanza = Question(questionPhrase, answers, Seq("23","23","23"), None, false)
      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(ScalarType, "X", "9")), Seq("22"), true)),
                        KeyedStanza("22", Choice(ChoiceStanza(Seq("2","3"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("2", instructionStanza),
                        KeyedStanza("3", questionStanza),
                        KeyedStanza("23", ValueStanza(List(Value(ScalarType, "X", "467")), Seq("24"), true)),
                        KeyedStanza("24", Choice(ChoiceStanza(Seq("end","1"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("end", EndStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)
      val labels = LabelCache()

      val (next, _) = renderPagePostSubmit(page, labels, "0")

      next shouldBe None

    }

    "Evaluate the stanzas after user input stanza when question which indicate a return to current page (identifying first stanza after the page stanza)" in new Test {

      val instructionStanza = Instruction(Phrase("Hello", "Hello"), Seq("3"), None, false)
      val questionStanza = Question(questionPhrase, answers, Seq("23","23","23"), None, false)
      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(ScalarType, "X", "9")), Seq("22"), true)),
                        KeyedStanza("22", Choice(ChoiceStanza(Seq("2","3"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("2", instructionStanza),
                        KeyedStanza("3", questionStanza),
                        KeyedStanza("23", ValueStanza(List(Value(ScalarType, "X", "467")), Seq("24"), true)),
                        KeyedStanza("24", Choice(ChoiceStanza(Seq("end","1"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("end", EndStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)
      val labels = LabelCache()

      val (next, _) = renderPagePostSubmit(page, labels, "0")

      next shouldBe None

    }

    "Evaluate the stanzas after user input stanza when question which indicate the supplied answer index is invalid" in new Test {

      val instructionStanza = Instruction(Phrase("Hello", "Hello"), Seq("3"), None, false)
      val questionStanza = Question(questionPhrase, answers, Seq("23","23","23"), None, false)
      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(ScalarType, "X", "9")), Seq("22"), true)),
                        KeyedStanza("22", Choice(ChoiceStanza(Seq("2","3"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("2", instructionStanza),
                        KeyedStanza("3", questionStanza),
                        KeyedStanza("23", ValueStanza(List(Value(ScalarType, "X", "467")), Seq("24"), true)),
                        KeyedStanza("24", Choice(ChoiceStanza(Seq("end","1"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("end", EndStanza)
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)
      val labels = LabelCache()

      val (next, _) = renderPagePostSubmit(page, labels, "12")

      next shouldBe None

    }

    "Return correct result when an attempt is made to renderPagePostSubmit on a page without any user input" in new Test {

      val instructionStanza = Instruction(Phrase("Hello", "Hello"), Seq("23"), None, false)

      val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                        KeyedStanza("1", ValueStanza(List(Value(ScalarType, "X", "9")), Seq("22"), true)),
                        KeyedStanza("22", Choice(ChoiceStanza(Seq("2","23"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false))),
                        KeyedStanza("2", instructionStanza),
                        KeyedStanza("23", ValueStanza(List(Value(ScalarType, "X", "56")), Seq("24"), true)),
                        KeyedStanza("24", Choice(ChoiceStanza(Seq("25","34"), Seq(ChoiceStanzaTest("[label:X]", LessThanOrEquals, "8")), false)))
                      )
      val page = Page(Process.StartStanzaId, "/test-page", stanzas, answerDestinations)
      val labels = LabelCache()

      val (next, newLabels) = renderPagePostSubmit(page, labels, "0")
      next shouldBe Some("34")
      newLabels.updatedLabels shouldBe Map("X" -> ScalarLabel("X",List("56")))

    }

    "execute calculation stanza when rendering page" in new Test {

      val p1: Phrase = Phrase(Vector("Title [label:input1]", "Welsh - Title [label:input1]"))
      val callout: Callout = TitleCallout(p1,Seq("2"),stack = false)
      val ep1: Phrase = Phrase(Vector("Title 60", "Welsh - Title [label:input1]"))
      val expandedCallout = TitleCallout(ep1,Seq("2"),stack = false)

      val p2: Phrase = Phrase(Vector("Example calculation", "Welsh - Example calculation"))
      val instruction1: Instruction = Instruction(p2,Seq("3"),None,stack = false)

      val operations: Seq[CalcOperation] = Seq(CalcOperation("[label:input1]", Addition, "10", "output1"))
      val calculationStanza: CalculationStanza = CalculationStanza(operations, Seq("4"), stack = false)
      val calculation: Calculation = Calculation(calculationStanza)

      val p3: Phrase = Phrase(Vector("Sum of values : [label:output1]", "Welsh - Sum of values : [label:output1]"))
      val instruction2: Instruction = Instruction(p3,Seq("end"),None,stack = false)
      val ep3: Phrase = Phrase(Vector("Sum of values : 70", "Welsh - Sum of values : [label:output1]"))
      val expandedInstruction2: Instruction = Instruction(ep3,Seq("end"),None,stack = false)

      val stanzas: Seq[KeyedStanza] = Seq(
        KeyedStanza("start", PageStanza("/start", Seq("1"), stack = false)),
        KeyedStanza("1", callout),
        KeyedStanza("2", instruction1),
        KeyedStanza("3", calculation),
        KeyedStanza("4", instruction2),
        KeyedStanza("end", EndStanza)
      )

      val page: Page = Page(Process.StartStanzaId, "/render", stanzas, Seq("end"))

      val input1: Label = ScalarLabel( "input1", List("60"))

      val labelMap: Map[String, Label] = Map(input1.name -> input1)

      val (visualStanzas, labels, dataInput) = renderPage(page, LabelCache(labelMap))

      visualStanzas shouldBe List(expandedCallout, instruction1, expandedInstruction2)

      dataInput shouldBe None

      labels.labelMap shouldBe labelMap

      val expectedUpdatedLabels: Map[String, Label] = Map("output1" -> ScalarLabel("output1", List("70")))

      labels.updatedLabels shouldBe expectedUpdatedLabels

      dataInput shouldBe None
    }

    "execute calculation stanza when submitting page" in new Test {

      val valueStanza: ValueStanza = ValueStanza(List(Value(ScalarType, "input1", "10")), Seq("2"), true)
      val callout: Callout = TitleCallout(Phrase(Vector("Title", "Welsh - Title")), Seq("3"), false)

      val instruction1: Instruction = Instruction(
        Phrase(Vector("Example of calculation after submit", "Welsh - Example of calculation after submit")),
        Seq("4"),
        None,
        stack = false)

      val questionStanza = Question(
        questionPhrase,
        answers,
        Seq("5","5","5"),
        None,
        stack = false)

      val operations: Seq[CalcOperation] = Seq(
        CalcOperation("[label:input1]", Addition, "15", "output1")
      )

      val calculationStanza: CalculationStanza = CalculationStanza(operations, Seq("6"), stack = false)

      val calculation: Calculation = Calculation(calculationStanza)

      val choiceStanzaTest: ChoiceStanzaTest = ChoiceStanzaTest("[label:output1]", LessThan, "50")

      val choiceStanza: ChoiceStanza = ChoiceStanza(
        Seq("7", "14"),
        Seq(choiceStanzaTest),
        stack = false)

      val choice: Choice = Choice(choiceStanza)

      val stanzas: Seq[KeyedStanza] = Seq(
        KeyedStanza("start", PageStanza("/start", Seq("1"), stack = false)),
        KeyedStanza("1", valueStanza),
        KeyedStanza("2", callout),
        KeyedStanza("3", instruction1),
        KeyedStanza("4", questionStanza),
        KeyedStanza("5", calculation),
        KeyedStanza("6", choice),
        KeyedStanza("end", EndStanza)
      )

      val page: Page = Page(Process.StartStanzaId, "/render", stanzas, answerDestinations)

      val labels = LabelCache()

      val (next, newLabels) = renderPagePostSubmit(page, labels, "0")

      next shouldBe Some("7")

      val expectedUpdatedLabels: Map[String, Label] = Map(
        "input1" -> ScalarLabel("input1", List("10")),
        "output1" -> ScalarLabel("output1", List("25"))
      )

      newLabels.updatedLabels shouldBe expectedUpdatedLabels
    }
  }

    "Build page with loop and display loop index" in new Test {

      val p1: Phrase = Phrase("Title [label:input1]", "Welsh - Title [label:input1]")
      val expP1: Phrase = Phrase("Title 56", "Welsh - Title [label:input1]")
      val callout: Callout = TitleCallout(p1,Seq("2"),stack = false)
      val expCallout: Callout = TitleCallout(expP1,Seq("2"),stack = false)

      val valueStanza: ValueStanza = ValueStanza(List(Value(ScalarType, "X", "0")), Seq("3"), true)
      val p2: Phrase = Phrase("X = [label:X]", "Welsh - X = [label:X]")
      val instruction1: Instruction = Instruction(p2,Seq("4"),None,stack = false)

      val operations: Seq[CalcOperation] = Seq(CalcOperation("[label:X]", Addition, "1", "X"))
      val calculationStanza: CalculationStanza = CalculationStanza(operations, Seq("5"), stack = false)
      val calculation: Calculation = Calculation(calculationStanza)

      val choiceStanzaTest: ChoiceStanzaTest = ChoiceStanzaTest("[label:X]", LessThan, "10")

      val choice: Choice = Choice(ChoiceStanza(Seq("3", "end"),Seq(choiceStanzaTest),stack = false))

      val expandedInstructions: List[Instruction] = Range(0,10).toList.map(idx => Instruction(Phrase(s"X = $idx", "Welsh - X = [label:X]"),Seq("4"),None,stack = false))

      val renderedVisualStanzas = expCallout :: expandedInstructions

      val stanzas: Seq[KeyedStanza] = Seq(
        KeyedStanza("start", PageStanza("/start", Seq("1"), stack = false)),
        KeyedStanza("1", callout),
        KeyedStanza("2", valueStanza),
        KeyedStanza("3", instruction1),
        KeyedStanza("4", calculation),
        KeyedStanza("5", choice),
        KeyedStanza("end", EndStanza)
      )

      val page: Page = Page(Process.StartStanzaId, "/render", stanzas, Seq("end"))

      val (visualStanzas, labels, dataInput) = renderPage(page, LabelCache(List(ScalarLabel("input1", List("56")))))

      visualStanzas.length shouldBe renderedVisualStanzas.length

      visualStanzas shouldBe renderedVisualStanzas

      dataInput shouldBe None

      labels.updatedLabels shouldBe Map("X" -> ScalarLabel("X", List("10")))
    }

}
