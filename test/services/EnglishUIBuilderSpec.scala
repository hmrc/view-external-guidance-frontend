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

import base.{BaseSpec, EnglishLanguage}
import core.models.ocelot._
import core.models.ocelot.errors.UnsupportedUiPatternError
import core.models.ocelot.stanzas._
import core.services._
import models.ocelot.stanzas._
import models.{PageDesc, ui}
import models.ui.{BulletPointList, ConfirmationPanel, CyaSummaryList, Details, ErrorMsg, FormPage, H1, H3, H4, InsetText, Link, Paragraph, RequiredErrorMsg, Sequence, Table, Text, WarningText, Words}
import play.api.i18n.{Messages, MessagesApi}

class EnglishUIBuilderSpec extends BaseSpec with ProcessJson with EnglishLanguage {
  implicit val labels: Labels = LabelCache()

  trait BaseTest {
    val messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    implicit val messages: Messages = messagesApi.preferred(Seq())
  }

  trait QuestionTest extends BaseTest {
    implicit val urlMap: Map[String, PageDesc] =
      Map(
        Process.StartStanzaId -> PageDesc(Process.StartStanzaId, "/blah"),
        "3" -> PageDesc("3", "dummy-path"),
        "4" -> PageDesc("4", "dummy-path/question"),
        "5" -> PageDesc("5", "dummy-path/blah"),
        "6" -> PageDesc("6", "dummy-path/anotherquestion"),
        "34" -> PageDesc("34", "dummy-path/next")
      )
    val answerDestinations: Seq[String] = Seq("4", "5", "6")
    val questionPhrase: Phrase = Phrase(Vector("Some Text", "Welsh: Some Text"))
    val questionHintString: String = "A hint!!"
    val questionWithHintPhrase: Phrase = Phrase(Vector(s"Some Text[hint:${questionHintString}]", s"Welsh: Some Text[hint:${questionHintString}]"))

    val answers =
      Seq(Phrase(Vector("Some Text", "Welsh: Some Text")), Phrase(Vector("Some Text", "Welsh: Some Text")), Phrase(Vector("Some Text", "Welsh: Some Text")))
    val question: core.models.ocelot.stanzas.Question = Question(questionPhrase, answers, answerDestinations, None, false)

    val stanzas = Seq(
      KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
      KeyedStanza("1", ErrorCallout(Phrase(Vector("Some Text", "Welsh: Some Text")), Seq("3"), false)),
      KeyedStanza("3", SectionCallout(Phrase(Vector("Some Text", "Welsh: Some Text")), Seq("31"), false)),
      KeyedStanza("31", SubSectionCallout(Phrase(Vector("Some Text", "Welsh: Some Text")), Seq("4"), false)),
      KeyedStanza("4", Instruction(Phrase(Vector("Some Text", "Welsh: Some Text")), Seq("end"), None, false))
    )

    val page = Page(Process.StartStanzaId, "/test-page", stanzas :+ KeyedStanza("5", Question(questionPhrase, answers, answerDestinations, None, false)), Seq.empty)

    val pageWithQuestionHint =
      Page(Process.StartStanzaId, "/test-page", stanzas :+ KeyedStanza("5", Question(questionWithHintPhrase, answers, answerDestinations, None, false)), Seq.empty)

    val uiBuilder: UIBuilder = new UIBuilder()
    implicit val ctx: UIContext = UIContext(labels, urlMap, messages)
    val four: Int = 4
  }

  "UIBulider Question processing" must {

    "Ignore Error Callouts when there are no errors" in new QuestionTest {
      uiBuilder.buildPage(page.url, page.stanzas.toList.collect{case s: VisualStanza => s}, NoError) match {
        case Right(s: FormPage) if s.formComponent.errorMsgs.isEmpty => succeed
        case Right(s: FormPage) => fail("No error messages should be included on page")
        case _ => fail("Should return FormPage")
      }
    }

    "Include Error messages when there are errors" in new QuestionTest {

      uiBuilder.buildPage(page.url, page.stanzas.toList.collect{case s: VisualStanza => s}, ValueMissingError) match {
        case Right(s: FormPage) if s.formComponent.errorMsgs.isEmpty => fail("No error messages found on page")
        case Right(s: FormPage) => succeed
        case _ => fail("Should return FormPage")
      }
    }

    "Maintain order of components within a Question" in new QuestionTest {
      uiBuilder.buildPage(page.url, page.stanzas.toList.collect{case s: VisualStanza => s}, NoError) match {
        case Right(q: FormPage) =>
          q.formComponent.body(0) match {
            case h: H3 => succeed
            case _ => fail("Ordering of question body components not maintained")
          }
          q.formComponent.body(1) match {
            case h: H4 => succeed
            case _ => fail("Ordering of question body components not maintained")
          }
          q.formComponent.body(2) match {
            case h: Paragraph => succeed
            case _ => fail("Ordering of question body components not maintained")
          }

        case _ => fail("Page should be a Question page")
      }
    }

    "Include a question hint appended to the question text" in new QuestionTest {
      uiBuilder.buildPage(pageWithQuestionHint.url, pageWithQuestionHint.stanzas.toList.collect{case s: VisualStanza => s}) match {
        case Right(s: FormPage) if s.formComponent.hint == Some(Text(questionHintString)) => succeed
        case Right(s: FormPage) => fail("No hint found within Question")
        case _ => fail("Should return FormPage")
      }
    }
  }

  trait Test extends BaseTest with ProcessJson {

    case class UnsupportedVisualStanza(override val next: Seq[String], stack: Boolean) extends VisualStanza
    val lang0 = Vector("Some Text", "Welsh: Some Text")
    val lang1 = Vector("Some Text1", "Welsh: Some Text1")
    val lang2 = Vector("Some Text2", "Welsh: Some Text2")
    val lang3 = Vector("Some Text3", "Welsh: Some Text3")
    val lang4 = Vector("Some Text4", "Welsh: Some Text4")
    val lang5 = Vector("Some Text5", "Welsh: Some Text5")
    val ltxt1 = Text(Words("This is a ", true))
    val ltxt2 = Text(" followed by ")
    val ltxt3 = Text(" and nothing")
    val link1Txt = "A link"
    val link2Txt = "Another Link"
    val link2Start = "Back to beginning"
    val link1Txt2 = "A link at start of phrase"
    val link2Txt2 = "Another Link at end of phrase"
    val pageLink1Text = "A page link"
    val pageLink2Text = "Another page link"
    val q1 = Vector("Do you agree?", "Welsh: Do you agree?")
    val ans1 = Vector("Yes", "Welsh: Yes")
    val ans2 = Vector("No", "Welsh: Yes")
    val ans3 = Vector("Not sure", "Welsh: Yes")
    val ans1WithHint = Vector("Yes[hint:You agree with the assertion]", "Welsh: Yes[hint:Welsh: You agree with the assertion]")
    val ans2WithHint = Vector("No[hint:You DONT agree with the assertion]", "Welsh: Yes[hint:Welsh: You DONT agree with the assertion]")
    val ans3WithHint = Vector("Not sure[hint:You dont know]", "Welsh: Yes[hint:Welsh: You dont know]")
    val hint1 = Text("You agree with the assertion")
    val hint2 = Text("You DONT agree with the assertion")
    val hint3 = Text("You dont know")
    val link1 = Link("https://www.bbc.co.uk", link1Txt)
    val link2 = Link("https://www.gov.uk", link2Txt)
    val link2_1 = Link("https://www.bbc.co.uk", link1Txt2)
    val link2_2 = Link("https://www.gov.uk", link2Txt2)
    val link3 = Link("dummy-path/blah", lang4(0))
    val link4 = Link("https://www.bbc.co.uk", lang4(0))
    val pageLink1 = Link("dummy-path/next", pageLink1Text)
    val pageLink2 = Link("dummy-path", pageLink2Text)
    val startLink = Link("/blah", link2Start)

    implicit val urlMap: Map[String, PageDesc] =
      Map(
        Process.StartStanzaId -> PageDesc(Process.StartStanzaId, "/blah"),
        "3" -> PageDesc("3", "dummy-path"),
        "4" -> PageDesc("4", "dummy-path/question"),
        "5" -> PageDesc("5", "dummy-path/blah"),
        "6" -> PageDesc("6", "dummy-path/anotherquestion"),
        "34" -> PageDesc("34", "dummy-path/next")
      )
    val answerDestinations = Seq("4", "5", "6")

    val txtWithLinks = Phrase(
      Vector(
        "[bold:This is a ][link:A link:https://www.bbc.co.uk] followed by [link:Another Link:https://www.gov.uk] and nothing",
        "[bold:Welsh: This is a ][link:Welsh: A link:https://www.bbc.co.uk] Welsh: followed by [link:Welsh: Another Link:https://www.gov.uk] Welsh: and nothing"
      )
    )

    val txtWithLinks2 = Phrase(
      Vector(
        "[link:A link at start of phrase:https://www.bbc.co.uk] followed by [link:Another Link at end of phrase:https://www.gov.uk]",
        "[link:Welsh: A link at start of phrase:https://www.bbc.co.uk] Welsh: followed by [link:Welsh: Another Link at end of phrase:https://www.gov.uk]"
      )
    )

    val txtWithPageLinks = Phrase(
      Vector(
        "[bold:This is a ][link:A page link:34] followed by [link:Another page link:3] and nothing",
        "[bold:Welsh: This is a ][link:Welsh: A page link:34] Welsh: followed by [link:Welsh: Another page link:3] Welsh: and nothing"
      )
    )

    val txtWithAllLinks = Phrase(
      Vector(
        "[link:A link at start of phrase:https://www.bbc.co.uk] followed by [link:A page link:34][link:Back to beginning:start]",
        "[link:Welsh: A link at start of phrase:https://www.bbc.co.uk] Welsh: followed by [link:Welsh: A page link:34][link:Back to beginning:start]"
      )
    )

    val linkInstructionStanza = Instruction(Phrase(lang4), Seq("end"), Some(core.models.ocelot.Link(7, "5", "", false)), false)
    val hyperLinkInstructionStanza = Instruction(Phrase(lang4), Seq("end"), Some(core.models.ocelot.Link(7, "https://www.bbc.co.uk", "", false)), false)
    val embeddedLinkInstructionStanza = Instruction(txtWithLinks, Seq("end"), None, false)
    val embeddedLinkInstructionStanza2 = Instruction(txtWithLinks2, Seq("end"), None, false)
    val embeddedPageLinkInstructionStanza = Instruction(txtWithPageLinks, Seq("end"), None, false)
    val embeddedAllLinkInstructionStanza = Instruction(txtWithAllLinks, Seq("end"), None, false)
    val embeddedSubsectionCalloutStanza = SubSectionCallout(Phrase(lang5), Seq("3"), false)
    val importantCalloutStanza = ImportantCallout(Phrase(lang0), Seq("3"), false)
    val valueErrorCalloutStanza = ValueErrorCallout(Phrase(lang0), Seq("3"), false)
    val typeErrorCalloutStanza = TypeErrorCallout(Phrase(lang0), Seq("3"), false)
    val questionPhrase: Phrase = Phrase(q1)
    val answers = Seq(Phrase(ans1), Phrase(ans2), Phrase(ans3))
    val answersWithHints = Seq(Phrase(ans1WithHint), Phrase(ans2WithHint), Phrase(ans3WithHint))
    val question: core.models.ocelot.stanzas.Question = Question(questionPhrase, answers, answerDestinations, None, false)
    val questionWithAnswerHints: core.models.ocelot.stanzas.Question = Question(questionPhrase, answersWithHints, answerDestinations, None, false)

    val initialStanza = Seq(
      KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
      KeyedStanza("1", Instruction(Phrase(lang2), Seq("2"), None, false)),
      KeyedStanza("2", TitleCallout(Phrase(lang0), Seq("3"), false)),
      KeyedStanza("3", SubTitleCallout(Phrase(lang1), Seq("4"), false)),
      KeyedStanza("4", LedeCallout(Phrase(lang2), Seq("5"), false)),
      KeyedStanza("5", Instruction(Phrase(lang3), Seq("end"), None, false)),
    )

    val stanzasWithQuestion = Seq(
      KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
      KeyedStanza("1", Instruction(Phrase(lang2), Seq("2"), None, false)),
      KeyedStanza("2", Instruction(Phrase(lang3), Seq("3"), None, false)),
      KeyedStanza("3", question)
    )

    val stanzasWithQuestionAndHints = Seq(
      KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
      KeyedStanza("1", Instruction(Phrase(lang2), Seq("2"), None, false)),
      KeyedStanza("2", Instruction(Phrase(lang3), Seq("3"), None, false)),
      KeyedStanza("3", questionWithAnswerHints)
    )

    val questionPage = Page(Process.StartStanzaId, "/blah", stanzasWithQuestion, Seq.empty)

    val questionPageWithHints = Page(Process.StartStanzaId, "/blah", stanzasWithQuestionAndHints, Seq.empty)

    val stanzas: Seq[KeyedStanza] = initialStanza ++ Seq(KeyedStanza("1", linkInstructionStanza),
                                                         KeyedStanza("2", importantCalloutStanza),
                                                         KeyedStanza("21", valueErrorCalloutStanza),
                                                         KeyedStanza("22", typeErrorCalloutStanza),
                                                         KeyedStanza("3", EndStanza))
    val stanzasWithHyperLink: Seq[KeyedStanza] = initialStanza ++ Seq(KeyedStanza("6", hyperLinkInstructionStanza), KeyedStanza("7", EndStanza))
    val stanzasWithEmbeddedLinks: Seq[KeyedStanza] = initialStanza ++ Seq(KeyedStanza("6", embeddedLinkInstructionStanza), KeyedStanza("7", EndStanza))
    val stanzasWithEmbeddedLinks2: Seq[KeyedStanza] = initialStanza ++ Seq(KeyedStanza("6", embeddedLinkInstructionStanza2), KeyedStanza("7", EndStanza))
    val stanzasWithEmbeddedPageLinks: Seq[KeyedStanza] = initialStanza ++ Seq(KeyedStanza("6", embeddedPageLinkInstructionStanza), KeyedStanza("7", EndStanza))
    val stanzasWithEmbeddedAllLinks: Seq[KeyedStanza] = initialStanza ++ Seq(KeyedStanza("6", embeddedAllLinkInstructionStanza), KeyedStanza("7", EndStanza))
    val stanzasWithEmbeddedSubsection: Seq[KeyedStanza] = initialStanza ++ Seq(KeyedStanza("1", embeddedSubsectionCalloutStanza), KeyedStanza("2", EndStanza))

    val page: Page = Page(Process.StartStanzaId, "/test-page", stanzas, Seq.empty)

    val hyperLinkPage: Page = Page(Process.StartStanzaId, "/test-page", stanzasWithHyperLink, Seq.empty)

    val pageWithH4 = Page(Process.StartStanzaId, "/test-page", stanzas, Seq.empty)

    val textItems = ltxt1 +
      Text(link1) +
      ltxt2 +
      Text(link2) +
      ltxt3
    val textItems2 = Text(link2_1) + ltxt2 + Text(link2_2)

    val pageLinkTextItems = ltxt1 +
      Text(pageLink1) +
      ltxt2 +
      Text(pageLink2) +
      ltxt3
    val allLinksTextItems = Text(link2_1) + ltxt2 + Text(pageLink1) + Text(startLink)

    val pageWithEmbeddLinks: Page = Page(Process.StartStanzaId, "/test-page", stanzasWithEmbeddedLinks, Seq.empty)

    val pageWithEmbeddLinks2 = Page(Process.StartStanzaId, "/test-page", stanzasWithEmbeddedLinks2, Seq.empty)

    val pageWithEmbeddPageLinks = Page(Process.StartStanzaId, "/test-page", stanzasWithEmbeddedPageLinks, Seq.empty)

    val pageWithEmbeddAllLinks = Page(Process.StartStanzaId, "/test-page", stanzasWithEmbeddedAllLinks, Seq.empty)

    val pageWithEmbeddH4 = Page(Process.StartStanzaId, "/test-page", stanzasWithEmbeddedSubsection, Seq.empty)

    val brokenLinkPhrase = Phrase(Vector("Hello [link:Blah Blah:htts://www.bbc.co.uk]", "Welsh: Hello [link:Blah Blah:htts://www.bbc.co.uk]"))
    // for multi page testing
    val pageBuilder: PageBuilder = new PageBuilder(new Timescales(new DefaultTodayProvider))
    val stanzaPages = pageBuilder.pages(prototypeJson.as[Process]).getOrElse(Seq.empty)
    val prototypeUrlMap = stanzaPages.map(p => (p.id, p.url)).toMap

    // Create pages for extra income V6 process
    val extraIncomeStanzaPages = pageBuilder.pages(prototypeExtraIncomeV6Json.as[Process]).getOrElse(Seq.empty)
    val extraIncomeUrlMap = extraIncomeStanzaPages.map(p => (p.id, PageDesc(p.id, p.url))).toMap

    // Define instance of class to be used in tests
    val uiBuilder: UIBuilder = new UIBuilder()
    implicit val ctx: UIContext = UIContext(labels.update("week", "week", "Welsh: week")
                                                  .update("Money", "2.0", "1.0"),
                                            urlMap,
                                            messages)
    val four: Int = 4
    val five: Int = 5
  }

  trait SLTest extends Test {
    val rows = Seq.fill(3)(Row(Seq(Phrase(Vector("HELLO", "HELLO")), Phrase(Vector("World", "World")), Phrase()), Seq()))
    val rowsComplete = Seq.fill(3)(Row(Seq(Phrase(Vector("HELLO", "HELLO")), Phrase(Vector("World", "World")), Phrase(Vector("Blah", "Blah"))), Seq()))
    val sparseRows = Seq(rows(0), Row(Seq(Phrase(Vector("HELLO", "HELLO"))), Seq()), rows(2))
    val phraseWithLinkAndHint = Phrase(Vector("[link:Change[hint:HELLO]:3]", "[link:Change[hint:HELLO]:3]"))
    val rowsWithLinkAndHint = Seq.fill(3)(Row(Seq(Phrase(Vector("HELLO", "HELLO")), Phrase(Vector("World", "World")), phraseWithLinkAndHint), Seq()))
    val phraseWithLinkAndFakedWelsh = Phrase(Vector("[link:Change[hint:HELLO]:3]", "Welsh: [link:Change[hint:HELLO]:3]"))
    val rowsWithFakedWelshLinK = Seq.fill(3)(Row(Seq(Phrase(Vector("HELLO", "HELLO")), Phrase(Vector("World", "World")), phraseWithLinkAndFakedWelsh), Seq()))
    val sparseRowsWithLinkAndHint = Seq(rowsWithLinkAndHint(0), Row(Seq(Phrase(Vector("HELLO", "HELLO"))), Seq()), rowsWithLinkAndHint(2))

    val dlRows = Seq.fill(3)(Seq(Text("HELLO"), Text("World"), Text("")))
    val expectedDl = CyaSummaryList(dlRows)
    val dlRowsComplete = Seq.fill(3)(Seq(Text("HELLO"), Text("World"), Text("Blah")))
    val expectedDlComplete = CyaSummaryList(dlRowsComplete)
    val sparseDlRows = Seq(dlRows(0), Seq(Text("HELLO"), Text(""), Text("")), dlRows(2))
    val expectedDlSparse = CyaSummaryList(sparseDlRows)
    val dlRowsWithLinkAndHint = Seq.fill(3)(Seq(Text("HELLO"),
                                                               Text("World"),
                                                               Text.link("dummy-path","Change", false, false, Some("HELLO"))))
    val expectedDLWithLinkAndHint = CyaSummaryList(dlRowsWithLinkAndHint)
    val headingPhrase = Phrase("Heading", "Welsh: Heading")
  }

  trait TableTest extends Test {
    val rows = Row(Seq(Phrase(Vector("HELLO", "HELLO")), Phrase(Vector("World", "World"))), Seq()) +:
               Seq.fill(3)(Row(Seq(Phrase(Vector("HELLO", "HELLO")), Phrase(Vector("World", "World"))), Seq(), true))
    val simpleRowGroup = RowGroup(rows)
    val stackedRowGroup = RowGroup(Seq.fill(3)(Row(Seq(Phrase(Vector("HELLO", "HELLO")), Phrase(Vector("World", "World"))), Seq(), true)))
    val rowsWithHeading = Row(Seq(Phrase(Vector("[bold:HELLO]", "[bold:Welsh: HELLO]")), Phrase(Vector("[bold:World]", "[bold:Welsh: World]"))), Seq(), true) +:
               Seq.fill(3)(Row(Seq(Phrase(Vector("HELLO", "HELLO")), Phrase(Vector("World", "World"))), Seq(), true))
    val tableHeading = rowsWithHeading(0).cells.map(TextBuilder.fromPhrase(_))
    val tableRowGroup = RowGroup(rowsWithHeading)
    val numericRowGroup = RowGroup(Seq.fill(4)(Row(Seq(Phrase(Vector("HELLO", "HELLO")), Phrase(Vector("[label:Money:currency]", "[label:Money:currency]"))), Seq(), true)))
    val headingPhrase = Phrase("Heading", "Welsh: Heading")
    val headingText = Text(headingPhrase.value(lang))

    val emptyRows = Seq.fill(3)(Row(Seq.empty, Seq.empty, true))
    val oddRowGroup = RowGroup(emptyRows)
    val emptyRowGroup = RowGroup(Seq.empty)

  }

  trait NumberListTest extends Test {
    val headingPhrase = Phrase("Heading", "Welsh: Heading")
    val num1Phrase = Phrase(Vector("Line1", "Welsh Line1"))
    val num2Phrase = Phrase(Vector("Line2", "Welsh Line2"))
    val num3Phrase = Phrase(Vector("Line3", "Welsh Line3"))
    val num4Phrase = Phrase(Vector("Line4", "Welsh Line4"))

    val num1ListCo = NumberedListItemCallout(num1Phrase, Seq(""), false)
    val num2ListCo = NumberedListItemCallout(num2Phrase, Seq(""), true)
    val num3ListCo = NumberedListItemCallout(num3Phrase, Seq(""), true)
    val num4ListCo = NumberedListItemCallout(num4Phrase, Seq(""), true)
    val num1CircListCo = NumberedCircleListItemCallout(num1Phrase, Seq(""), false)
    val num2CircListCo = NumberedCircleListItemCallout(num2Phrase, Seq(""), true)
    val num3CircListCo = NumberedCircleListItemCallout(num3Phrase, Seq(""), true)
    val num4CircListCo = NumberedCircleListItemCallout(num4Phrase, Seq(""), true)

    val numberedListGroup = NumberedList(Seq(num1ListCo,num2ListCo,num3ListCo,num4ListCo))
    val numberedCircListGroup = NumberedCircleList(Seq(num1CircListCo,num2CircListCo,num3CircListCo,num4CircListCo))

    val emptyNumberedList = NumberedList(Seq.empty)
    val emptyNumberedCircleList = NumberedCircleList(Seq.empty)

  }

  trait NoteTest extends Test {
    val num1Phrase = Phrase(Vector("Line1", "Welsh Line1"))
    val num2Phrase = Phrase(Vector("Line2", "Welsh Line2"))
    val num3Phrase = Phrase(Vector("Line3", "Welsh Line3"))
    val num4Phrase = Phrase(Vector("Line4", "Welsh Line4"))

    val note1Co = NoteCallout(num1Phrase, Seq(""), false)
    val note2Co = NoteCallout(num2Phrase, Seq(""), true)
    val note3Co = NoteCallout(num3Phrase, Seq(""), true)
    val note4Co = NoteCallout(num4Phrase, Seq(""), true)

    val noteGroup = NoteGroup(Seq(note1Co,note2Co,note3Co,note4Co))

    val emptyNoteGroup = NoteGroup(Seq.empty)
  }

  "UIBuilder" must {
    "Convert a empty number list into a NumberList" in new NumberListTest {
      uiBuilder.buildPage("/start", List(TitleCallout(headingPhrase, Seq.empty, false),
                                                emptyNumberedList)) match {
        case Right(p) =>
          p.components match {
            case Seq(_: H1, _: ui.NumberedList) => succeed
            case x => fail(s"Found $x")
          }
        case Left(_) => fail()
      }
    }

    "Convert a empty numbered circle list into a NumberList" in new NumberListTest {
      val p = uiBuilder.buildPage("/start", List(TitleCallout(headingPhrase, Seq.empty, false),
                                                emptyNumberedCircleList))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: H1, _: ui.NumberedCircleList) => succeed
        case x => fail(s"Found $x")
      }
    }

    "Convert sequence of num list callouts into a numbered list" in new NumberListTest {
      val p = uiBuilder.buildPage("/start", List(TitleCallout(headingPhrase, Seq.empty, false),
                                                num1ListCo,
                                                num2ListCo,
                                                num3ListCo,
                                                num4ListCo
                                              ))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: H1, _: ui.NumberedList) => succeed
        case x => fail(s"Found $x")
      }
    }

    "Convert sequence of num circ list callouts into a numbered circle list" in new NumberListTest {
      val p = uiBuilder.buildPage("/start", List(TitleCallout(headingPhrase, Seq.empty, false),
                                                num1CircListCo,
                                                num2CircListCo,
                                                num3CircListCo,
                                                num4CircListCo
                                              ))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: H1, _: ui.NumberedCircleList) => succeed
        case x => fail(s"Found $x")
      }
    }

    "Convert single of unstacked num list callouts into separate a numbered lists" in new NumberListTest {
      val p = uiBuilder.buildPage("/start", List(TitleCallout(headingPhrase, Seq.empty, false),
                                                num1ListCo
                                              ))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: H1, _: ui.NumberedList) => succeed
        case x => fail(s"Found $x")
      }
    }

    "Convert sequence of unstacked num list callouts into separate a numbered lists" in new NumberListTest {
      val p = uiBuilder.buildPage("/start", List(TitleCallout(headingPhrase, Seq.empty, false),
                                                num1ListCo,
                                                num1ListCo,
                                                num1ListCo
                                              ))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: H1, _: ui.NumberedList, _: ui.NumberedList, _: ui.NumberedList) => succeed
        case x => fail(s"Found $x")
      }
    }

    "Convert sequence of unstacked num circ list callouts into separate a numbered circle lists" in new NumberListTest {
      val p = uiBuilder.buildPage("/start", List(TitleCallout(headingPhrase, Seq.empty, false),
                                                num1CircListCo,
                                                num1CircListCo,
                                                num1CircListCo
                                              ))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: H1, _: ui.NumberedCircleList, _: ui.NumberedCircleList, _: ui.NumberedCircleList) => succeed
        case x => fail(s"Found $x")
      }
    }

    "Empty RowGroup generates an UnsupportedUiPatternError error" in new TableTest {
      uiBuilder.buildPage("/start", List(TitleCallout(headingPhrase, Seq.empty, false), emptyRowGroup)) match {
        case Left(err) if err.errors == List(UnsupportedUiPatternError) => succeed
        case _ => fail()
      }
    }

    "Convert a simple RowGroup into a NameValueSummaryList" in new TableTest {
      val p = uiBuilder.buildPage("/start", List(TitleCallout(headingPhrase, Seq.empty, false),
                                                simpleRowGroup))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: H1, _: ui.NameValueSummaryList) => succeed
        case x => fail(s"Found $x")
      }
    }

    "Convert a non-summarylist RowGroup into a table with a heading line" in new TableTest {
      val p = uiBuilder.buildPage("/start", List(SubSectionCallout(headingPhrase, Seq.empty, false),
                                                tableRowGroup))
      p.fold(_ => fail(), p => p).components match {
        case Seq(Table(Text(h), th, _)) if h.headOption == Some(Words(headingPhrase.value(lang))) => succeed
        case x =>
          fail(s"Found $x")
      }
    }

    "convert a non-summarylist RowGroup stacked to a SubSection into a table with caption and a heading" in new TableTest {
      val p = uiBuilder.buildPage("/start", List(SubSectionCallout(headingPhrase, Seq.empty, false),
                                                tableRowGroup))
      p.fold(_ => fail(), p => p).components match {
        case Seq(Table(Text(h), _, rows)) if h.headOption == Some(Words(headingPhrase.value(lang))) => succeed
        case x => fail(s"Found $x")
      }
    }

    "convert a summarylist RowGroup stacked to a SubSection into an H4 and a NameValueSummaryList" in new TableTest {
      val p = uiBuilder.buildPage("/start", List(SubSectionCallout(headingPhrase, Seq.empty, false),
                                                stackedRowGroup))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: H4, _: ui.NameValueSummaryList) => succeed
        case x => fail(s"Found $x")
      }
    }

    "convert a summarylist RowGroup into a NameValueSummaryList with a right aligned numeric column" in new TableTest {
      val p = uiBuilder.buildPage("/start", List(TitleCallout(headingPhrase, Seq.empty, false), numericRowGroup))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: H1, nvsl: ui.NameValueSummaryList) if nvsl.rows.size == 4 => succeed
        case x => fail(s"Found $x")
      }
    }

    "convert a RowGroup with three sparse columns including a link and hint into a SummaryList" in new SLTest {
      val p = uiBuilder.buildPage("/start", List(TitleCallout(headingPhrase, Seq.empty, false),
                                                RowGroup(Seq("2"), sparseRowsWithLinkAndHint, true)))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: H1, _: CyaSummaryList) => succeed
        case x => fail(s"Found $x")
      }

    }

    "convert a RowGroup with three columns including a link and hint into a SummaryList" in new SLTest {
      val p = uiBuilder.buildPage("/start", List(TitleCallout(headingPhrase, Seq.empty, false),
                                                RowGroup(Seq("2"), rowsWithLinkAndHint, true)))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: H1, _: CyaSummaryList) => succeed
        case x => fail(s"Found $x")
      }

    }

    "convert a RowGroup with three columns into a SummaryList and faked welsh link" in new SLTest {
      val p = uiBuilder.buildPage("/start", List(TitleCallout(headingPhrase, Seq.empty, false),
                                                RowGroup(Seq("2"), rowsWithFakedWelshLinK, true)))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: H1, _: CyaSummaryList) => succeed
        case x => fail(s"Found $x")
      }

    }

    "convert and Ocelot page into a UI page with the same url" in new Test {

      uiBuilder.buildPage(page.url, page.stanzas.toList.collect{case s: VisualStanza => s}) match {
        case Right(p) if p.urlPath == page.url => succeed
        case Right(p) => fail(s"UI page urlPath set incorrectly to ${p.urlPath}")
        case Left(err) => fail(s"Error $err")
      }
    }

    "Unsupported VisualStanzas generates UnsupportedUiPatternError" in new Test {
      val visual = List(UnsupportedVisualStanza(Seq("end"), false))
      uiBuilder.buildPage(page.url, visual) match {
        case Left(err) if err.errors == List(UnsupportedUiPatternError) => succeed
        case _ => fail()
      }
    }

    "convert 1st Callout type Title to H1" in new Test {
      val uiPage = uiBuilder.buildPage(page.url, page.stanzas.toList.collect{case s: VisualStanza => s})
      uiPage.fold(_ => fail(), p => p).components(1) shouldBe models.ui.H1(Text(Phrase(lang0).value(lang)))
    }

    "convert 2nd Callout type SubTitle to H2" in new Test {

      val uiPage = uiBuilder.buildPage(page.url, page.stanzas.toList.collect{case s: VisualStanza => s})
      uiPage.fold(_ => fail(), p => p).components(2) shouldBe models.ui.H2(Text(Phrase(lang1).value(lang)))
    }

    "convert Callout type Lede to lede Paragraph" in new Test {

      val uiPage = uiBuilder.buildPage(page.url, page.stanzas.toList.collect{case s: VisualStanza => s})
      uiPage.fold(_ => fail(), p => p).components(3) shouldBe models.ui.Paragraph(Text(Phrase(lang2).value(lang)), true)
    }

    "Dont convert Callout type Important into an ErrorMsg" in new Test {
      val uiPage = uiBuilder.buildPage(page.url, page.stanzas.toList.collect{case s: VisualStanza => s})
      val errs: Seq[ErrorMsg] = uiPage.fold(_ => fail(), p => p).components.collect{case err: ErrorMsg => err}

      errs.exists{e => e.id == "ID"} shouldBe false
    }

    "convert Callout type ValueError to an ErrorMsg" in new Test {
      val uiPage = uiBuilder.buildPage(page.url, page.stanzas.toList.collect{case s: VisualStanza => s})
      uiPage.fold(_ => fail(), p => p).components(7) shouldBe models.ui.ValueErrorMsg(Text(Phrase(lang0).value(lang)))
    }

    "convert Callout type TypeError to an ErrorMsg" in new Test {
      val uiPage = uiBuilder.buildPage(page.url, page.stanzas.toList.collect{case s: VisualStanza => s}, ValueTypeError)
      uiPage.fold(_ => fail(), p => p).components(8) shouldBe models.ui.TypeErrorMsg(Text(Phrase(lang0).value(lang)))
    }

    "convert Simple instruction to Paragraph" in new Test {

      val uiPage = uiBuilder.buildPage(page.url, page.stanzas.toList.collect{case s: VisualStanza => s})
      uiPage.fold(_ => fail(), p => p).components(four) shouldBe models.ui.Paragraph(Text(Phrase(lang3).value(lang)), false)
    }

    "convert Link instruction to Paragraph" in new Test {

      val uiPage = uiBuilder.buildPage(page.url, page.stanzas.toList.collect{case s: VisualStanza => s})
      uiPage.fold(_ => fail(), p => p).components(five) shouldBe models.ui.Paragraph(Text(Link("dummy-path/blah", Phrase(lang4).value(lang))))
    }

    "convert page with instruction stanza containing a sequence of Text and Link items" in new Test {
      val uiPage = uiBuilder.buildPage(pageWithEmbeddLinks.url, pageWithEmbeddLinks.stanzas.toList.collect{case s: VisualStanza => s})
      uiPage.fold(_ => fail(), p => p).components(five) shouldBe models.ui.Paragraph(textItems, false)
    }

    "convert page with instruction stanza containing a sequence of TextItems beginning and ending with HyperLinks" in new Test {
      val uiPage = uiBuilder.buildPage(pageWithEmbeddLinks2.url, pageWithEmbeddLinks2.stanzas.toList.collect{case s: VisualStanza => s})
      uiPage.fold(_ => fail(), p => p).components(5) shouldBe models.ui.Paragraph(textItems2, false)
    }

    "convert page with instruction stanza text containing PageLinks and Text" in new Test {
      val uiPage = uiBuilder.buildPage(pageWithEmbeddPageLinks.url, pageWithEmbeddPageLinks.stanzas.toList.collect{case s: VisualStanza => s})
      uiPage.fold(_ => fail(), p => p).components(5) shouldBe models.ui.Paragraph(pageLinkTextItems, false)
    }

    "convert Callout type SubSection to H4" in new Test {
      val uiPage = uiBuilder.buildPage(pageWithEmbeddH4.url, pageWithEmbeddH4.stanzas.toList.collect{case s: VisualStanza => s})
      uiPage.fold(_ => fail(), p => p).components(five) shouldBe models.ui.H4(Text(Phrase(lang5).value(lang)))
    }

    "convert page with instruction stanza text containing PageLinks, HyperLinks and Text" in new Test {
      val uiPage = uiBuilder.buildPage(pageWithEmbeddAllLinks.url, pageWithEmbeddAllLinks.stanzas.toList.collect{case s: VisualStanza => s})
      uiPage.fold(_ => fail(), p => p).components(five) shouldBe models.ui.Paragraph(allLinksTextItems, false)
    }

    "convert page including a PageLink instruction stanza" in new Test {
      val uiPage = uiBuilder.buildPage(page.url, page.stanzas.toList.collect{case s: VisualStanza => s})
      uiPage.fold(_ => fail(), p => p).components(five) shouldBe models.ui.Paragraph(Text(link3), false)
    }

    "convert page including a Link instruction stanza" in new Test {
      val uiPage = uiBuilder.buildPage(hyperLinkPage.url, hyperLinkPage.stanzas.toList.collect{case s: VisualStanza => s})
      uiPage.fold(_ => fail(), p => p).components(five) shouldBe models.ui.Paragraph(Text(link4), false)
    }

    "convert a question page into a Seq of a single Question UI object" in new Test {
      val uiPage = uiBuilder.buildPage(questionPage.url, questionPage.stanzas.toList.collect{case s: VisualStanza => s})

      uiPage.fold(_ => fail(), p => p).components.length shouldBe 1

      uiPage.fold(_ => fail(), p => p).components.head match {
        case q: models.ui.Question =>
          q.answers.length shouldBe 3
          q.body.length shouldBe 2
          q.answers.head shouldBe models.ui.Answer(Text(ans1(0)), None)
          q.answers(1) shouldBe models.ui.Answer(Text(ans2(0)), None)
          q.answers(2) shouldBe models.ui.Answer(Text(ans3(0)), None)
        case _ => fail("Found non question UIComponent")
      }
    }

    "convert a question page including answer hints into a Seq of a single Question UI object" in new Test {
      val uiPage = uiBuilder.buildPage(questionPageWithHints.url, questionPageWithHints.stanzas.toList.collect{case s: VisualStanza => s})

      uiPage.fold(_ => fail(), p => p).components.length shouldBe 1

      uiPage.fold(_ => fail(), p => p).components.head match {
        case q: models.ui.Question =>
          q.answers.length shouldBe 3
          q.body.length shouldBe 2
          q.answers.head shouldBe models.ui.Answer(Text(ans1(0)), Some(hint1))
          q.answers(1) shouldBe models.ui.Answer(Text(ans2(0)), Some(hint2))
          q.answers(2) shouldBe models.ui.Answer(Text(ans3(0)), Some(hint3))
        case _ => fail("Found non question UIComponent")
      }
    }

    "Process page with a simple instruction group" in new Test {
      val phrase1: Phrase = Phrase(Vector("My favourite sweets are wine gums", "Fy hoff losin yw deintgig gwin"))
      val phrase2: Phrase = Phrase(Vector("My favourite sweets are humbugs", "Fy hoff losin yw humbugs"))
      val instruction1: Instruction = Instruction(phrase1, Seq("2"), None, true)
      val instruction2: Instruction = Instruction(phrase2, Seq("end"), None, false)
      val instructionGroup: InstructionGroup = InstructionGroup(Seq(instruction1, instruction2))
      val bulletPointListStanzas = Seq(
        KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
        KeyedStanza("1", instructionGroup)
      )
      val bulletPointListPage = Page(Process.StartStanzaId, "/blah", bulletPointListStanzas, Seq.empty)
      val uiPage = uiBuilder.buildPage(bulletPointListPage.url, bulletPointListPage.stanzas.toList.collect{case s: VisualStanza => s})

      uiPage.fold(_ => fail(), p => p).components.length shouldBe 1

      // Check contents of bullet point list
      val leadingTextItems: Text = Text(Words("My favourite sweets are"))
      val bulletPointOne: Text = Text("wine gums")
      val bulletPointTwo: Text = Text("humbugs")

      uiPage.fold(_ => fail(), p => p).components.head match {
        case b: BulletPointList =>

          b.text shouldBe leadingTextItems

          b.listItems.size shouldBe 2

          b.listItems.head shouldBe bulletPointOne
          b.listItems.last shouldBe bulletPointTwo
        case _ => fail("Did not find bullet point list")
      }
    }

    "Process page with a simple instruction group from prototypeJson" in new Test {

      val phrase1: Phrase = Phrase(
        Vector(
          "In some circumstances, you do not have to tell HMRC about extra income you've made. In each tax year you can earn up to £11,000, tax free, if you are: selling goods or services (trading)",
          "Mewn rhai amgylchiadau, nid oes rhaid i chi ddweud wrth Cyllid a Thollau EF am incwm ychwanegol rydych wedi'i wneud. Ymhob blwyddyn dreth gallwch ennill hyd at £ 11,000, yn ddi-dreth, os ydych chi: gwerthu nwyddau neu wasanaethau (masnachu)"
        )
      )
      val phrase2: Phrase = Phrase(
        Vector(
          "In some circumstances, you do not have to tell HMRC about extra income you've made. In each tax year you can earn up to £11,000, tax free, if you are: renting land or property",
          "Mewn rhai amgylchiadau, nid oes rhaid i chi ddweud wrth Cyllid a Thollau EF am incwm ychwanegol rydych wedi'i wneud. Ymhob blwyddyn dreth gallwch ennill hyd at £ 11,000, yn ddi-dreth, os ydych chi: rhentu tir neu eiddo"
        )
      )

      val instruction1: Instruction = Instruction(phrase1, Seq("2"), None, true)
      val instruction2: Instruction = Instruction(phrase2, Seq("end"), None, false)

      val instructionGroup: InstructionGroup = InstructionGroup(Seq(instruction1, instruction2))

      val bulletPointListStanzas = Seq(
        KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
        KeyedStanza("1", instructionGroup)
      )

      val bulletPointListPage = Page(Process.StartStanzaId, "/blah", bulletPointListStanzas, Seq.empty)

      val uiPage = uiBuilder.buildPage(bulletPointListPage.url, bulletPointListPage.stanzas.toList.collect{case s: VisualStanza => s})

      uiPage.fold(_ => fail(), p => p).components.length shouldBe 1

      // Check contents of bullet point list
      val leadingTextItems: Text = Text("In some circumstances, you do not have to tell HMRC about extra income you’ve made. In each tax year you can earn up to £11,000, tax free, if you are:")

      val bulletPointOne: Text = Text("selling goods or services (trading)")
      val bulletPointTwo: Text = Text("renting land or property")

      uiPage.fold(_ => fail(), p => p).components.head match {
        case b: BulletPointList =>
          b.text shouldBe leadingTextItems
          b.listItems.size shouldBe 2
          b.listItems.head shouldBe bulletPointOne
          b.listItems.last shouldBe bulletPointTwo
        case _ => fail("Did not find bullet point list")
      }
    }

    "Process complex page with both instruction groups and single instructions" in new Test {

      val phrase1: Phrase = Phrase(Vector("Going to the market", "Mynd i'r farchnad"))
      val phrase2: Phrase = Phrase(Vector("Fruit and Vegetables", "Ffrwythau a llysiau"))
      val phrase3: Phrase = Phrase(Vector("Vegetables", "Llysiau"))
      val phrase4: Phrase = Phrase(Vector("What you can buy in our lovely vegetable market", "Beth allwch chi ei brynu yn ein marchnad llysiau hyfryd"))
      val phrase5: Phrase = Phrase(Vector("Today we have special parsnips for sale", "Heddiw mae gennym bananas arbennig ar werth"))
      val phrase6: Phrase = Phrase(Vector("Today we have special purple carrots for sale", "Heddiw mae gennym foron porffor arbennig ar werth"))
      val phrase7: Phrase = Phrase(Vector("Today we have special brussels sprouts for sale", "Heddiw mae gennym ysgewyll cregyn gleision arbennig ar werth"))
      val phrase8: Phrase = Phrase(Vector("Thank you", "Diolch"))

      val titleCallout: Callout = TitleCallout(phrase1, Seq("1"), false)
      val instruction1: Instruction = Instruction(phrase2, Seq("2"), None, false)
      val subTitleCallout: Callout = SubTitleCallout(phrase3, Seq("3"), false)
      val instruction2: Instruction = Instruction(phrase4, Seq("4"), None, false)

      val instructionGroupInstruction1: Instruction = Instruction(phrase5, Seq("5"), None, true)
      val instructionGroupInstruction2: Instruction = Instruction(phrase6, Seq("6"), None, false)
      val instructionGroupInstruction3: Instruction = Instruction(phrase7, Seq("7"), None, false)

      val instructionGroup: InstructionGroup = InstructionGroup(Seq(instructionGroupInstruction1, instructionGroupInstruction2, instructionGroupInstruction3))

      val instruction3: Instruction = Instruction(phrase8, Seq("8"), None, false)

      // Build sequence of stanzas
      val stanzaSeq = Seq(
        KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
        KeyedStanza("1", titleCallout),
        KeyedStanza("2", instruction1),
        KeyedStanza("3", subTitleCallout),
        KeyedStanza("4", instruction2),
        KeyedStanza("5", instructionGroup),
        KeyedStanza("6", instruction3)
      )

      val complexPage = Page(Process.StartStanzaId, "/blah", stanzaSeq, Seq.empty)

      val complexUiPage = uiBuilder.buildPage(complexPage.url, complexPage.stanzas.toList.collect{case s: VisualStanza => s})

      complexUiPage.fold(_ => fail(), p => p).components.size shouldBe 6

      // Check contents of bullet point list
      val leadingTextItems: Text = Text("Today we have special")

      val bulletPointOne: Text = Text("parsnips for sale")
      val bulletPointTwo: Text = Text("purple carrots for sale")
      val bulletPointThree: Text = Text("brussels sprouts for sale")

      complexUiPage.fold(_ => fail(), p => p).components(four) match {
        case b: BulletPointList =>

          b.text shouldBe leadingTextItems

          b.listItems.size shouldBe 3

          b.listItems.head shouldBe bulletPointOne
          b.listItems(1) shouldBe bulletPointTwo
          b.listItems.last shouldBe bulletPointThree
        case _ => fail("Did not find bullet point list")
      }

      val finalParagraph: Paragraph = Paragraph(Text("Thank you"))

      complexUiPage.fold(_ => fail(), p => p).components(five) match {
        case p: Paragraph =>
          p shouldBe finalParagraph
        case _ => fail("The last components is not an instruction")
      }
    }

    "Process page with multiple line bullet point list" in new Test {

      val phrase1: Phrase = Phrase(Vector("You must have a tea bag", "Rhaid i chi gael bag te"))
      val phrase2: Phrase = Phrase(Vector("You must have a cup", "Rhaid i chi gael cwpan"))
      val phrase3: Phrase = Phrase(Vector("You must have a teaspoon", "Rhaid i chi gael llwy de"))
      val phrase4: Phrase = Phrase(Vector("You must have water", "Rhaid i chi gael dŵr"))
      val phrase5: Phrase = Phrase(Vector("You must have an electric kettle", "Rhaid bod gennych chi degell trydan"))
      val phrase6: Phrase = Phrase(Vector("You must have an electricity supply", "Rhaid bod gennych gyflenwad trydan"))

      val instruction1: Instruction = Instruction(phrase1, Seq("2"), None, stack = true)
      val instruction2: Instruction = Instruction(phrase2, Seq("3"), None, stack = true)
      val instruction3: Instruction = Instruction(phrase3, Seq("4"), None, stack = true)
      val instruction4: Instruction = Instruction(phrase4, Seq("5"), None, stack = true)
      val instruction5: Instruction = Instruction(phrase5, Seq("6"), None, stack = true)
      val instruction6: Instruction = Instruction(phrase6, Seq("end"), None, stack = true)

      val instructionGroup: InstructionGroup = InstructionGroup(
        Seq(
          instruction1,
          instruction2,
          instruction3,
          instruction4,
          instruction5,
          instruction6
        )
      )

      val bulletPointStanzas = Seq(KeyedStanza("start", PageStanza("/page-1", Seq("1"), false)),
                                   KeyedStanza("1", instructionGroup))

      val bulletPointListPage = Page(Process.StartStanzaId, "/page-1", bulletPointStanzas, Seq.empty)

      val uiPage = uiBuilder.buildPage(bulletPointListPage.url, bulletPointListPage.stanzas.toList.collect{case s: VisualStanza => s})

      uiPage.fold(_ => fail(), p => p).components.length shouldBe 1

      val leadingTextItems: Text = Text("You must have")

      val bulletPointOne: Text = Text("a tea bag")
      val bulletPointTwo: Text = Text("a cup")
      val bulletPointThree: Text = Text("a teaspoon")
      val bulletPointFour: Text = Text("water")
      val bulletPointFive: Text = Text("an electric kettle")
      val bulletPointSix: Text = Text("an electricity supply")

      uiPage.fold(_ => fail(), p => p).components.head match {

        case b: BulletPointList =>

          b.text shouldBe leadingTextItems

          b.listItems.size shouldBe 6

          b.listItems.head shouldBe bulletPointOne
          b.listItems(1) shouldBe bulletPointTwo
          b.listItems(2) shouldBe bulletPointThree
          b.listItems(3) shouldBe bulletPointFour
          b.listItems(four) shouldBe bulletPointFive
          b.listItems.last shouldBe bulletPointSix
        case _ => fail("Did not find bullet point list")
      }

    }

    "Process bullet point list in do you need to tell HMRC about extra income V6" in new Test {
      val ocelotPage = extraIncomeStanzaPages.head
      val visualStanzas: List[VisualStanza] = ocelotPage.stanzas.toList.collect{case s: VisualStanza => s}
      implicit override val ctx: UIContext = UIContext(labels, extraIncomeUrlMap, messages)
      val uiPage = uiBuilder.buildPage(ocelotPage.url, visualStanzas)
      val leadingTextItems: Text = Text("You’ve received income that you have not yet paid tax on from:")
      val bulletPointOne: Text = Text("a business you own or control (such as a partnership or limited company)")
      val bulletPointTwo: Text = Text("a business a relative owns or controls")
      val bulletPointThree: Text = Text("your employer (for example for freelance services outside your normal contract hours)")
      val bulletPointFour: Text = Text("the employer of your spouse or civil partner")

      uiPage.fold(_ => fail(), p => p).components.head match {
        case b: BulletPointList =>

          b.text shouldBe leadingTextItems

          b.listItems.size shouldBe 4

          b.listItems.head shouldBe bulletPointOne
          b.listItems(1) shouldBe bulletPointTwo
          b.listItems(2) shouldBe bulletPointThree
          b.listItems.last shouldBe bulletPointFour

        case  _ => fail("First component should be a bullet point list")
      }
    }

    "Process page with a simple instruction group with break markers" in new Test {

      val phrase1: Phrase = Phrase(Vector("My favourite sweets are[break] wine gums", "Fy hoff losin yw[break] deintgig gwin"))
      val phrase2: Phrase = Phrase(Vector("My favourite sweets are[break] humbugs", "Fy hoff losin yw[break] humbugs"))

      val instruction1: Instruction = Instruction(phrase1, Seq("2"), None, stack = true)
      val instruction2: Instruction = Instruction(phrase2, Seq("end"), None, stack = false)

      val instructionGroup: InstructionGroup = InstructionGroup(Seq(instruction1, instruction2))

      val bulletPointListStanzas = Seq(
        KeyedStanza("start", PageStanza("/blah", Seq("1"), stack = false)),
        KeyedStanza("1", instructionGroup)
      )
      val bulletPointListPage = Page(Process.StartStanzaId, "/blah", bulletPointListStanzas, Seq.empty)
      val uiPage = uiBuilder.buildPage(bulletPointListPage.url, bulletPointListPage.stanzas.toList.collect{case s: VisualStanza => s})

      uiPage.fold(_ => fail(), p => p).components.length shouldBe 1

      // Check contents of bullet point list
      val leadingTextItems: Text = Text(Words("My favourite sweets are"))
      val bulletPointOne: Text = Text("wine gums")
      val bulletPointTwo: Text = Text("humbugs")

      uiPage.fold(_ => fail(), p => p).components.head match {
        case b: BulletPointList =>

          b.text shouldBe leadingTextItems

          b.listItems.size shouldBe 2

          b.listItems.head shouldBe bulletPointOne
          b.listItems.last shouldBe bulletPointTwo
        case _ => fail("Did not find bullet point list")
      }
    }

    "Process page with a simple instruction group where bullet points are links" in new Test {
      val phrase1: Phrase = Phrase(Vector("You can also find out about:[link:tax overpayments and underpayments:https://www.gov.uk/tax-overpayments-and-underpayments]",
                                          "Welsh: You can also find out about:[link:tax overpayments and underpayments:https://www.gov.uk/tax-overpayments-and-underpayments]"))
      val phrase2: Phrase = Phrase(Vector("You can also find out about:[link:tax codes:https://www.gov.uk/tax-codes]",
                                          "Welsh: You can also find out about:[link:tax codes:https://www.gov.uk/tax-codes]"))
      val instruction1: Instruction = Instruction(phrase1, Seq("2"), None, true)
      val instruction2: Instruction = Instruction(phrase2, Seq("end"), None, false)
      val instructionGroup: InstructionGroup = InstructionGroup(Seq(instruction1, instruction2))
      val bulletPointListStanzas = Seq(
        KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
        KeyedStanza("1", instructionGroup)
      )
      val bulletPointListPage = Page(Process.StartStanzaId, "/blah", bulletPointListStanzas, Seq.empty)
      val uiPage = uiBuilder.buildPage(bulletPointListPage.url, bulletPointListPage.stanzas.toList.collect{case s: VisualStanza => s})

      uiPage.fold(_ => fail(), p => p).components.length shouldBe 1

      // Check contents of bullet point list
      val leadingTextItems: Text = Text(Words("You can also find out about:"))
      val bulletPointOne: Text = Text(Link("https://www.gov.uk/tax-overpayments-and-underpayments", "tax overpayments and underpayments"))
      val bulletPointTwo: Text = Text(Link("https://www.gov.uk/tax-codes", "tax codes"))

      uiPage.fold(_ => fail(), p => p).components.head match {
        case b: BulletPointList =>

          b.text shouldBe leadingTextItems

          b.listItems.size shouldBe 2

          b.listItems.head shouldBe bulletPointOne
          b.listItems.last shouldBe bulletPointTwo
        case _ => fail("Did not find bullet point list")
      }
    }

    "Process complex page with defined instruction groups and single instructions" in new Test {

      val phrase1: Phrase = Phrase(Vector("Going to the market", "Welsh: Going to the market"))
      val phrase2: Phrase = Phrase(Vector("Fruit and Vegetables", "Welsh: Fruit and Vegetables"))
      val phrase3: Phrase = Phrase(Vector("Vegetables", "Welsh: Vegetables"))
      val phrase4: Phrase = Phrase(Vector("What you can buy in our lovely vegetable market", "Welsh: What you can buy in our lovely vegetable market"))
      val phrase5: Phrase = Phrase(Vector("Today we have special[break] parsnips for sale", "Welsh: Today we have very special[break] parsnips for sale"))
      val phrase6: Phrase = Phrase(
        Vector("Today we have special[break] purple carrots for sale", "Welsh: Today we have very special[break] purple carrots for sale")
      )
      val phrase7: Phrase = Phrase(
        Vector("Today we have special[break] brussels sprouts for sale", "Welsh: Today we have very special[break] brussels sprouts for sale")
      )
      val phrase8: Phrase = Phrase(Vector("Thank you", "Diolch"))

      val titleCallout: Callout = TitleCallout(phrase1, Seq("1"), stack = false)
      val instruction1: Instruction = Instruction(phrase2, Seq("2"), None, stack = false)
      val subTitleCallout: Callout = SubTitleCallout(phrase3, Seq("3"), stack = false)
      val instruction2: Instruction = Instruction(phrase4, Seq("4"), None, stack = false)

      val instructionGroupInstruction1: Instruction = Instruction(phrase5, Seq("5"), None, stack = true)
      val instructionGroupInstruction2: Instruction = Instruction(phrase6, Seq("6"), None, stack = false)
      val instructionGroupInstruction3: Instruction = Instruction(phrase7, Seq("7"), None, stack = false)

      val instructionGroup: InstructionGroup = InstructionGroup(Seq(instructionGroupInstruction1, instructionGroupInstruction2, instructionGroupInstruction3))

      val instruction3: Instruction = Instruction(phrase8, Seq("8"), None, stack = false)

      // Build sequence of stanzas
      val stanzaSeq = Seq(
        KeyedStanza("start", PageStanza("/blah", Seq("1"), stack = false)),
        KeyedStanza("1", titleCallout),
        KeyedStanza("2", instruction1),
        KeyedStanza("3", subTitleCallout),
        KeyedStanza("4", instruction2),
        KeyedStanza("5", instructionGroup),
        KeyedStanza("6", instruction3)
      )

      val complexPage = Page(Process.StartStanzaId, "/blah", stanzaSeq, Seq.empty)

      val complexUiPage = uiBuilder.buildPage(complexPage.url, complexPage.stanzas.toList.collect{case s: VisualStanza => s})

      complexUiPage.fold(_ => fail(), p => p).components.size shouldBe 6

      // Check contents of bullet point list
      val leadingTextItems: Text = Text("Today we have special")

      val bulletPointOne: Text = Text("parsnips for sale")
      val bulletPointTwo: Text = Text("purple carrots for sale")
      val bulletPointThree: Text = Text("brussels sprouts for sale")

      complexUiPage.fold(_ => fail(), p => p).components(four) match {
        case b: BulletPointList =>

          b.text shouldBe leadingTextItems

          b.listItems.size shouldBe 3

          b.listItems.head shouldBe bulletPointOne
          b.listItems(1) shouldBe bulletPointTwo
          b.listItems.last shouldBe bulletPointThree
        case _ => fail("Did not find bullet point list")
      }

      val finalParagraph: Paragraph = Paragraph(Text("Thank you"))

      complexUiPage.fold(_ => fail(), p => p).components(five) match {
        case p: Paragraph =>
          p shouldBe finalParagraph
        case _ => fail("The last components is not an instruction")
      }
    }

    "Process page with multiple line bullet point list defined using a break marker" in new Test {

      val phrase1: Phrase = Phrase(Vector("You must have[break] a tea bag", "Welsh: You require[break] a tea bag"))
      val phrase2: Phrase = Phrase(Vector("You must have[break] a cup", "Welsh: You require[break] a cup"))
      val phrase3: Phrase = Phrase(Vector("You must have[break] a teaspoon", "Welsh: You require[break] a teaspoon"))
      val phrase4: Phrase = Phrase(Vector("You must have[break] water", "Welsh: You require[break] water"))
      val phrase5: Phrase = Phrase(Vector("You must have[break] an electric kettle", "Welsh: You require[break] an electric kettle"))
      val phrase6: Phrase = Phrase(Vector("You must have[break] an electricity supply", "Welsh: You require[break] an electricity supply"))

      val instruction1: Instruction = Instruction(phrase1, Seq("2"), None, stack = true)
      val instruction2: Instruction = Instruction(phrase2, Seq("3"), None, stack = true)
      val instruction3: Instruction = Instruction(phrase3, Seq("4"), None, stack = true)
      val instruction4: Instruction = Instruction(phrase4, Seq("5"), None, stack = true)
      val instruction5: Instruction = Instruction(phrase5, Seq("6"), None, stack = true)
      val instruction6: Instruction = Instruction(phrase6, Seq("end"), None, stack = true)

      val instructionGroup: InstructionGroup = InstructionGroup(
        Seq(
          instruction1,
          instruction2,
          instruction3,
          instruction4,
          instruction5,
          instruction6
        )
      )

      val bulletPointStanzas = Seq(KeyedStanza("start", PageStanza("/page-1", Seq("1"), stack = false)),
        KeyedStanza("1", instructionGroup))

      val bulletPointListPage = Page(Process.StartStanzaId, "/page-1", bulletPointStanzas, Seq.empty)

      val uiPage = uiBuilder.buildPage(bulletPointListPage.url, bulletPointListPage.stanzas.toList.collect{case s: VisualStanza => s})

      uiPage.fold(_ => fail(), p => p).components.length shouldBe 1

      val leadingTextItems: Text = Text("You must have")

      val bulletPointOne: Text = Text("a tea bag")
      val bulletPointTwo: Text = Text("a cup")
      val bulletPointThree: Text = Text("a teaspoon")
      val bulletPointFour: Text = Text("water")
      val bulletPointFive: Text = Text("an electric kettle")
      val bulletPointSix: Text = Text("an electricity supply")

      uiPage.fold(_ => fail(), p => p).components.head match {

        case b: BulletPointList =>

          b.text shouldBe leadingTextItems

          b.listItems.size shouldBe 6

          b.listItems.head shouldBe bulletPointOne
          b.listItems(1) shouldBe bulletPointTwo
          b.listItems(2) shouldBe bulletPointThree
          b.listItems(3) shouldBe bulletPointFour
          b.listItems(four) shouldBe bulletPointFive
          b.listItems.last shouldBe bulletPointSix
        case _ => fail("Did not find bullet point list")
      }

    }

    "Convert a empty note list into a InsetText" in new NoteTest {
      val p = uiBuilder.buildPage("/start", List(emptyNoteGroup))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: InsetText) => succeed
        case x => fail(s"Found $x")
      }
    }

    "Convert sequence of note callouts into a single inset text" in new NoteTest {
      val p = uiBuilder.buildPage("/start", List(note1Co, note2Co, note3Co, note4Co))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: InsetText) => succeed
        case x => fail(s"Found $x")
      }
    }

    "Convert single unstacked note callout into separate inset text" in new NoteTest {
      val p = uiBuilder.buildPage("/start", List(note1Co))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: InsetText) => succeed
        case x => fail(s"Found $x")
      }
    }

    "Convert sequence of unstacked note callouts into separate inset texts" in new NoteTest {
      val p = uiBuilder.buildPage("/start", List(note1Co, note1Co, note1Co))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: InsetText, _: InsetText, _: InsetText) => succeed
        case x => fail(s"Found $x")
      }
    }
  }

  trait InputTest extends BaseTest {
    implicit val urlMap: Map[String, PageDesc] =
      Map(
        Process.StartStanzaId -> PageDesc(Process.StartStanzaId, "/blah"),
        "3" -> PageDesc("3", "dummy-path"),
        "4" -> PageDesc("4", "dummy-path/question"),
        "5" -> PageDesc("5", "dummy-path/blah"),
        "6" -> PageDesc("6", "dummy-path/anotherquestion"),
        "34" -> PageDesc("34", "dummy-path/next")
      )
    val inputNext = Seq("4")
    val inputPhrase: Phrase = Phrase(Vector("Some Text", "Welsh: Some Text"))
    val helpPhrase: Phrase = Phrase(Vector("Help text", "Welsh: Help text"))

    val stanzas = Seq(
      KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
      KeyedStanza("1", ErrorCallout(Phrase(Vector("Some Text", "Welsh: Some Text")), Seq("3"), false)),
      KeyedStanza("3", SectionCallout(Phrase(Vector("Some Text", "Welsh: Some Text")), Seq("4"), false)),
      KeyedStanza("4", Instruction(Phrase(Vector("Some Text", "Welsh: Some Text")), Seq("end"), None, false))
    )
    val input1 = core.models.ocelot.stanzas.CurrencyInput(inputNext, inputPhrase, Some(helpPhrase), label ="input1", None, stack = false)
    val page = Page(Process.StartStanzaId, "/test-page", stanzas :+ KeyedStanza("5", input1), Seq.empty)
    val inputCurrencyPoundsOnly = core.models.ocelot.stanzas.CurrencyPoundsOnlyInput(inputNext, inputPhrase, Some(helpPhrase), label ="inputPounds", None, stack = false)
    val pagePoundsOnly = Page(Process.StartStanzaId, "/test-page", stanzas :+ KeyedStanza("5", inputCurrencyPoundsOnly), Seq.empty)
    val inputText = core.models.ocelot.stanzas.TextInput(inputNext, inputPhrase, Some(helpPhrase), label ="inputText", None, stack = false)
    val pageText = Page(Process.StartStanzaId, "/test-page", stanzas :+ KeyedStanza("5", inputText), Seq.empty)
    val inputNumber = core.models.ocelot.stanzas.NumberInput(inputNext, inputPhrase, Some(helpPhrase), label ="inputNumber", None, stack = false)
    val pageNumber = Page(Process.StartStanzaId, "/test-page", stanzas :+ KeyedStanza("5", inputNumber), Seq.empty)

    val uiBuilder: UIBuilder = new UIBuilder()
    implicit val ctx: UIContext = UIContext(labels, urlMap, messages)
    val four: Int = 4
  }

  "UIBuilder TextInput Input processing" must {

    "Ignore Error Callouts when there are no errors" in new InputTest {
      uiBuilder.buildPage(pageText.url, pageText.stanzas.toList.collect{case s: VisualStanza => s}) match {
        case Right(s: FormPage) if s.formComponent.errorMsgs.isEmpty => succeed
        case Right(_: FormPage) => fail("No error messages should be included on page")
        case x => fail(s"Should return FormPage: found $x")
      }
    }

    "Include Error messages when there are errors" in new InputTest {

      uiBuilder.buildPage(pageText.url, pageText.stanzas.toList.collect{case s: VisualStanza => s}, ValueMissingError) match {
        case Right(s: FormPage) if s.formComponent.errorMsgs.isEmpty => fail("No error messages found on page")
        case Right(_: FormPage) => succeed
        case x => fail(s"Should return FormPage: found $x")
      }
    }

    "Maintain order of components within an Input" in new InputTest {
      uiBuilder.buildPage(pageText.url, pageText.stanzas.toList.collect{case s: VisualStanza => s}) match {
        case Right(i: FormPage) =>
          i.formComponent.body(0) match {
            case _: H3 => succeed
            case _ => fail("Ordering of input body components not maintained")
          }
          i.formComponent.body(1) match {
            case _: Paragraph => succeed
            case _ => fail("Ordering of input body components not maintained")
          }

        case x => fail(s"Should return FormPage: found $x")
      }
    }

    "Include a page hint appended to the input text" in new InputTest {
      uiBuilder.buildPage(pageText.url, pageText.stanzas.toList.collect{case s: VisualStanza => s}) match {
        case Right(i: FormPage) if i.formComponent.hint == Some(Text("Help text")) => succeed
        case Right(_: FormPage) => fail("No hint found within Input")
        case x => fail(s"Should return FormPage: found $x")
      }
    }
  }

  "UIBuilder NumberInput Input processing" must {

    "Ignore Error Callouts when there are no errors" in new InputTest {
      uiBuilder.buildPage(pageNumber.url, pageNumber.stanzas.toList.collect{case s: VisualStanza => s}) match {
        case Right(s: FormPage) if s.formComponent.errorMsgs.isEmpty => succeed
        case Right(_: FormPage) => fail("No error messages should be included on page")
        case x => fail(s"Should return FormPage: found $x")
      }
    }

    "Include Error messages when there are errors" in new InputTest {

      uiBuilder.buildPage(pageNumber.url, pageNumber.stanzas.toList.collect{case s: VisualStanza => s}, ValueMissingError) match {
        case Right(s: FormPage) if s.formComponent.errorMsgs.isEmpty => fail("No error messages found on page")
        case Right(_: FormPage) => succeed
        case x => fail(s"Should return FormPage: found $x")
      }
    }

    "Maintain order of components within an Input" in new InputTest {
      uiBuilder.buildPage(pageNumber.url, pageNumber.stanzas.toList.collect{case s: VisualStanza => s}) match {
        case Right(i: FormPage) =>
          i.formComponent.body(0) match {
            case _: H3 => succeed
            case _ => fail("Ordering of input body components not maintained")
          }
          i.formComponent.body(1) match {
            case _: Paragraph => succeed
            case _ => fail("Ordering of input body components not maintained")
          }

        case x => fail(s"Should return FormPage: found $x")
      }
    }

    "Include a page hint appended to the input text" in new InputTest {
      uiBuilder.buildPage(pageNumber.url, pageNumber.stanzas.toList.collect{case s: VisualStanza => s}) match {
        case Right(i: FormPage) if i.formComponent.hint == Some(Text("Help text")) => succeed
        case Right(_: FormPage) => fail("No hint found within Input")
        case x => fail(s"Should return FormPage: found $x")
      }
    }
  }

  "UIBuilder Currency Input processing" must {

    "Ignore Error Callouts when there are no errors" in new InputTest {
      uiBuilder.buildPage(page.url, page.stanzas.toList.collect{case s: VisualStanza => s}) match {
        case Right(s: FormPage) if s.formComponent.errorMsgs.isEmpty => succeed
        case Right(_: FormPage) => fail("No error messages should be included on page")
        case x => fail(s"Should return FormPage: found $x")
      }
    }

    "Include Error messages when there are errors" in new InputTest {

      uiBuilder.buildPage(page.url, page.stanzas.toList.collect{case s: VisualStanza => s}, ValueMissingError) match {
        case Right(s: FormPage) if s.formComponent.errorMsgs.isEmpty => fail("No error messages found on page")
        case Right(_: FormPage) => succeed
        case x => fail(s"Should return FormPage: found $x")
      }
    }

    "Maintain order of components within an Input" in new InputTest {
      uiBuilder.buildPage(page.url, page.stanzas.toList.collect{case s: VisualStanza => s}) match {
        case Right(i: FormPage) =>
          i.formComponent.body(0) match {
            case _: H3 => succeed
            case _ => fail("Ordering of input body components not maintained")
          }
          i.formComponent.body(1) match {
            case _: Paragraph => succeed
            case _ => fail("Ordering of input body components not maintained")
          }

        case x => fail(s"Should return FormPage: found $x")
      }
    }

    "Include a page hint appended to the input text" in new InputTest {
      uiBuilder.buildPage(page.url, page.stanzas.toList.collect{case s: VisualStanza => s}) match {
        case Right(i: FormPage) if i.formComponent.hint == Some(Text("Help text")) => succeed
        case Right(_: FormPage) => fail("No hint found within Input")
        case x => fail(s"Should return FormPage: found $x")
      }
    }
  }

  "UIBuilder CurrencyPoundsOnly Input processing" must {
    "Ignore Error Callouts when there are no errors" in new InputTest {
      uiBuilder.buildPage(pagePoundsOnly.url, pagePoundsOnly.stanzas.toList.collect{case s: VisualStanza => s}) match {
        case Right(s: FormPage) if s.formComponent.errorMsgs.isEmpty => succeed
        case Right(_: FormPage) => fail("No error messages should be included on page")
        case x => fail(s"Should return FormPage: found $x")
      }
    }

    "Include Error messages when there are errors" in new InputTest {
      uiBuilder.buildPage(pagePoundsOnly.url, pagePoundsOnly.stanzas.toList.collect{case s: VisualStanza => s}, ValueMissingError) match {
        case Right(s: FormPage) if s.formComponent.errorMsgs.isEmpty => fail("No error messages found on page")
        case Right(_: FormPage) => succeed
        case x => fail(s"Should return FormPage: found $x")
      }
    }

    "Maintain order of components within an Input" in new InputTest {
      uiBuilder.buildPage(pagePoundsOnly.url, pagePoundsOnly.stanzas.toList.collect{case s: VisualStanza => s}) match {
        case Right(i: FormPage) =>
          i.formComponent.body(0) match {
            case _: H3 => succeed
            case _ => fail("Ordering of input body components not maintained")
          }
          i.formComponent.body(1) match {
            case _: Paragraph => succeed
            case _ => fail("Ordering of input body components not maintained")
          }

        case x => fail(s"Should return FormPage: found $x")
      }

    }

    "Include a page hint appended to the input text" in new InputTest {
      uiBuilder.buildPage(pagePoundsOnly.url, pagePoundsOnly.stanzas.toList.collect{case s: VisualStanza => s}) match {
        case Right(i: FormPage) if i.formComponent.hint == Some(Text("Help text")) => succeed
        case Right(_: FormPage) => fail("No hint found within Input")
        case x => fail(s"Should return FormPage: found $x")
      }
    }
  }

  trait ConfirmationPanelTest extends BaseTest {
    implicit val urlMap: Map[String, PageDesc] =
      Map(
        Process.StartStanzaId -> PageDesc(Process.StartStanzaId, "/page-1"),
        "3" -> PageDesc("3", "dummy-path"),
        "4" -> PageDesc("4", "dummy-path/input"),
        "5" -> PageDesc("5", "dummy-path/blah"),
        "6" -> PageDesc("6", "dummy-path/anotherinput"),
        "34" -> PageDesc("34", "dummy-path/next")
      )

    implicit val ctx: UIContext = UIContext(labels, urlMap, messages)

    val confirmationPanelHeaderPhrase: Phrase = Phrase(Vector("Confirmation", "Welsh: Confirmation"))
    val confirmationPanelAdditionalText1Phrase: Phrase =  Phrase(Vector("Additional line 1", "Welsh: Additional line 1"))
    val confirmationPanelAdditionalText2Phrase: Phrase = Phrase(Vector("Additional line 2", "Welsh: Additional line 2"))
    val instruction1Phrase: Phrase = Phrase(Vector("Instruction one", "Welsh: Instruction one"))
    val instruction2Phrase: Phrase = Phrase(Vector("Instruction two", "Welsh: Instruction two"))
    val sectionCallOutPhrase: Phrase = Phrase(Vector("Section title", "Welsh: Section title"))
    val subSectionCalloutPhrase: Phrase = Phrase(Vector("Subsection title", "Welsh: Subsection title"))

    val confirmationPanelHeader: Callout = YourCallCallout(confirmationPanelHeaderPhrase, Seq("2"), stack = false)
    val stackedConfirmationPanelHeader: Callout = YourCallCallout(confirmationPanelHeaderPhrase, Seq("2"), stack = true)
    val confirmationPanelAdditional1: Callout = YourCallCallout(confirmationPanelAdditionalText1Phrase, Seq("4"), stack = true)
    val confirmationPanelAdditional2: Callout = YourCallCallout(confirmationPanelAdditionalText2Phrase, Seq("5"), stack = true)
    val instruction1: Instruction = Instruction(instruction1Phrase, Seq("6"), None, stack = false)
    val stackedInstruction1: Instruction = Instruction(instruction1Phrase, Seq("6"), None, stack = true)
    val instruction2: Instruction = Instruction(instruction2Phrase, Seq("7"), None, stack = false)
    val sectionCallout: Callout = SectionCallout(sectionCallOutPhrase, Seq("8"), stack = true)
    val subSectionCallout: Callout = SubSectionCallout(subSectionCalloutPhrase, Seq("8"), stack = true)

    val confirmationPanelHeaderText: Text = TextBuilder.fromPhrase(confirmationPanelHeaderPhrase)
    val confirmationPanelAdditional1Text: Text = TextBuilder.fromPhrase(confirmationPanelAdditionalText1Phrase)
    val confirmationPanelAdditional2Text: Text = TextBuilder.fromPhrase(confirmationPanelAdditionalText2Phrase)
    val instruction1Text: Text = TextBuilder.fromPhrase(instruction1Phrase)
    val instruction2Text: Text = TextBuilder.fromPhrase(instruction2Phrase)
    val sectionCalloutText: Text = TextBuilder.fromPhrase(sectionCallOutPhrase)
    val subSectionCalloutText: Text = TextBuilder.fromPhrase(subSectionCalloutPhrase)

    val uiBuilder: UIBuilder = new UIBuilder()

  }

  "UIBuilder confirmation panel processing" must {

    "create a title only confirmation panel for a single unstacked your call callout" in new ConfirmationPanelTest {

      val stanzas: List[VisualStanza] = List(
        confirmationPanelHeader,
        instruction1,
        instruction2
      )

      val page = uiBuilder.buildPage("/page-1", stanzas)

      page.fold(_ => fail(), p => p).components.head shouldBe ConfirmationPanel(TextBuilder.fromPhrase(confirmationPanelHeaderPhrase))
      page.fold(_ => fail(), p => p).components(1) shouldBe Paragraph(instruction1Text)
      page.fold(_ => fail(), p => p).components.last shouldBe Paragraph(instruction2Text)
    }

    "create a full confirmation panel from three stacked your call callouts" in new ConfirmationPanelTest {

      val stanzas: List[VisualStanza] = List(
        confirmationPanelHeader,
        confirmationPanelAdditional1,
        confirmationPanelAdditional2,
        instruction1,
        instruction2
      )

      val expectedConfirmationPanel: ConfirmationPanel = ConfirmationPanel(
        confirmationPanelHeaderText,
        Seq(
          confirmationPanelAdditional1Text,
          confirmationPanelAdditional2Text
        )
      )

      val page = uiBuilder.buildPage("/page-1", stanzas)

      page.fold(_ => fail(), p => p).components.head shouldBe expectedConfirmationPanel
      page.fold(_ => fail(), p => p).components(1) shouldBe Paragraph(instruction1Text)
      page.fold(_ => fail(), p => p).components.last shouldBe Paragraph(instruction2Text)
    }

    "create a full confirmation panel from 2 YourCall callouts with extra stacked after an instruction" in new ConfirmationPanelTest {

      val stanzas: List[VisualStanza] = List(
        confirmationPanelHeader,
        confirmationPanelAdditional1,
        instruction1.copy(stack = true),
        confirmationPanelAdditional2,
        instruction1,
        instruction2
      )

      val expectedConfirmationPanel: ConfirmationPanel = ConfirmationPanel(
        confirmationPanelHeaderText,
        Seq(confirmationPanelAdditional1Text)
      )

      val page = uiBuilder.buildPage("/page-1", stanzas)

      page.fold(_ => fail(), p => p).components.head shouldBe expectedConfirmationPanel
    }

    "create a two text items confirmation panel followed by an instruction from two stacked your call callouts and an instruction" in new ConfirmationPanelTest {

      val stanzas: List[VisualStanza] = List(
        confirmationPanelHeader,
        confirmationPanelAdditional1,
        instruction1
      )

      val page = uiBuilder.buildPage("/page-1", stanzas)

      val expectedConfirmationPanel: ConfirmationPanel = ConfirmationPanel(
        confirmationPanelHeaderText,
        Seq(confirmationPanelAdditional1Text)
      )

      page.fold(_ => fail(), p => p).components.head shouldBe expectedConfirmationPanel

      page.fold(_ => fail(), p => p).components(1) shouldBe Paragraph(instruction1Text)
    }

    "create single text item confirmation panel from stacked your call and section callouts" in new ConfirmationPanelTest {

      val stanzas: List[VisualStanza] = List(
        confirmationPanelHeader,
        sectionCallout
      )

      val page = uiBuilder.buildPage("/page-1", stanzas)

      page.fold(_ => fail(), p => p).components.head shouldBe ConfirmationPanel(confirmationPanelHeaderText)
      page.fold(_ => fail(), p => p).components.last shouldBe H3(sectionCalloutText)
    }

    "create paragraph and single item confirmation panel from stacked instruction and your call callout" in new ConfirmationPanelTest {

      val stanzas: List[VisualStanza] = List(
        instruction2,
        stackedConfirmationPanelHeader
      )

      val page = uiBuilder.buildPage("/page-1", stanzas)

      page.fold(_ => fail(), p => p).components.head shouldBe Paragraph(instruction2Text)
      page.fold(_ => fail(), p => p).components.last shouldBe ConfirmationPanel(confirmationPanelHeaderText)
    }

    "process stacked group with two your call callouts followed by two other visual stanzas" in new ConfirmationPanelTest {

      val stanzas: List[VisualStanza] = List(
        confirmationPanelHeader,
        confirmationPanelAdditional2,
        sectionCallout,
        stackedInstruction1
      )

      val page = uiBuilder.buildPage("/page-1", stanzas)

      val expectedConfirmationPanel: ConfirmationPanel = ConfirmationPanel(
        confirmationPanelHeaderText,
        Seq(confirmationPanelAdditional2Text)
      )

      page.fold(_ => fail(), p => p).components.head shouldBe expectedConfirmationPanel
      page.fold(_ => fail(), p => p).components(1) shouldBe H3(sectionCalloutText)
      page.fold(_ => fail(), p => p).components.last shouldBe Paragraph(instruction1Text)
    }

    "process stacked group with three your call callouts sandwiched within other visual stanza types" in new ConfirmationPanelTest {

      val stanzas: List[VisualStanza] = List(
        instruction2,
        stackedConfirmationPanelHeader,
        confirmationPanelAdditional1,
        confirmationPanelAdditional2,
        stackedInstruction1
      )

      val page = uiBuilder.buildPage("/page-1", stanzas)

      val expectedConfirmationPanel: ConfirmationPanel = ConfirmationPanel(
        confirmationPanelHeaderText,
        Seq(
          confirmationPanelAdditional1Text,
          confirmationPanelAdditional2Text
        )
      )

      page.fold(_ => fail(), p => p).components.head shouldBe Paragraph(instruction2Text)
      page.fold(_ => fail(), p => p).components(1) shouldBe expectedConfirmationPanel
      page.fold(_ => fail(), p => p).components.last shouldBe Paragraph(instruction1Text)
    }

    "process stacked group with three your call callouts preceded by two other visual stanza types" in new ConfirmationPanelTest {
      val stanzas: List[VisualStanza] = List(
        instruction1,
        subSectionCallout,
        stackedConfirmationPanelHeader,
        confirmationPanelAdditional1,
        confirmationPanelAdditional2
      )
      val page = uiBuilder.buildPage("/page-1", stanzas)
      val expectedConfirmationPanel: ConfirmationPanel = ConfirmationPanel(
        confirmationPanelHeaderText,
        Seq(
          confirmationPanelAdditional1Text,
          confirmationPanelAdditional2Text)
      )

      page.fold(_ => fail(), p => p).components.head shouldBe Paragraph(instruction1Text)
      page.fold(_ => fail(), p => p).components(1) shouldBe H4(subSectionCalloutText)
      page.fold(_ => fail(), p => p).components.last shouldBe expectedConfirmationPanel
    }
  }

  "UIBuilder Date Input processing" must {
    trait DateInputTest extends BaseTest {
      implicit val urlMap: Map[String, PageDesc] =
        Map(
          Process.StartStanzaId -> PageDesc(Process.StartStanzaId, "/blah"),
          "3" -> PageDesc("3", "dummy-path"),
          "4" -> PageDesc("4", "dummy-path/input"),
          "5" -> PageDesc("5", "dummy-path/blah"),
          "6" -> PageDesc("6", "dummy-path/anotherinput"),
          "34" -> PageDesc("34", "dummy-path/next")
        )
      val inputNext = Seq("4")
      val inputPhrase: Phrase = Phrase(Vector("Some Text", "Welsh: Some Text"))
      val helpPhrase: Phrase = Phrase(Vector("Help text", "Welsh: Help text"))
      val stanzas = Seq(
        KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
        KeyedStanza("1", ErrorCallout(Phrase(Vector("Some Error Text", "Welsh: Some Error Text")), Seq("11"), false)),
        KeyedStanza("11", ErrorCallout(Phrase(Vector("Some Error Text {0}", "Welsh: Some Error Text {0}")), Seq("111"), true)),
        KeyedStanza("111", ErrorCallout(Phrase(Vector("Some Error Text {0} and {1}", "Welsh: Some Error Text {0} and {1}")), Seq("3"), true)),
        KeyedStanza("3", SectionCallout(Phrase(Vector("Some Text", "Welsh: Some Text")), Seq("4"), false)),
        KeyedStanza("4", Instruction(Phrase(Vector("Some Text", "Welsh: Some Text")), Seq("end"), None, false))
      )
      val dateInput = DateInput(inputNext, inputPhrase, Some(helpPhrase), label ="input1", None, stack = false)
      val datePage = Page(Process.StartStanzaId, "/test-page", stanzas :+ KeyedStanza("5", dateInput), Seq.empty)
      val uiBuilder: UIBuilder = new UIBuilder()

      implicit val ctx: UIContext = UIContext(labels, urlMap, messages)
    }

    "Ignore Error Callouts when there are no errors" in new DateInputTest {
      uiBuilder.buildPage(datePage.url, datePage.stanzas.toList.collect{case s: VisualStanza => s}) match {
        case Right(s: FormPage) if s.formComponent.errorMsgs.isEmpty => succeed
        case Right(_: FormPage) => fail("No error messages should be included on page")
        case x => fail(s"Should return FormPage: found $x")
      }
    }

    "Include correct Error messages when all fields missing" in new DateInputTest {
      uiBuilder.buildPage(datePage.url, datePage.stanzas.toList.collect{case s: VisualStanza => s}, ValueMissingGroupError(Nil)) match {
        case Right(s: FormPage) if s.formComponent.errorMsgs.isEmpty => fail("No error messages found on page")
        case Right(s: FormPage) => succeed
        case x => fail(s"Should return FormPage: found $x")
      }
    }

    "Include correct Error message for one missing field" in new DateInputTest {
      uiBuilder.buildPage(datePage.url, datePage.stanzas.toList.collect{case s: VisualStanza => s}, ValueMissingGroupError(List("Year"))) match {
        case Right(s: FormPage) if s.formComponent.errorMsgs.isEmpty => fail("No error messages found on page")
        case Right(s: FormPage) => s.formComponent.errorMsgs.headOption shouldBe Some(RequiredErrorMsg(Text("Some Error Text Year")))
        case x => fail(s"Should return FormPage: found $x")
      }
    }

    "Include correct Error message for two missing fields" in new DateInputTest {
      uiBuilder.buildPage(datePage.url, datePage.stanzas.toList.collect{case s: VisualStanza => s}, ValueMissingGroupError(List("Day", "Year"))) match {
        case Right(s: FormPage) if s.formComponent.errorMsgs.isEmpty => fail("No error messages found on page")
        case Right(s: FormPage) => s.formComponent.errorMsgs.headOption shouldBe Some(RequiredErrorMsg(Text("Some Error Text Day and Year")))
        case x => fail(s"Should return FormPage: found $x")
      }
    }

    "Maintain order of components within an Input" in new DateInputTest {
      uiBuilder.buildPage(datePage.url, datePage.stanzas.toList.collect{case s: VisualStanza => s}) match {
        case Right(i: FormPage) =>
          i.formComponent.body(0) match {
            case _: H3 => succeed
            case _ => fail("Ordering of input body components not maintained")
          }
          i.formComponent.body(1) match {
            case _: Paragraph => succeed
            case _ => fail("Ordering of input body components not maintained")
          }

        case x => fail(s"Should return FormPage: found $x")
      }
    }

    "Include a page hint appended to the input text" in new DateInputTest {
      uiBuilder.buildPage(datePage.url, datePage.stanzas.toList.collect{case s: VisualStanza => s}) match {
        case Right(i: FormPage) if i.formComponent.hint == Some(Text("Help text")) => succeed
        case Right(_: FormPage) => fail("No hint found within Input")
        case x => fail(s"Should return FormPage: found $x")
      }
    }
  }

  "UIBuilder Details Component processing" must {
    trait DetailsTest extends Test {
      val sectionPhrase = Phrase(Vector("Visible text", "Welsh visible Text"))
      val num1Phrase = Phrase(Vector("Line1", "Welsh Line1"))
      val num2Phrase = Phrase(Vector("Line2", "Welsh Line2"))
      val num3Phrase = Phrase(Vector("Line3", "Welsh Line3"))
      val num4Phrase = Phrase(Vector("Line4", "Welsh Line4"))
      val stackedNote1 = NoteCallout(num1Phrase, Seq(""), stack = true)
      val stackedNote2 = NoteCallout(num2Phrase, Seq(""), stack = true)
      val unstackedNote = NoteCallout(num2Phrase, Seq(""), stack = false)
      val subSectionCallout: Callout = SubSectionCallout(sectionPhrase, Seq("8"), stack = false)

      // Define callouts for details with bullet point lists
      val detailSectionPhrase: Phrase = Phrase("Bullet point detail", "Welsh: Bullet point detail")

      val startPhrase: Phrase = Phrase("Start", "Welsh: Start")
      val bpl1Phrase1: Phrase = Phrase(
        "The days of the week include Monday",
        "Welsh: The days of the week include Monday"
      )
      val bpl1Phrase2: Phrase = Phrase(
        "The days of the week include Tuesday",
        "Welsh: The days of the week include Tuesday"
      )
      val bpl1Phrase3: Phrase = Phrase(
        "The days of the week include Wednesday",
        "Welsh: The days of the week include Wednesday"
      )
      val endPhrase: Phrase = Phrase("End", "Welsh: End")
      val bpl2Phrase1: Phrase = Phrase(
        "You can apply for a job",
        "Welsh: You can apply for a job"
      )
      val bpl2Phrase2: Phrase = Phrase(
        "You can apply for an apprenticeship",
        "Welsh: You can apply for an apprenticeship"
      )
      val middlePhrase: Phrase = Phrase("Middle", "Welsh: Middle")

      val detailSectionCallout: SubSectionCallout = SubSectionCallout(detailSectionPhrase, Seq(""), stack = false)
      val detailStackedNote1: NoteCallout = NoteCallout(startPhrase, Seq(""), stack = true)
      val detailStackedNote2: NoteCallout = NoteCallout(endPhrase, Seq(""), stack = true)
      val detailStackedNote3: NoteCallout = NoteCallout(bpl1Phrase1, Seq(""), stack = true)
      val detailStackedNote4: NoteCallout = NoteCallout(bpl1Phrase2, Seq(""), stack = true)
      val detailStackedNote5: NoteCallout = NoteCallout(bpl1Phrase3, Seq(""), stack = true)
      val detailStackedNote6: NoteCallout = NoteCallout(bpl2Phrase1, Seq(""), stack = true)
      val detailStackedNote7: NoteCallout = NoteCallout(bpl2Phrase2, Seq(""), stack = true)
      val detailStackedNote8: NoteCallout = NoteCallout(middlePhrase, Seq(""), stack = true)

      // Define bullet point callouts with label content
      val bpl3Phrase1: Phrase = Phrase(
        "The days of the week include Monday",
        "Welsh: The days of the [label:week] include Monday"
      )
      val bpl3Phrase2: Phrase = Phrase(
        "The days of the week include Tuesday",
        "Welsh: The days of the [label:week] include Tuesday"
      )
      val bpl3Phrase3: Phrase = Phrase(
        "The days of the week include Wednesday",
        "Welsh: The days of the [label:week] include Wednesday"
      )

      val detailStackedNote9: NoteCallout = NoteCallout(bpl3Phrase1, Seq(""), stack = true)
      val detailStackedNote10: NoteCallout = NoteCallout(bpl3Phrase2, Seq(""), stack = true)
      val detailStackedNote11: NoteCallout = NoteCallout(bpl3Phrase3, Seq(""), stack = true)

      // Define components for break defined bullet point list
      val bpl4Phrase1: Phrase = Phrase(
        "I like to go on holiday to the following:[break] Switzerland",
        "Welsh: I like to go on holiday to the following:[break] Switzerland"
      )

      val bpl4Phrase2: Phrase = Phrase(
        "I like to go on holiday to the following:[break] Jamaica",
        "Welsh: I like to go on holiday to the following:[break] Jamaica"
      )

      val detailStackedNote12: NoteCallout = NoteCallout(bpl4Phrase1, Seq(""), stack = true)
      val detailStackedNote13: NoteCallout = NoteCallout(bpl4Phrase2, Seq(""), stack = true)

      // Define components for multiple bullet point test
      val bpl5Phrase1: Phrase = Phrase(
        "I like to go on holiday to the following: Australia",
        "Welsh: I like to go on holiday to the following: Australia"
      )

      val bpl5Phrase2: Phrase = Phrase(
        "I like to go on holiday to the following: New Zealand",
        "Welsh: I like to go on holiday to the following: New Zealand"
      )

      val detailStackedNote14: NoteCallout = NoteCallout(bpl5Phrase1, Seq(""), stack = true)
      val detailStackedNote15: NoteCallout = NoteCallout(bpl5Phrase2, Seq(""), stack = true)

      val bpl6Phrase1: Phrase = Phrase(
        "I like to go on holiday to the following:[break] France",
        "Welsh: I like to go on holiday to the following:[break] France"
      )

      val bpl6Phrase2: Phrase = Phrase(
        "I like to go on holiday to the following:[break] Hawaii",
        "Welsh: I like to go on holiday to the following:[break] Hawaii"
      )

      val detailStackedNote16: NoteCallout = NoteCallout(bpl6Phrase1, Seq(""), stack = true)
      val detailStackedNote17: NoteCallout = NoteCallout(bpl6Phrase2, Seq(""), stack = true)

      val bpl7Phrase1: Phrase = Phrase(
        "I like to go on holiday to the following: Bognor Regis",
        "Welsh: I like to go on holiday to the following: Bognor Regis"
      )

      val bpl7Phrase2: Phrase = Phrase(
        "I like to go on holiday to the following: Prestatyn",
        "Welsh: I like to go on holiday to the following: Prestatyn"
      )

      val detailStackedNote18: NoteCallout = NoteCallout(bpl7Phrase1, Seq(""), stack = true)
      val detailStackedNote19: NoteCallout = NoteCallout(bpl7Phrase2, Seq(""), stack = true)
    }

    "Convert a subSection callout and single stacked note callout into a single Details component" in new DetailsTest {
      val p = uiBuilder.buildPage("/start", List(subSectionCallout, stackedNote1))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: Details) => succeed
        case x => fail(s"Found $x")
      }
    }
    "Convert a subSection callout and two stacked note callouts into a single Details component" in new DetailsTest {
      val p = uiBuilder.buildPage("/start", List(subSectionCallout, stackedNote1, stackedNote2))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: Details) => succeed
        case x => fail(s"Found $x")
      }
    }

    "Convert a subSection callout and an unstacked note callout into an H3 and InsetText" in new DetailsTest {
      val p = uiBuilder.buildPage("/start", List(subSectionCallout, unstackedNote))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: H4, _: InsetText) => succeed
        case x => fail(s"Found $x")
      }
    }

    "Convert a subSection callout, two stacked note callouts and one unstacked note into a single Details component and an InsetText" in new DetailsTest {
      val p = uiBuilder.buildPage("/start", List(subSectionCallout, stackedNote1, stackedNote2, unstackedNote))
      p.fold(_ => fail(), p => p).components match {
        case Seq(_: Details, _: InsetText) => succeed
        case x => fail(s"Found $x")
      }
    }

    "convert a subSection callout followed by three note callouts into a Details component with a bullet point list group" in new DetailsTest {

      val p = uiBuilder.buildPage(
        "/start",
        List(
          detailSectionCallout,
          detailStackedNote3,
          detailStackedNote4,
          detailStackedNote5
        )
      )

      p.fold(_ => fail(), p => p).components match {
        case Seq(details: Details) =>
          details.disclosure.size shouldBe 1
          details.disclosure.map {
            case bp: BulletPointList =>
              bp.listItems.size shouldBe 3
              bp.text.asString shouldBe "The days of the week include"
              bp.listItems(0).asString shouldBe "Monday"
              bp.listItems(1).asString shouldBe "Tuesday"
              bp.listItems(2).asString shouldBe "Wednesday"
            case _ => fail()
          }

        case err => fail(s"UIBuilder created page with components $err")
      }
    }

    "convert a SubSection callout followed by four note callouts into a Details component with text and a bullet point list group" in new DetailsTest {

      val p = uiBuilder.buildPage(
        "/start",
        List(
          detailSectionCallout,
          detailStackedNote1,
          detailStackedNote3,
          detailStackedNote4,
          detailStackedNote5
        )
      )

      p.fold(_ => fail(), p => p).components match {
        case Seq(details: Details) =>
          details.disclosure.size shouldBe 2

          details.disclosure.map {
            case bp: BulletPointList => bp.listItems.size shouldBe 3
            case p: Paragraph => p.text.asString shouldBe "Start"
            case _ => fail()
          }
        case err => fail(s"UIBuilder created page with components $err")
      }
    }

    "convert a SubSection callout followed by four note callouts into a Details component with bullet point group followed by text" in new DetailsTest {

      val p = uiBuilder.buildPage(
        "/start",
        List(
          detailSectionCallout,
          detailStackedNote3,
          detailStackedNote4,
          detailStackedNote5,
          detailStackedNote2
        )
      )

      p.fold(_ => fail(), p => p).components match {
        case Seq(details: Details) =>
          details.disclosure.size shouldBe 2

          details.disclosure.zipWithIndex.map {
            case (bp: BulletPointList, 0) => bp.listItems.size shouldBe 3
            case (p: Paragraph, 1) => p.text.asString shouldBe "End"
            case _ => fail()
          }

        case err => fail(s"UIBuilder created page with components $err")
      }
    }

    "convert a Subsection callout followed by note callouts into text and bullet point groups" in new DetailsTest {

      val p = uiBuilder.buildPage(
        "/start",
        List(
          detailSectionCallout,
          detailStackedNote1,
          detailStackedNote3,
          detailStackedNote4,
          detailStackedNote5,
          detailStackedNote8,
          detailStackedNote6,
          detailStackedNote7,
          detailStackedNote2
        )
      )

      p.fold(_ => fail(), p => p).components match {
        case Seq(details: Details) =>
          details.disclosure.size shouldBe 5

          details.disclosure.zipWithIndex.map {
            case (p: Paragraph, 0) =>
            case (bp: BulletPointList, 1) => bp.listItems.size shouldBe 3
            case (p: Paragraph, 2) =>
            case (bp: BulletPointList, 3) => bp.listItems.size shouldBe 2
            case (p: Paragraph, 4) =>
            case _ => fail()
          }

        case err => fail(s"UIBuilder created page with components $err")
      }
    }

    "convert a subSection callout followed by three note callouts with labels into a Details component with a bullet point list" in new DetailsTest {

      val p = uiBuilder.buildPage(
        "/start",
        List(
          detailSectionCallout,
          detailStackedNote9,
          detailStackedNote10,
          detailStackedNote11
        )
      )

      p.fold(_ => fail(), p => p).components match {
        case Seq(details: Details) =>
          details.disclosure.size shouldBe 1

          details.disclosure.zipWithIndex.map {
            case (bp: BulletPointList, 0) => bp.text.asString shouldBe "The days of the week include"
            case _ => fail()
          }

        case err => fail(s"UIBuilder created page with components $err")
      }

    }

    "convert a sub section callout followed by note callouts with break markers into a details component with a bullet point list" in new DetailsTest {

      val p = uiBuilder.buildPage(
        "/start",
        List(
          detailSectionCallout,
          detailStackedNote12,
          detailStackedNote13
        )
      )

      p.fold(_ => fail(), p => p).components match {
        case Seq(details: Details) =>
          details.disclosure.size shouldBe 1

          details.disclosure.zipWithIndex.map {
            case (bp: BulletPointList, 0) =>
              bp.listItems.size shouldBe 2
              bp.text.asString shouldBe "I like to go on holiday to the following:"
              bp.listItems(0).asString shouldBe "Switzerland"
              bp.listItems(1).asString shouldBe "Jamaica"
            case _ => fail()
          }

        case err => fail(s"UIBuilder created page with components $err")
      }
    }

    "convert a sub section callout followed by note callouts into a details component with multiple bullet point lists" in new DetailsTest {

      val p = uiBuilder.buildPage(
        "/start",
        List(
          detailSectionCallout,
          detailStackedNote12,
          detailStackedNote13,
          detailStackedNote14,
          detailStackedNote15,
          detailStackedNote16,
          detailStackedNote17,
          detailStackedNote18,
          detailStackedNote19
        )
      )

      p.fold(_ => fail(), p => p).components match {
        case Seq(details: Details) =>
          details.disclosure.size shouldBe 4

          details.disclosure.zipWithIndex.map {
            case (bp: BulletPointList, 0) =>
              bp.listItems.size shouldBe 2
              bp.text.asString shouldBe "I like to go on holiday to the following:"
              bp.listItems(0).asString shouldBe "Switzerland"
              bp.listItems(1).asString shouldBe "Jamaica"

            case (bp: BulletPointList, 1) =>
              bp.listItems.size shouldBe 2
              bp.text.asString shouldBe "I like to go on holiday to the following:"
              bp.listItems(0).asString shouldBe "Australia"
              bp.listItems(1).asString shouldBe "New Zealand"

            case (bp: BulletPointList, 2) =>
              bp.listItems.size shouldBe 2
              bp.text.asString shouldBe "I like to go on holiday to the following:"
              bp.listItems(0).asString shouldBe "France"
              bp.listItems(1).asString shouldBe "Hawaii"

            case (bp: BulletPointList, 3) =>
              bp.listItems.size shouldBe 2
              bp.text.asString shouldBe "I like to go on holiday to the following:"
              bp.listItems(0).asString shouldBe "Bognor Regis"
              bp.listItems(1).asString shouldBe "Prestatyn"

            case _ => fail()
          }

        case err => fail(s"UIBuilder created page with components $err")
      }

    }

  }

  "UIBuilder Important Group processing" must {
    trait ImportantTest extends Test {
      val num1Phrase = Phrase(Vector("Line1", "Welsh Line1"))
      val num2Phrase = Phrase(Vector("Line2", "Welsh Line2"))
      val num3Phrase = Phrase(Vector("Line3", "Welsh Line3"))
      val num4Phrase = Phrase(Vector("Line4", "Welsh Line4"))

      val important1Co = ImportantCallout(num1Phrase, Seq(""), false)
      val important2Co = ImportantCallout(num2Phrase, Seq(""), true)
      val important3Co = ImportantCallout(num3Phrase, Seq(""), true)
      val important4Co = ImportantCallout(num4Phrase, Seq(""), true)

      val importantGroup = ImportantGroup(Seq(important1Co,important2Co,important3Co,important4Co))

      val emptyImportantGroup = ImportantGroup(Seq.empty)
    }

    "Convert a empty important list into a ImportantGroup" in new ImportantTest {
      val p = uiBuilder.buildPage("/start", List(emptyImportantGroup))
      p.fold(_ => fail(), p => p).components match {
      case Seq(_: WarningText) => succeed
      case x => fail(s"Found $x")
      }
    }

    "Convert sequence of important callouts into a single ImportantGroup" in new ImportantTest {
      val p = uiBuilder.buildPage("/start", List(important1Co, important2Co, important3Co, important4Co))
      p.fold(_ => fail(), p => p).components match {
      case Seq(_: WarningText) => succeed
      case x => fail(s"Found $x")
      }
    }

    "Convert single unstacked important callout into separate ImportantGroup" in new ImportantTest {
      val p = uiBuilder.buildPage("/start", List(important1Co))
      p.fold(_ => fail(), p => p).components match {
      case Seq(_: WarningText) => succeed
      case x => fail(s"Found $x")
      }
    }
    "Convert sequence of unstacked Important callouts into separate importantGroup" in new ImportantTest {
      val p = uiBuilder.buildPage("/start", List(important1Co, important1Co, important1Co))
      p.fold(_ => fail(), p => p).components match {
      case Seq(_: WarningText, _: WarningText, _: WarningText) => succeed
      case x => fail(s"Found $x")
      }
    }
  }

  "UIBuilder non-exclusive sequence processing" must {

    trait NonExclusiveSequenceTest extends BaseTest {

      implicit val urlMap: Map[String, PageDesc] =
        Map(
          Process.StartStanzaId -> PageDesc(Process.StartStanzaId, "start"),
          "10" -> PageDesc("10", "page-2"),
          "35" -> PageDesc("35", "page-4"),
          "50" -> PageDesc("50", "page-10")
      )

      val next: Seq[String] = Seq("10", "35", "50")

      val textPhrase: Phrase = Phrase(Vector("Select type of bee", "Welsh: Select type of bee"))
      val textPhraseWithHint = Phrase(
        Vector(
          "Select type of bee [hint:For example Worker or Drone]",
        "Welsh: Select type of bee [hint:For example Worker or Drone]"
        )
      )

      val optionsPhrases: Seq[Phrase] = Seq(
        Phrase(Vector("Drone", "Welsh: Drone")),
        Phrase(Vector("Worker", "Welsh: Worker"))
      )

      val stanzas: Seq[KeyedStanza] = Seq(
        KeyedStanza("start", PageStanza("/start", Seq("1"), stack = false)),
        KeyedStanza(
          "1",
          ErrorCallout(
          Phrase(Vector("You must select a type of bee", "Welsh: You must select a kind of bee")),
          Seq("2"),
          stack = false
          )
        ),
        KeyedStanza(
          "2",
          SectionCallout(
            Phrase(Vector("Questions about bees", "Welsh: Questions about bees")),
              Seq("3"),
              stack = false)
          ),
        KeyedStanza(
          "3",
          Instruction(
            Phrase(Vector("General questions", "Welsh: General questions")),
            Seq("4"),
            None,
            stack = false
          )
        )
      )

      val nonExclusiveSequence = core.models.ocelot.stanzas.Sequence(textPhrase, next, optionsPhrases, None, None, stack = false)
      val nonExclusiveSequenceWithHint = core.models.ocelot.stanzas.Sequence(textPhraseWithHint, next, optionsPhrases, None, None, stack = false)

      val page: Page = Page(Process.StartStanzaId, "/start", stanzas :+ KeyedStanza("4", nonExclusiveSequence), Seq.empty)
      val pageWithHint: Page = Page(Process.StartStanzaId, "/start", stanzas :+ KeyedStanza("4", nonExclusiveSequenceWithHint), Seq.empty)
      implicit val ctx: UIContext = UIContext(labels, urlMap, messages)
      val uiBuilder: UIBuilder = new UIBuilder()
    }

    "ignore error callouts if no errors have occurred" in new NonExclusiveSequenceTest {

      val uiPage = uiBuilder.buildPage(
        page.url,
        page.stanzas.toList.collect{case s: VisualStanza => s}
      )

      uiPage match {
        case Right(f: FormPage) if(f.formComponent.errorMsgs.isEmpty) =>
          f.formComponent match {
            case s: Sequence =>
              s.text.asString shouldBe "Select type of bee"
              s.options.size shouldBe 2
              s.options.head.text.asString shouldBe "Drone"
              s.options.last.text.asString shouldBe "Worker"
              s.body.size shouldBe 2
              s.body.head match {
                case h3: H3 => h3.text.asString shouldBe "Questions about bees"
                case _ => fail("The first component in the sequence body should be an H3")
              }
              s.body.last match {
                case p: Paragraph => p.text.asString shouldBe "General questions"
                case _ => fail("The second component in the sequence body should be a paragraph")
              }
            case _ => fail("Form component should be a sequence form component")
          }
        case Right(_: FormPage) => fail("No error callouts should be present in the sequence")
        case otherPage => fail(s"Incorrect page type created by builder. Page : $otherPage")
      }
    }

    "handle definition of hint in non-exclusive sequence title" in new NonExclusiveSequenceTest {

      val uiPageWithHint = uiBuilder.buildPage(
        pageWithHint.url,
        pageWithHint.stanzas.toList.collect{case s: VisualStanza => s},
        ValueMissingError
      )

      uiPageWithHint match {
        case Right(f: FormPage) =>
          f.formComponent match {
            case s: Sequence =>
              s.text.asString shouldBe "Select type of bee"
              s.hint match {
                case Some(value) => value.asString shouldBe "For example Worker or Drone"
                case _ => fail("Hint text should be defined")
              }
            case _ => fail("Form component should be a sequence form component")
          }
        case otherPage => fail(s"Incorrect page type created by builder. Page : $otherPage")
      }

    }

    "include errors when a missing input value error has occurred" in new NonExclusiveSequenceTest {

      val uiPage = uiBuilder.buildPage(
        page.url,
        page.stanzas.toList.collect{case s: VisualStanza => s},
        ValueMissingError
      )

      uiPage match {
        case Right(f: FormPage) if(f.formComponent.errorMsgs.nonEmpty) =>
          f.formComponent match {
            case s: Sequence =>
              s.errorMsgs.size shouldBe 1
              s.errorMsgs.head match {
                case r: RequiredErrorMsg => r.text.asString shouldBe "You must select a type of bee"
                case otherError => fail(s"Unexpected error type encountered. Error is $otherError")
              }
            case _ => fail("Form component should be a sequence form component")
          }
        case Right(_: FormPage) => fail("An error message should be included in the sequence")
        case otherPage => fail(s"Incorrect page type created by builder. Page : $otherPage")
      }

    }

  }

  "UIBuilder exclusive sequence processing" must {

    trait ExclusiveSequenceTest extends BaseTest {

      implicit val urlMap: Map[String, PageDesc] =
        Map(
          Process.StartStanzaId -> PageDesc(Process.StartStanzaId, "start"),
          "10" -> PageDesc("10", "page-2"),
          "35" -> PageDesc("35", "page-4"),
          "50" -> PageDesc("50", "page-10")
      )

      val next: Seq[String] = Seq("10", "35", "40", "50")

      val textPhrase: Phrase = Phrase(Vector("Select type of bee", "Welsh: Select type of bee"))
      val textPhraseWithHint: Phrase = Phrase(
        Vector(
          "Select type of bee [hint:For example Worker or Drone]",
          "Welsh: Select type of bee [hint:For example Worker or Drone]"
        )
      )

      val optionsPhrases: Seq[Phrase] = Seq(
        Phrase(Vector("Drone", "Welsh: Drone")),
        Phrase(Vector("Worker", "Welsh: Worker"))
      )

      val optionsExclusivePhrase: Option[Phrase] = Some(Phrase(Vector(
          "Queen [hint:Selecting this checkbox will deselect the other checkboxes]",
          "Welsh: Queen [hint:Welsh: Selecting this checkbox will deselect the other checkboxes]"
        )))

      val stanzas: Seq[KeyedStanza] = Seq(
        KeyedStanza("start", PageStanza("/start", Seq("1"), stack = false)),
        KeyedStanza(
          "1",
          ErrorCallout(
            Phrase(Vector("You must select a type of bee", "Welsh: You must select a kind of bee")),
            Seq("2"),
            stack = false
          )
        ),
        KeyedStanza(
          "2",
          SectionCallout(
            Phrase(Vector("Questions about bees", "Welsh: Questions about bees")),
            Seq("3"),
            stack = false)
        ),
        KeyedStanza(
          "3",
          Instruction(
            Phrase(Vector("General questions", "Welsh: General questions")),
            Seq("4"),
            None,
            stack = false
          )
        )
      )

      val exclusiveSequence = core.models.ocelot.stanzas.Sequence(textPhrase, next, optionsPhrases, optionsExclusivePhrase, None, stack = false)
      val exclusiveSequenceWithHint = core.models.ocelot.stanzas.Sequence(textPhraseWithHint, next, optionsPhrases, optionsExclusivePhrase, None, stack = false)

      val page: Page = Page(Process.StartStanzaId, "/start", stanzas :+ KeyedStanza("4", exclusiveSequence), Seq.empty)
      val pageWithHint: Page = Page(Process.StartStanzaId, "/start", stanzas :+ KeyedStanza("4", exclusiveSequenceWithHint), Seq.empty)
      implicit val ctx: UIContext = UIContext(labels, urlMap, messages)
      val uiBuilder: UIBuilder = new UIBuilder()
    }

    "ignore error callouts if no errors have occurred" in new ExclusiveSequenceTest {

      val uiPage = uiBuilder.buildPage(
        page.url,
        page.stanzas.toList.collect{case s: VisualStanza => s}
      )

      uiPage match {
        case Right(f: FormPage) if(f.formComponent.errorMsgs.isEmpty) =>
          f.formComponent match {
            case s: Sequence =>
              s.text.asString shouldBe "Select type of bee"
              s.options.size shouldBe 2
              s.options.head.text.asString shouldBe "Drone"
              s.options.last.text.asString shouldBe "Worker"
              s.exclusiveOption.get.text.asString shouldBe "Queen"
              s.body.size shouldBe 2
              s.body.head match {
                case h3: H3 => h3.text.asString shouldBe "Questions about bees"
                case _ => fail("The first component in the sequence body should be an H3")
              }
              s.body.last match {
                case p: Paragraph => p.text.asString shouldBe "General questions"
                case _ => fail("The second component in the sequence body should be a paragraph")
              }
            case _ => fail("Form component should be a sequence form component")
          }
        case Right(_: FormPage) => fail("No error callouts should be present in the sequence")
        case otherPage => fail(s"Incorrect page type created by builder. Page : $otherPage")
      }
    }

    "handle definition of hint in non-exclusive sequence title" in new ExclusiveSequenceTest {

      val uiPageWithHint = uiBuilder.buildPage(
        pageWithHint.url,
        pageWithHint.stanzas.toList.collect{case s: VisualStanza => s},
        ValueMissingError
      )

      uiPageWithHint match {
        case Right(f: FormPage) =>
          f.formComponent match {
            case s: Sequence =>
              s.text.asString shouldBe "Select type of bee"
              s.hint match {
                case Some(value) => value.asString shouldBe "For example Worker or Drone"
                case _ => fail("Hint text should be defined")
              }
            case _ => fail("Form component should be a sequence form component")
          }
        case otherPage => fail(s"Incorrect page type created by builder. Page : $otherPage")
      }

    }

    "include errors when a missing input value error has occurred" in new ExclusiveSequenceTest {

      val uiPage = uiBuilder.buildPage(
        page.url,
        page.stanzas.toList.collect{case s: VisualStanza => s},
        ValueMissingError
      )

      uiPage match {
        case Right(f: FormPage) if(f.formComponent.errorMsgs.nonEmpty) =>
          f.formComponent match {
            case s: Sequence =>
              s.errorMsgs.size shouldBe 1
              s.errorMsgs.head match {
                case r: RequiredErrorMsg => r.text.asString shouldBe "You must select a type of bee"
                case otherError => fail(s"Unexpected error type encountered. Error is $otherError")
              }
            case _ => fail("Form component should be a sequence form component")
          }
        case Right(_: FormPage) => fail("An error message should be included in the sequence")
        case otherPage => fail(s"Incorrect page type created by builder. Page : $otherPage")
      }

    }

  }
}

