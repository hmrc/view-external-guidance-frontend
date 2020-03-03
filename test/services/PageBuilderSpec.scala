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
import models.ocelot.{Page, _}
import play.api.libs.json._
import utils.StanzaHelper

class PageBuilderSpec extends BaseSpec with ProcessJson with StanzaHelper {

  val meta: Meta = Json.parse(prototypeMetaSection).as[Meta]

  case object DummyStanza extends Stanza {
    override val next: Seq[String] = Seq("1")
  }

  trait Test {
    val pageId1 = "start"
    val pageId2 = "4"
    val pageId3 = "6"
    val pageId4 = "9"
    val pageId5 = "11"
    val pageId6 = "14"
    val pageIds = Seq(pageId1,pageId2,pageId3,pageId4,pageId5,pageId6)
    private val flow = Map(
      pageId1 -> ValueStanza(List(Value(Scalar, "PageUrl", "/start")), Seq("1"), false),
      "1" -> InstructionStanza(3, Seq("2"), None, false),
      "2" -> QuestionStanza(1, Seq(2, 1), Seq(pageId2, pageId4), false),

      pageId2 -> ValueStanza(List(Value(Scalar, "PageUrl", "/this4")), Seq("5"), false),
      "5" -> InstructionStanza(1, Seq("end"), Some(0), false),

      pageId3 -> ValueStanza(List(Value(Scalar, "PageUrl", "/this6")), Seq("7"), false),
      "7" -> InstructionStanza(2, Seq("8"), None, false),
      "8" -> QuestionStanza(1, Seq(2, 3), Seq(pageId4, pageId6), false),

      pageId4 -> ValueStanza(List(Value(Scalar, "PageUrl", "/this9")), Seq("16"), false),
      "16" -> InstructionStanza(3, Seq("10"), None, false),
      "10" -> InstructionStanza(2, Seq("end"), None, false),

      pageId5 -> ValueStanza(List(Value(Scalar, "PageUrl", "/this11")), Seq("12"), false),
      "12" -> InstructionStanza(0, Seq("13"), None, false),
      "13" -> QuestionStanza(1, Seq(2, 3), Seq(pageId6, pageId2), false),

      pageId6 -> ValueStanza(List(Value(Scalar, "PageUrl", "/this14")), Seq("15"), false),
      "15" -> InstructionStanza(0, Seq("end"), None, false),
      "end" -> EndStanza
    )

    private val phrases = Vector[Phrase](
                                 Phrase(Vector("Some Text","Welsh, Some Text")),
                                 Phrase(Vector("Some Text1","Welsh, Some Text1")),
                                 Phrase(Vector("Some Text2","Welsh, Some Text2")),
                                 Phrase(Vector(s"Some [link:Link to stanza 11:${pageId5}] Text3",s"Welsh, Some [link:Link to stanza 11:${pageId5}] Text3")))

    private val links = Vector(Link(0,pageId3, "", false),Link(1,pageId6, "", false))

    val processWithLinks = Process(metaSection, flow, phrases, links)

  }

  "PageBuilder error handling" must {

    val flow = Map(
      "start" -> ValueStanza(List(Value(Scalar, "PageUrl", "/")), Seq("1"), false),
      "1" -> InstructionStanza(0, Seq("2"), None, false),
      "2" -> DummyStanza
    )

    "detect UnknownStanzaType error" in {

      val process = Process(metaSection, flow, Vector[Phrase](Phrase(Vector("Some Text","Welsh, Some Text"))), Vector[Link]())

      PageBuilder.buildPage("start", process) match {
        case Left(UnknownStanzaType(DummyStanza)) => succeed
        case Left(stanza) => fail(s"Unknown stanza not detected, found $stanza")
        case err => fail(s"Unknown stanza not detected $err")
      }

    }

    "detect StanzaNotFound error" in {
      val process = Process(metaSection, flow, Vector[Phrase](), Vector[Link]())

      PageBuilder.buildPage("4", process) match {
        case Left(StanzaNotFound("4")) => succeed
        case _ => fail("Unknown stanza not detected")
      }
    }

    "detect MissingPageUrlValueStanza error when stanza routes to page not starting with PageUrl ValueStanza" in {
      val flow = Map(
        "start" -> ValueStanza(List(Value(Scalar, "PageUrl", "/")), Seq("1"), false),
        "1" -> InstructionStanza(0, Seq("2"), None, false),
        "2" -> QuestionStanza(1, Seq(2, 3), Seq("4", "5"), false),
        "4" -> InstructionStanza(0, Seq("end"), None, false),
        "5" -> InstructionStanza(0, Seq("end"), None, false),
        "end" -> EndStanza
      )
      val process = Process(metaSection, flow, Vector[Phrase](Phrase(Vector("Some Text","Welsh, Some Text")),
                                                              Phrase(Vector("Some Text1","Welsh, Some Text1")),
                                                              Phrase(Vector("Some Text2","Welsh, Some Text2")),
                                                              Phrase(Vector("Some Text3","Welsh, Some Text3"))), Vector[Link]())

      PageBuilder.pages(process) match {
        case Left(MissingPageUrlValueStanza("4")) => succeed
        case Left(err) => fail(s"Missing ValueStanza containing PageUrl value not detected, failed with $err")
        case _ => fail(s"Missing ValueStanza containing PageUrl value not detected")
      }
    }

    "detect MissingPageUrlValueStanza error when PageValue is present but url is blank" in {
      val flow = Map(
        "start" -> ValueStanza(List(Value(Scalar, "PageUrl", "")), Seq("1"), false),
        "1" -> InstructionStanza(0, Seq("2"), None, false),
        "2" -> QuestionStanza(1, Seq(2, 3), Seq("4", "5"), false),
        "4" -> InstructionStanza(0, Seq("end"), None, false),
        "5" -> InstructionStanza(0, Seq("end"), None, false),
        "end" -> EndStanza
      )
      val process = Process(metaSection, flow, Vector[Phrase](Phrase(Vector("Some Text","Welsh, Some Text")),
                                                              Phrase(Vector("Some Text1","Welsh, Some Text1")),
                                                              Phrase(Vector("Some Text2","Welsh, Some Text2")),
                                                              Phrase(Vector("Some Text3","Welsh, Some Text3"))), Vector[Link]())

      PageBuilder.pages(process) match {
        case Left(MissingPageUrlValueStanza("start")) => succeed
        case Left(err) => fail(s"Missing ValueStanza containing PageUrl value not detected, failed with $err")
        case _ => fail(s"Missing ValueStanza containing PageUrl value not detected")
      }
    }

    "detect MissingPageUrlWithinValueStanza error when no PageUrl named value" in {
      val flow = Map(
        "start" -> ValueStanza(List(Value(Scalar, "SomeValue", "Blah")), Seq("1"), false),
        "1" -> InstructionStanza(0, Seq("2"), None, false),
        "2" -> QuestionStanza(1, Seq(2, 3), Seq("4", "5"), false),
        "4" -> InstructionStanza(0, Seq("end"), None, false),
        "5" -> InstructionStanza(0, Seq("end"), None, false),
        "end" -> EndStanza
      )
      val process = Process(metaSection, flow, Vector[Phrase](Phrase(Vector("Some Text","Welsh, Some Text")),
                                                              Phrase(Vector("Some Text1","Welsh, Some Text1")),
                                                              Phrase(Vector("Some Text2","Welsh, Some Text2")),
                                                              Phrase(Vector("Some Text3","Welsh, Some Text3"))), Vector[Link]())

      PageBuilder.pages(process) match {
        case Left(MissingPageUrlValueStanza("start")) => succeed
        case Left(err) => fail(s"Missing PageUrl value within initial ValueStanza not found with error $err")
        case Right(_) => fail(s"Missing PageUrl value within initial ValueStanza missed")
      }
    }

    "detect PhraseNotFound in QuestionStanza text" in {
      val flow = Map(
        "start" -> ValueStanza(List(Value(Scalar, PageUrlValueName.toString, "Blah")), Seq("1"), false),
        "1" -> InstructionStanza(0, Seq("2"), None, false),
        "2" -> QuestionStanza(four, Seq(two, three), Seq("4", "5"), false),
        "4" -> InstructionStanza(0, Seq("end"), None, false),
        "5" -> InstructionStanza(0, Seq("end"), None, false),
        "end" -> EndStanza
      )
      val process = Process(metaSection, flow, Vector[Phrase](Phrase(Vector("Some Text","Welsh, Some Text")),
                                                              Phrase(Vector("Some Text1","Welsh, Some Text1")),
                                                              Phrase(Vector("Some Text2","Welsh, Some Text2")),
                                                              Phrase(Vector("Some Text3","Welsh, Some Text3"))), Vector[Link]())

      PageBuilder.pages(process) match {
        case Left(PhraseNotFound(four)) => succeed
        case Left(err) => fail(s"Missing PhraseNotFound(4) with error $err")
        case Right(_) => fail(s"Missing PhraseNotFound(4)")
      }
    }

    "detect PhraseNotFound in QuestionStanza answers" in {
      val flow = Map(
        "start" -> ValueStanza(List(Value(Scalar, PageUrlValueName.toString, "Blah")), Seq("1"), false),
        "1" -> InstructionStanza(0, Seq("2"), None, false),
        "2" -> QuestionStanza(1, Seq(four, three), Seq("4", "5"), false),
        "4" -> InstructionStanza(0, Seq("end"), None, false),
        "5" -> InstructionStanza(0, Seq("end"), None, false),
        "end" -> EndStanza
      )
      val process = Process(metaSection, flow, Vector[Phrase](Phrase(Vector("Some Text","Welsh, Some Text")),
                                                              Phrase(Vector("Some Text1","Welsh, Some Text1")),
                                                              Phrase(Vector("Some Text2","Welsh, Some Text2")),
                                                              Phrase(Vector("Some Text3","Welsh, Some Text3"))), Vector[Link]())

      PageBuilder.pages(process) match {
        case Left(PhraseNotFound(four)) => succeed
        case Left(err) => fail(s"Missing PhraseNotFound(4) with error $err")
        case Right(_) => fail(s"Missing PhraseNotFound(4)")
      }
    }

    "detect PhraseNotFound in InstructionStanza" in {
      val flow = Map(
        "start" -> ValueStanza(List(Value(Scalar, PageUrlValueName.toString, "Blah")), Seq("1"), false),
        "1" -> InstructionStanza(2, Seq("end"), None, false),
        "end" -> EndStanza
      )
      val process = Process(metaSection, flow, Vector[Phrase](Phrase(Vector("Some Text","Welsh, Some Text"))), Vector[Link]())

      PageBuilder.pages(process) match {
        case Left(PhraseNotFound(2)) => succeed
        case Left(err) => fail(s"Missing PhraseNotFound(2) with error $err")
        case Right(_) => fail(s"Missing PhraseNotFound(2)")
      }
    }

    "detect PhraseNotFound in CalloutStanza" in {
      val flow = Map(
        "start" -> ValueStanza(List(Value(Scalar, PageUrlValueName.toString, "Blah")), Seq("1"), false),
        "1" -> CalloutStanza(Title, 2, Seq("end"), false),
        "end" -> EndStanza
      )
      val process = Process(metaSection, flow, Vector[Phrase](Phrase(Vector("Some Text","Welsh, Some Text"))), Vector[Link]())

      PageBuilder.pages(process) match {
        case Left(PhraseNotFound(2)) => succeed
        case Left(err) => fail(s"Missing PhraseNotFound(2) with error $err")
        case Right(_) => fail(s"Missing PhraseNotFound(2)")
      }
    }

    "detect LinkNotFound(1) in InstructionStanza" in {

      val flow: Map[String, Stanza] = Map(
        "start" -> ValueStanza(List(Value(Scalar, PageUrlValueName.toString, "Blah Blah")), Seq("1"), false),
        "1" -> InstructionStanza(0, Seq("2"), None, false),
        "2" -> InstructionStanza(1, Seq("end"), Some(1), false),
        "end" -> EndStanza
      )

      // Create process with single link
      val process: Process = Process(metaSection, flow,
        Vector[Phrase](Phrase(Vector("First English phrase", "First Welsh Phrase")), Phrase(Vector("Second English Phrase", "Second Welsh Phrase"))),
        Vector[Link](Link(0, "http://my.com/search", "MyCOM Search Engine", false)))

      PageBuilder.pages(process) match {
        case Left(LinkNotFound(1)) => succeed
        case Left(err) => fail(s"Missing LinkNotFound error. Actual error raised is $err")
        case Right(_) => fail("Page building terminated successfully when LinkNotFound error expected")
      }
    }

    "detect DuplicatePageUrl" in {
      val flow = Map(
        "start" -> ValueStanza(List(Value(Scalar, "PageUrl", "/this")), Seq("1"), false),
        "1" -> InstructionStanza(0, Seq("2"), None, false),
        "2" -> QuestionStanza(1, Seq(2, 3), Seq("4", "5"), false),
        "4" -> ValueStanza(List(Value(Scalar, "PageUrl", "/this")), Seq("5"), false),
        "5" -> InstructionStanza(0, Seq("end"), None, false),
        "end" -> EndStanza
      )
      val process = Process(metaSection, flow, Vector[Phrase](Phrase(Vector("Some Text","Welsh, Some Text")),
                                                              Phrase(Vector("Some Text1","Welsh, Some Text1")),
                                                              Phrase(Vector("Some Text2","Welsh, Some Text2")),
                                                              Phrase(Vector("Some Text3","Welsh, Some Text3"))), Vector[Link]())

      PageBuilder.pages(process) match {
        case Left(DuplicatePageUrl("4", "/this")) => succeed
        case Left(err) => fail(s"DuplicatePageUrl error not detected, failed with $err")
        case _ => fail(s"DuplicatePageUrl not detected")
      }
    }

  }

  "PageBuilder" must {

    "be not buildable from non-existent key" in {


      val process: Process = prototypeJson.as[Process]

      PageBuilder.buildPage("unknown", process) match {
        case Right(_) => fail("Invalid key should not return a page")
        case Left(err) => succeed
      }
    }

    "Sequence of connected pages" must {

      "not be extractable from a Process using an invalid start key" in {

        val process: Process = prototypeJson.as[Process]

        PageBuilder.pages(process, "unknown") match {
          case Right(_) => fail("""Should fail with StanzaNotFound("unknown")""")
          case Left(err) if err == StanzaNotFound("unknown") => succeed
          case Left(wrongErr) => fail("""Should fail with StanzaNotFound("unknown")""")
        }
      }

      "be extractable from a Process using key 'start''" in {

        val process: Process = prototypeJson.as[Process]

        PageBuilder.pages(process) match {
          case Right(pages) =>
            pages mustNot be(Nil)

            pages.length mustBe 28

          case Left(err) => fail(s"FlowError $err")
        }

      }

      "correctly identify the pages in a Process accounting fro every stanza" in {

        val process: Process = prototypeJson.as[Process]

        PageBuilder.pages( process ) match {
          case Right(pages) => {

            testPagesInPrototypeJson( pages )

          }
          case Left(err) => fail( s"Flow error $err" )
        }


      }

      "consist of one page when only page exists" in {
        val process: Process = validOnePageJson.as[Process]

        PageBuilder.pages(process, "1") match {
          case Right(pages) =>
            pages mustNot be(Nil)

            pages.length mustBe 1

          case Left(err) => fail(s"FlowError $err")
        }

      }

      "confirm one page elements" in {

        val process: Process = Process(meta, onePage, Vector[Phrase](Phrase(Vector("Some Text","Welsh, Some Text")),
                                                                     Phrase(Vector("Some Text1","Welsh, Some Text1"))), Vector[Link]())

        PageBuilder.pages(process) match {
          case Right(pages) =>
            pages mustNot be(Nil)

            pages.length mustBe 1

          case Left(err) => fail(s"FlowError $err")
        }
      }

      "follows links to pages idenitified by stanza id " in new Test {

        PageBuilder.pages(processWithLinks) match {
          case Right(pages) =>
            pages.length mustBe 6
            val pageMap = pages.map(p => (p.id, p.linked)).toMap

            pageIds.forall(pageMap.contains) mustBe true

            pageMap(pageId1) mustBe List(pageId5)
            pageMap(pageId2) mustBe List(pageId3)
            pageMap(pageId3) mustBe Nil
            pageMap(pageId4) mustBe List(pageId5)
            pageMap(pageId5) mustBe Nil
            pageMap(pageId6) mustBe Nil

          case Left(err) => fail(s"FlowError $err")
        }
      }

    }

    "When processing a simple question page" must {

      val process: Process = Process(meta, simpleQuestionPage, phrases, links )

        PageBuilder.pages(process) match {

          case Right(pages) =>

            "Determine the correct number of pages to be displayed" in {

              pages mustNot be(Nil)

              pages.length mustBe 3
            }

            val indexedSeqOfPages = pages.toIndexedSeq

            // Test contents of individual pages
            testSqpQp(indexedSeqOfPages(0))

            testSqpFap(indexedSeqOfPages(1))

            testSqpSap(indexedSeqOfPages(2))

          case Left(err) => fail(s"Flow error $err")
        }
    }

    "Return ungrouped Instruction stanzas when text contents do not start with similar text" in {

      val instructionStanza1: InstructionStanza = InstructionStanza(0, Seq("2"), None, false)
      val instructionStanza2: InstructionStanza = InstructionStanza(1, Seq("end"), None, false)

      val phrase1: Phrase = Phrase( Vector("Some Text","Welsh, Some Text") )
      val phrase2: Phrase = Phrase( Vector("Some Text1","Welsh, Some Text1") )

        val flow = Map(
          "start" -> ValueStanza(List(Value(Scalar, "PageUrl", "/this")), Seq("1"), false),
          "1" -> instructionStanza1,
          "2" -> instructionStanza2,
          "end" -> EndStanza
        )
        val process = Process(metaSection, flow, Vector[Phrase]( phrase1, phrase2 ), Vector[Link]())

         PageBuilder.pages( process ) match {
           case Right(pages) => {

             assert( pages.head.stanzas.size == 4 )

             pages.head.stanzas(1) mustBe Instruction( instructionStanza1, phrase1, None )
             pages.head.stanzas(2) mustBe Instruction( instructionStanza2, phrase2, None )
           }
           case Left(err) => fail( s"Flow error $err" )
         }
    }

    "Return grouped Instruction stanzas when text contents start with similar text" in {

      val instructionStanza1: InstructionStanza = InstructionStanza(0, Seq("2"), None, true)
      val instructionStanza2: InstructionStanza = InstructionStanza(1, Seq("3"), None, false)
      val instructionStanza3: InstructionStanza = InstructionStanza(2, Seq("end"), None, false )

      val phrase1: Phrase = Phrase(Vector("Today I bought some beetroot",""))
      val phrase2: Phrase = Phrase(Vector("Today I bought some carrots",""))
      val phrase3: Phrase = Phrase(Vector("Today I bought some peppers", ""))

      val flow = Map(
        "start" -> ValueStanza(List(Value(Scalar, "PageUrl", "/this")), Seq("1"), false),
        "1" -> instructionStanza1,
        "2" -> instructionStanza2,
        "3" -> instructionStanza3,
        "end" -> EndStanza
      )

      val process = Process(metaSection, flow, Vector[Phrase]( phrase1, phrase2, phrase3 ), Vector[Link]())

      PageBuilder.pages( process ) match {
        case Right(pages) => {

          assert( pages.head.stanzas.size == 3 )

          // Construct expected instruction group stanza
          val instruction1: Instruction = Instruction( instructionStanza1, phrase1, None )
          val instruction2: Instruction = Instruction( instructionStanza2, phrase2, None )
          val instruction3: Instruction = Instruction( instructionStanza3, phrase3, None )

          val expectedInstructionGroup: InstructionGroup = InstructionGroup( Seq( instruction1,
            instruction2,
            instruction3 ) )

          pages.head.stanzas(1) mustBe expectedInstructionGroup
        }
        case Left(err) => fail( s"Flow error $err" )
      }
    }

    "Correctly group instructions stanzas in a complex page" in {

      val calloutStanza1:CalloutStanza =  CalloutStanza( Title, 0, Seq("2"), false )
      val calloutStanza2:CalloutStanza = CalloutStanza( SubTitle, four, Seq( "6" ), false )

      val instructionStanza1: InstructionStanza = InstructionStanza( 1, Seq( "3" ), None, true )
      val instructionStanza2: InstructionStanza = InstructionStanza( 2, Seq( "4" ), None, true )
      val instructionStanza3: InstructionStanza = InstructionStanza( 3, Seq( "5" ), None, false )
      val instructionStanza4: InstructionStanza = InstructionStanza( five, Seq( "7" ), None, false )
      val instructionStanza5: InstructionStanza = InstructionStanza( six, Seq( "9" ), None, false )
      val instructionStanza6: InstructionStanza = InstructionStanza( seven, Seq( "10" ), None, true )
      val instructionStanza7: InstructionStanza = InstructionStanza( eight, Seq( "end" ), None, false )

      // Define phrases
      val phrase1: Phrase = Phrase( Vector( "Main title", "" ) )
      val phrase2: Phrase = Phrase( Vector( "My favourite sweets are Wine gums", "" ) )
      val phrase3: Phrase = Phrase( Vector( "My favourite sweets are humbugs", "" ) )
      val phrase4: Phrase = Phrase( Vector( "Today is Monday", "" ) )
      val phrase5: Phrase = Phrase( Vector( "More news", "" ) )
      val phrase6: Phrase = Phrase( Vector( "Today in the West Midlands", "") )
      val phrase7: Phrase = Phrase( Vector( "Late night in Brierly hill" ) )
      val phrase8: Phrase = Phrase( Vector( "What is happening in Dudley", "") )
      val phrase9: Phrase = Phrase( Vector( "What is happening in Halesowen", "" ) )

      val flow = Map(
        "start" -> ValueStanza(List(Value(Scalar, "PageUrl", "/this")), Seq("1"), false),
        "1" -> calloutStanza1,
        "2" -> instructionStanza1,
        "3" -> instructionStanza2,
        "4" -> instructionStanza3,
        "5" -> calloutStanza2,
        "6" -> instructionStanza4,
        "7" -> ValueStanza(List(Value(Scalar, "Region", "West Midlands" ) ), Seq( "8" ), false ),
        "8" -> instructionStanza5,
        "9" -> instructionStanza6,
        "10" -> instructionStanza7,
        "end" -> EndStanza
      )

      val process:Process =  Process( metaSection,
        flow,
      Vector[Phrase](
        phrase1,
        phrase2,
        phrase3,
        phrase4,
        phrase5,
        phrase6,
        phrase7,
        phrase8,
        phrase9),
      Vector[Link]()
      )

      PageBuilder.pages( process ) match {
        case Right(pages) => {

          assert( pages.head.stanzas.size == 10 )

          // Test expected instruction group stanzas
          val instruction1: Instruction = Instruction( instructionStanza1, phrase2, None )
          val instruction2: Instruction = Instruction( instructionStanza2, phrase3, None )

          val expectedInstructionGroup1: InstructionGroup = InstructionGroup( Seq( instruction1, instruction2 ) )

          pages.head.stanzas(2) mustBe expectedInstructionGroup1

          val instruction6: Instruction = Instruction( instructionStanza6, phrase8, None )
          val instruction7: Instruction = Instruction( instructionStanza7, phrase9, None )

          val expectedInstructionGroup2: InstructionGroup = InstructionGroup( Seq( instruction6, instruction7 ) )

          pages.head.stanzas( eight ) mustBe expectedInstructionGroup2
        }
        case Left(err) => fail( s"Flow error $err" )
      }

    }

  }


  "When processing a 2 page flow separated by a ValueStanza" must {
    val process: Process = Process(meta, twoPagesSeperatedByValueStanza, Vector(Phrase(Vector("Some Text","Welsh, Some Text")),
                                                                                Phrase(Vector("Some Text1","Welsh, Some Text1"))), Vector[Link]())
    "result in 2 pages" in {
      PageBuilder.pages(process) match {
        case Right(pages) if pages.length == 2 => succeed
        case Right(pages) => fail(s"Page count is incorrect, found ${pages.length} pages")
        case Left(err) => fail(s"FAIL ${err.toString}")
      }
    }
  }

  def testPagesInPrototypeJson( pages: Seq[Page] ) : Unit = {

    val expectedPageIds: List[String] = List( "start", "26", "36", "37", "39", "46", "53", "60",
                                              "70", "77", "120", "80", "83", "90", "97", "102",
                                              "109", "113", "121", "124", "127", "131", "159",
                                              "138", "143", "151", "157", "158" )

    val indexedPages: IndexedSeq[Page] = pages.toIndexedSeq

    expectedPageIds.zipWithIndex.foreach{
      case( id, index ) =>
        indexedPages( index ).id mustBe id
    }
  }

  /**
   * Test question page in simple question page test
   *
   * @param firstPage
   */
  def testSqpQp(firstPage: Page): Unit = {

    "Define the question page correctly" in {

      firstPage.id mustBe "start"
      firstPage.stanzas.size mustBe 4

      firstPage.stanzas mustBe Seq(sqpQpValueStanza,
                                   sqpQpInstruction,
                                   sqpQpCallout,
                                   sqpQpQuestion)

      firstPage.next mustBe Seq("4", "6")
    }

  }

  /**
   * Test first answer page in simple question page test
   *
   * @param secondPage
   */
  def testSqpFap(secondPage: Page): Unit = {

    "Define the first answer page correctly" in {

      secondPage.id mustBe "4"
      secondPage.stanzas.size mustBe 3

      secondPage.stanzas(0) mustBe sqpFapValueStanza
      secondPage.stanzas(1) mustBe sqpFapInstruction
      secondPage.stanzas.last mustBe EndStanza

      secondPage.next mustBe Nil
    }

  }

  /**
   * Test second answer page in simple question page
   *
   * @param thirdPage
   */
  def testSqpSap(thirdPage: Page): Unit = {

    "Define the second answer page correctly" in {

      thirdPage.id mustBe "6"
      thirdPage.stanzas.size mustBe 4

      thirdPage.stanzas(0) mustBe sqpSapValueStanza
      thirdPage.stanzas(1) mustBe sqpSapInstruction
      thirdPage.stanzas(2) mustBe sqpSapCallout
      thirdPage.stanzas.last mustBe EndStanza

      thirdPage.next mustBe Nil
    }

  }
}
