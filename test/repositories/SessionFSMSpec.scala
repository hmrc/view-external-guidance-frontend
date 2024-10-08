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

package repositories

import base.BaseSpec
import core.models.ocelot.stanzas.{ScalarType, Value, ValueStanza}
import models.{SessionKey, PageHistory, Session, RawPageHistory}
import core.models.ocelot.{SequenceJson, Published, LabelValue, Continuation, Phrase, ListLabel, FlowStage, Label}
import core.models.ocelot.{ScalarLabel, Flow, Process, ProcessJson, LabelOperation, Update, Delete}

import java.time.Instant

class SessionFSMSpec extends BaseSpec {
  type BackLinkAndStateUpdate = (Option[String], Option[List[PageHistory]], Option[List[FlowStage]], List[Label], List[LabelOperation])
  val fsm = new SessionFSM
  def verify(fsmOutput: BackLinkAndStateUpdate, bl: Option[String], ph: Option[List[PageHistory]], fs: Option[List[FlowStage]], l: List[Label]): Unit = {
    fsmOutput._1 shouldBe bl
    fsmOutput._2 shouldBe ph
    fsmOutput._3 shouldBe fs
    fsmOutput._4 shouldBe l
  }

  trait Test extends ProcessJson {
    val oneEn: String = "One"
    val oneCy: String = s"Welsh: $oneEn"
    val twoEn: String = "Two"
    val twoCy: String = s"Welsh: $twoEn"
    val threeEn: String = "Three"
    val threeCy: String = s"Welsh: $threeEn"
    val fourEn: String = "Four"
    val fourCy: String = s"Welsh: $fourEn"
    val phraseOne: Phrase = Phrase(oneEn, oneCy)
    val phraseTwo: Phrase = Phrase(twoEn, twoCy)
    val phraseThree: Phrase = Phrase(threeEn, threeCy)
    val phraseFour: Phrase = Phrase(fourEn, fourCy)
    val oneTwo: List[Phrase] = List(phraseOne, phraseTwo)
    val oneTwoThree: List[Phrase] = oneTwo :+ phraseThree
    val process: Process = validOnePageJson.as[Process]

  }

  trait NoFlowStackTest extends Test {
    val session: Session =
      new Session(
        SessionKey("id", process.meta.processCode),
        Some(Published),
        "processId",
        Map(),
        Nil,
        Map(),
        Map(),
        List(RawPageHistory("start", Nil, Nil)),
        Nil,
        None,
        Instant.now,
        process.meta.lastUpdate,
        process.meta.timescalesVersion,
        process.meta.ratesVersion
      )
  }

  "SessionFSM with no flowStack" must {
    "Return no backlink or updates for any url with no page history, forceForward false (Nil)" in new NoFlowStackTest {
      verify(fsm("/start", Nil, Nil, false, "/start"),
             None,
             Some(List(PageHistory("/start", Nil, Nil))),
             None,
             Nil)
    }

    "Return no backlink or updates for any url with no page history, forceForward true (Nil)" in new NoFlowStackTest {
      verify(fsm("/start", Nil, Nil, false, "/start"),
             None,
             Some(List(PageHistory("/start", Nil, Nil))),
             None,
             Nil)
    }

    "Return backlink and no updates for any url with single history, forceForward false (FORWARD)" in new NoFlowStackTest {
      verify(fsm("/next", List(PageHistory("/start", Nil, Nil)), Nil, false, "/start"),
             Some("/start"),
             Some(List(PageHistory("/start", Nil, Nil), PageHistory("/next", Nil, Nil))),
             None,
             Nil)
    }

    "Return backlink and no updates for any url with single history, forceForward true (FORWARD)" in new NoFlowStackTest {
      verify(fsm("/next", List(PageHistory("/start", Nil, Nil)), Nil, true, "/start"),
             Some("/start"),
             Some(List(PageHistory("/start", Nil, Nil), PageHistory("/next", Nil, Nil))),
             None,
             Nil)
    }

    "Return no backlink, + PageHistory update for repitition of the last url, with single history, forceForward false (REFRESH)" in new NoFlowStackTest {
      verify(fsm("/start", List(PageHistory("/start", Nil, Nil)), Nil, false, "/start"),
             None,
             None,
             None,
             Nil)
    }

    "Return no backlink, PageHistory update for repitition of the last url, with single history, forceForward  true (REFRESH)" in new NoFlowStackTest {
      verify(fsm("/start", List(PageHistory("/start", Nil, Nil)), Nil, true, "/start"),
             None,
             None,
             None,
             Nil)
    }

    "Return backlink, + PageHistory update for repitition of the last url, with multiple history, forceForward false (REFRESH)" in new NoFlowStackTest {
      verify(fsm("/next", List(PageHistory("/start", Nil, Nil), PageHistory("/next", Nil, Nil)), Nil, false, "/start"),
             Some("/start"),
             None,
             None,
             Nil)
    }

    "Return backlink, PageHistory update for repitition of the last url, with multiple history, forceForward  true (REFRESH)" in new NoFlowStackTest {
      verify(fsm("/next", List(PageHistory("/start", Nil, Nil), PageHistory("/next", Nil, Nil)), Nil, true, "/start"),
             Some("/start"),
             None,
             None,
             Nil)
    }

    "Return no backlink, PageHistory update with two element history, forceForward false (BACK)" in new NoFlowStackTest {
        verify(fsm("/start", List(PageHistory("/start", Nil, Nil), PageHistory("/next", Nil, Nil)), Nil, false, "/start"),
               None,
               Some(List(PageHistory("/start", Nil, Nil))),
               None,
               Nil)
    }

    "Return backlink, PageHistory update with multiple element history, forceForward false (BACK)" in new NoFlowStackTest {
        verify(fsm("/next", List(PageHistory("/start", Nil, Nil), PageHistory("/next", Nil, Nil), PageHistory("/another", Nil, Nil)), Nil, false, "/start"),
               Some("/start"),
               Some(List(PageHistory("/start", Nil, Nil), PageHistory("/next", Nil, Nil))),
               None,
               Nil)
    }

    "Return backlink, No updates with multiple element history, forceForward true (FORWARD to HISTORIC)" in new NoFlowStackTest {
        verify(fsm("/next", List(PageHistory("/start", Nil, Nil), PageHistory("/next", Nil, Nil), PageHistory("/another", Nil, Nil)), Nil, true, "/start"),
               Some("/another"),
               Some(List(PageHistory("/start", Nil, Nil), PageHistory("/next", Nil, Nil), PageHistory("/another", Nil, Nil), PageHistory("/next", Nil, Nil))),
               None,
               Nil)
    }

    "Return no backlink, PageHistory update with multiple element history, when returning to first page forceForward false (FORWARD)" in new NoFlowStackTest {
        verify(fsm("/start", List(PageHistory("/start", Nil, Nil), PageHistory("/next", Nil, Nil), PageHistory("/another", Nil, Nil)), Nil, true, "/start"),
               None,
               Some(List(PageHistory("/start", Nil, Nil))),
               Some(Nil),
               Nil)
    }

    "Return backlink, PageHistory update with multiple element history, when returning to a page not in a sequence flow, forceForward false (FORWARD to HISTORIC)" in new NoFlowStackTest {
        verify(fsm("/next", List(PageHistory("/start", Nil, Nil), PageHistory("/next", Nil, Nil), PageHistory("/somepage", Nil, Nil), PageHistory("/another", Nil, Nil)), Nil, true, "/start"),
               Some("/another"),
               Some(List(PageHistory("/start", Nil, Nil), PageHistory("/next", Nil, Nil), PageHistory("/somepage", Nil, Nil), PageHistory("/another", Nil, Nil), PageHistory("/next", Nil, Nil))),
               None,
               Nil)
    }

    "Return backlink, PageHistory update with multiple element history, when returning to a page within a sequence flow, forceForward false (FORWARD to HISTORIC)" in new NoFlowStackTest {
      val updatedPageHistory: List[PageHistory] = List(PageHistory("/start", Nil, Nil),
                                                       PageHistory("/next", Nil, List(Flow("8",None),Continuation("2"))),
                                                       PageHistory("/somepage", Nil, Nil),
                                                       PageHistory("/another", Nil, Nil))
        verify(fsm("/next", updatedPageHistory, Nil, true, "/start"),
               Some("/another"),
               Some(List(PageHistory("/start", Nil, Nil),
                         PageHistory("/next", Nil, List(Flow("8",None),Continuation("2"))),
                         PageHistory("/somepage", Nil, Nil),
                         PageHistory("/another", Nil, Nil),
                         PageHistory("/next", Nil, List(Flow("8",None), Continuation("2"))))),
               Some(List(Flow("8",None),Continuation("2"))),
               Nil)
    }

  }


  trait FlowStackTest extends Test with SequenceJson {
    override val process: Process = nestedSeqJson.as[Process]
    val session: Session =
      new Session(
        SessionKey("id", process.meta.processCode),
        Some(Published),
        "processId",
        Map("Choice" -> ScalarLabel("Choice",List(phraseThree.english),List(phraseThree.welsh)),
            "Choice_seq" -> ListLabel("Choice_seq",List(phraseThree.english, phraseFour.english),List(phraseThree.welsh, phraseFour.welsh))),
        List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2")),
        Map("6" -> ValueStanza(List(Value(ScalarType,"SecondSeqChoice","Loop value = [label:Choice]")),Vector("end"),false)),
        Map("/start" -> "2,3"),
        List(RawPageHistory("start", Nil, Nil)),
        Nil,
        None,
        Instant.now,
        process.meta.lastUpdate,
        process.meta.timescalesVersion,
        process.meta.ratesVersion
      )
  }

  "SessionFSM with flowStack" must {
    "Return no backlink or updates for any url with no page history, forceForward false (Nil)" in new FlowStackTest {
      verify(fsm("/start", Nil, Nil, false, "/start"),
             None,
             Some(List(PageHistory("/start", Nil, Nil))),
             None,
             Nil)
    }

    "Return no backlink or updates for any url with no page history, forceForward true (Nil)" in new FlowStackTest {
      verify(fsm("/start", Nil, Nil, true, "/start"),
             None,
             Some(List(PageHistory("/start", Nil, Nil))),
             None,
             Nil)
    }

    "Return backlink and no updates for any url with single history, forceForward false (FORWARD)" in new FlowStackTest {
      verify(fsm("/next", List(PageHistory("/start", Nil, Nil)), List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2")), false, "/start"),
             Some("/start"),
             Some(List(PageHistory("/start", Nil, List()),
                       PageHistory("/next", Nil, List(Flow("8",Some(LabelValue("Choice",phraseThree))),Flow("88",Some(LabelValue("Choice",phraseFour))),Continuation("2"))),
                       )),
             None,
             Nil)
    }

    "Return backlink and no updates for any url with single history, forceForward true (FORWARD)" in new FlowStackTest {
      verify(fsm("/next", List(PageHistory("/start", Nil, Nil)), List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2")), true, "/start"),
             Some("/start"),
             Some(List(PageHistory("/start", Nil, List()),
                       PageHistory("/next", Nil, List(Flow("8",Some(LabelValue("Choice",phraseThree))),Flow("88",Some(LabelValue("Choice",phraseFour))),Continuation("2"))),
                       )),
             None,
             Nil)
    }

    "Return no backlink, + PageHistory update for repitition of the last url, with single history, forceForward false (REFRESH)" in new FlowStackTest {
      verify(fsm("/start", List(PageHistory("/start", Nil, Nil)), List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2")), false, "/start"),
             None,
             None,
             None,
             Nil)
    }

    "Return no backlink, PageHistory update for repitition of the last url, with single history, forceForward  true (REFRESH)" in new FlowStackTest {
      verify(fsm("/start", List(PageHistory("/start", Nil, Nil)), List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2")), true, "/start"),
             None,
             None,
             None,
             Nil)
    }

    "Return backlink, + PageHistory update for repitition of the last url, with multiple history, forceForward false (REFRESH)" in new FlowStackTest {
      verify(fsm("/next", List(PageHistory("/start", Nil, Nil), PageHistory("/next", Nil, Nil)), List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2")), false, "/start"),
             Some("/start"),
             None,
             None,
             Nil)
    }

    "Return backlink, PageHistory update for repitition of the last url, with multiple history, forceForward  true (REFRESH)" in new FlowStackTest {
      verify(fsm("/next", List(PageHistory("/start", Nil, Nil), PageHistory("/next", Nil, Nil)), List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2")), true, "/start"),
             Some("/start"),
             None,
             None,
             Nil)
    }

    "Return no backlink, PageHistory update with two element history, forceForward false (BACK)" in new FlowStackTest {
        verify(fsm("/start", List(PageHistory("/start", Nil, Nil), PageHistory("/next", Nil, Nil)), List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2")), false, "/start"),
               None,
               Some(List(PageHistory("/start", Nil, Nil))),
               Some(Nil),
               Nil)
    }

    "Return backlink, PageHistory update with multiple element history, forceForward false (BACK)" in new FlowStackTest {
        verify(fsm("/next", List(PageHistory("/start", Nil, Nil),
                                 PageHistory("/next", Nil, Nil),
                                 PageHistory("/another", Nil, Nil)), List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2")), false, "/start"),
               Some("/start"),
               Some(List(PageHistory("/start", Nil, Nil),
                         PageHistory("/next", Nil, Nil))),
               Some(Nil),
               Nil)
    }

    "Return backlink, PageHistory update and flow label update with multiple element history, forceForward false (BACK)" in new FlowStackTest {
        val pageHistory = List(PageHistory("/fourth", Nil, List(Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2"))),
                             PageHistory("/third", Nil, List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2"))),
                             PageHistory("/start", Nil, List())).reverse
        val flowStack = List(Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2"))

        verify(fsm("/third", pageHistory, flowStack, false, "/start"),
               Some("/start"),
               Some(List(PageHistory("/start", Nil, List()),
                         PageHistory("/third", Nil, List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2"))))),
               Some(List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2"))),
               List(ScalarLabel("Choice",List(phraseThree.english),List(phraseThree.welsh))))
    }

    "Return backlink, PageHistory update with multiple element history, forceForward true (FORWARD to HISTORIC)" in new FlowStackTest {
      val flowStack = List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2"))
      verify(fsm("/next", List(PageHistory("/start", Nil, Nil),
                               PageHistory("/next", Nil, List(Flow("8",Some(LabelValue("Choice",phraseThree))),
                                                        Flow("88",Some(LabelValue("Choice",phraseFour))),
                                                        Continuation("2"))),
                               PageHistory("/another", Nil, Nil)), flowStack, true, "/start"),
             Some("/another"),
             Some(List(PageHistory("/start", Nil, List()),
                       PageHistory("/next", Nil, List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2"))),
                       PageHistory("/another", Nil, List()),
                       PageHistory("/next", Nil, List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2")))
                       )),
             None,
             Nil)
    }

    "Return no backlink, PageHistory update with multiple element history, when returning to first page forceForward false (FORWARD)" in new FlowStackTest {
      val flowStack = List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2"))
        verify(fsm("/start", List(PageHistory("/start", Nil, Nil),
                                  PageHistory("/next", Nil, Nil),
                                  PageHistory("/another", Nil, Nil)), flowStack, true, "/start"),
               None,
               Some(List(PageHistory("/start", Nil, Nil))),
               Some(Nil),
               Nil)
    }

    "Return no backlink, PageHistory update with multiple element history, when returning to first page when no first page visited" in new FlowStackTest {
      val flowStack = List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2"))
      verify(fsm("/start", List(PageHistory("/next", Nil, Nil), PageHistory("/another", Nil, Nil)), flowStack, true, "/start"),
             None,
             Some(List(PageHistory("/start", Nil, Nil))),
             Some(Nil),
             Nil)
    }

    "Return backlink, PageHistory update with multiple element history, when returning to a page not in a sequence flow, forceForward false (FORWARD to HISTORIC)" in new FlowStackTest {
      val flowStack = List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2"))
      verify(fsm("/somepage", List(PageHistory("/start", Nil, Nil),
                                   PageHistory("/next", Nil, List(Flow("8",None),Continuation("2"))),
                                   PageHistory("/somepage", Nil, Nil),
                                   PageHistory("/another", Nil, Nil)), flowStack, true, "/start"),
             Some("/another"),
             Some(List(PageHistory("/start", Nil, Nil),
                       PageHistory("/next", Nil, List(Flow("8",None),Continuation("2"))),
                       PageHistory("/somepage", Nil, Nil),
                       PageHistory("/another", Nil, Nil),
                       PageHistory("/somepage", Nil, Nil))),
             Some(Nil),
             Nil)
    }

    "Return backlink, PageHistory update with multiple element history, when returning to a page within a sequence flow, forceForward false (FORWARD to HISTORIC)" in new FlowStackTest {
      val flowStack = List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2"))

        verify(fsm("/next", List(PageHistory("/start", Nil, Nil),
                                 PageHistory("/next", Nil, List(Flow("8",None),Continuation("2"))),
                                 PageHistory("/somepage", Nil, Nil),
                                 PageHistory("/another", Nil, Nil)), flowStack, true, "/start"),
               Some("/another"),
               Some(List(PageHistory("/start", Nil, Nil),
                         PageHistory("/next", Nil, List(Flow("8",None),Continuation("2"))),
                         PageHistory("/somepage", Nil, Nil),
                         PageHistory("/another", Nil, Nil),
                         PageHistory("/next", Nil, flowStack))),
               None,
               Nil)
    }

  }

  "SessionFSM mergeRevertOperations" must {

    def lbl(name: String, v: String): Label = ScalarLabel(name, List(v), List(v))

    "order priority and other operations correctly" in {

      val priority = List(
        Update(lbl("X","1")),
        Update(lbl("Y","2")),
        Update(lbl("Z","3")),
        Delete("A")
      )
      val others = List(
        Update(lbl("A","4")),
        Update(lbl("B","5")),
        Update(lbl("Z","17")),
        Delete("D")
      )

      val expected = List(
        Update(lbl("X","1")),
        Update(lbl("Y","2")),
        Update(lbl("Z","3")),
        Delete("A"),
        Update(lbl("B","5")),
        Delete("D")
      )

      fsm.mergeRevertOperations(priority, others) shouldBe expected

    }
  }

}
