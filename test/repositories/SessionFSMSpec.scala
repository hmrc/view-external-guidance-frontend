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

package repositories

import base.BaseSpec
import core.models.ocelot.stanzas.{ValueStanza, Value, ScalarType}
import core.models.ocelot.{Process, ProcessJson, SequenceJson, FlowStage, ScalarLabel, ListLabel, Flow, Continuation, Label, LabelValue, Phrase}
import models.PageNext
import java.time.Instant

class SessionFSMSpec extends BaseSpec {
  type BackLinkAndStateUpdate = (Option[String], Option[List[PageHistory]], Option[List[FlowStage]], List[Label])
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

    val session: Session =
      new Session(
        "id",
        "processId",
        validOnePageJson.as[Process],
        Map(),
        Nil,
        Map(),
        Map(),
        Map(),
        List(PageHistory("/start", Nil)),
        Nil,
        Instant.now
      )
  }

  "SessionFSM with no flowStack" must {
    "Return no backlink or updates for any url with no page history, forceForward false (Nil)" in new Test {
      verify(fsm("/start", Session("id", "processId", validOnePageJson.as[Process]), false, "/start"),
             None,
             Some(List(PageHistory("/start", Nil))),
             None,
             Nil)
    }

    "Return no backlink or updates for any url with no page history, forceForward true (Nil)" in new Test {
      verify(fsm("/start", Session("id", "processId", validOnePageJson.as[Process]), true, "/start"),
             None,
             Some(List(PageHistory("/start", Nil))),
             None,
             Nil)
    }

    "Return backlink and no updates for any url with single history, forceForward false (FORWARD)" in new Test {
      verify(fsm("/next", session, false, "/start"),
             Some("/start"),
             Some(List(PageHistory("/start", Nil), PageHistory("/next",Nil))),
             None,
             Nil)
    }

    "Return backlink and no updates for any url with single history, forceForward true (FORWARD)" in new Test {
      verify(fsm("/next", session, true, "/start"),
             Some("/start"),
             Some(List(PageHistory("/start", Nil), PageHistory("/next",Nil))),
             None,
             Nil)
    }

    "Return no backlink, + PageHistory update for repitition of the last url, with single history, forceForward false (REFRESH)" in new Test {
      verify(fsm("/start", session, false, "/start"),
             None,
             None,
             None,
             Nil)
    }

    "Return no backlink, PageHistory update for repitition of the last url, with single history, forceForward  true (REFRESH)" in new Test {
      verify(fsm("/start", session, true, "/start"),
             None,
             None,
             None,
             Nil)
    }

    "Return backlink, + PageHistory update for repitition of the last url, with multiple history, forceForward false (REFRESH)" in new Test {
      verify(fsm("/next", session.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), false, "/start"),
             Some("/start"),
             None,
             None,
             Nil)
    }

    "Return backlink, PageHistory update for repitition of the last url, with multiple history, forceForward  true (REFRESH)" in new Test {
      verify(fsm("/next", session.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), true, "/start"),
             Some("/start"),
             None,
             None,
             Nil)
    }

    "Return no backlink, PageHistory update with two element history, forceForward false (BACK)" in new Test {
        verify(fsm("/start", session.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), false, "/start"),
               None,
               Some(List(PageHistory("/start", Nil))),
               None,
               Nil)
    }

    "Return backlink, PageHistory update with multiple element history, forceForward false (BACK)" in new Test {
        verify(fsm("/next", session.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil), PageHistory("/another", Nil))), false, "/start"),
               Some("/start"),
               Some(List(PageHistory("/start", Nil), PageHistory("/next", Nil))),
               None,
               Nil)
    }

    "Return backlink, No updates with multiple element history, forceForward true (FORWARD to HISTORIC)" in new Test {
        verify(fsm("/next", session.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil), PageHistory("/another", Nil))), true, "/start"),
               Some("/another"),
               Some(List(PageHistory("/start", Nil), PageHistory("/next", Nil), PageHistory("/another", Nil), PageHistory("/next", Nil))),
               None,
               Nil)
    }

    "Return no backlink, PageHistory update with multiple element history, when returning to first page forceForward false (FORWARD)" in new Test {
        verify(fsm("/start", session.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil), PageHistory("/another", Nil))), true, "/start"),
               None,
               Some(List(PageHistory("/start", Nil))),
               Some(Nil),
               Nil)
    }

    "Return backlink, PageHistory update with multiple element history, when returning to a page not in a sequence flow, forceForward false (FORWARD to HISTORIC)" in new Test {
        verify(fsm("/next", session.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil), PageHistory("/somepage", Nil), PageHistory("/another", Nil))), true, "/start"),
               Some("/another"),
               Some(List(PageHistory("/start", Nil), PageHistory("/next", Nil), PageHistory("/somepage", Nil), PageHistory("/another", Nil), PageHistory("/next", Nil))),
               None,
               Nil)
    }

    "Return backlink, PageHistory update with multiple element history, when returning to a page within a sequence flow, forceForward false (FORWARD to HISTORIC)" in new Test {
        verify(fsm("/next", session.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", List(Flow("8",None),Continuation("2"))), PageHistory("/somepage", Nil), PageHistory("/another", Nil))), true, "/start"),
               Some("/another"),
               Some(List(PageHistory("/start", Nil),
                         PageHistory("/next", List(Flow("8",None),Continuation("2"))),
                         PageHistory("/somepage", Nil),
                         PageHistory("/another", Nil),
                         PageHistory("/next", List(Flow("8",None), Continuation("2"))))),
               Some(List(Flow("8",None),Continuation("2"))),
               Nil)
    }

  }

  trait FlowStackTest extends Test with SequenceJson {
    override val session: Session =
      new Session(
        "id",
        "processId",
        nestedSeqJson.as[Process],
        Map("Choice" -> ScalarLabel("Choice",List(phraseThree.english),List(phraseThree.welsh)),
            "Choice_seq" -> ListLabel("Choice_seq",List(phraseThree.english, phraseFour.english),List(phraseThree.welsh, phraseFour.welsh))),
        List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2")),
        Map("6" -> ValueStanza(List(Value(ScalarType,"SecondSeqChoice","Loop value = [label:Choice]")),Vector("end"),false)),
        Map("/done" -> PageNext("2"), "/one" -> PageNext("4"), "/third" -> PageNext("8"), "/start" -> PageNext("start"), "/fourth" -> PageNext("88")),
        Map("/start" -> "2,3"),
        List(PageHistory("/start", Nil)),
        Nil,
        Instant.now
      )
  }

  "SessionFSM with flowStack" must {
    "Return no backlink or updates for any url with no page history, forceForward false (Nil)" in new FlowStackTest {
      verify(fsm("/start", Session("id", "processId", nestedSeqJson.as[Process]), false, "/start"),
             None,
             Some(List(PageHistory("/start", Nil))),
             None,
             Nil)
    }

    "Return no backlink or updates for any url with no page history, forceForward true (Nil)" in new FlowStackTest {
      verify(fsm("/start", Session("id", "processId", nestedSeqJson.as[Process]), true, "/start"),
             None,
             Some(List(PageHistory("/start", Nil))),
             None,
             Nil)
    }

    "Return backlink and no updates for any url with single history, forceForward false (FORWARD)" in new FlowStackTest {
      verify(fsm("/next", session, false, "/start"),
             Some("/start"),
             Some(List(PageHistory("/start",List()),
                       PageHistory("/next",
                          List(Flow("8",Some(LabelValue("Choice",phraseThree))),Flow("88",Some(LabelValue("Choice",phraseFour))),Continuation("2"))),
                       )),
             None,
             Nil)
    }

    "Return backlink and no updates for any url with single history, forceForward true (FORWARD)" in new FlowStackTest {
      verify(fsm("/next", session, true, "/start"),
             Some("/start"),
             Some(List(PageHistory("/start",List()),
                       PageHistory("/next",
                          List(Flow("8",Some(LabelValue("Choice",phraseThree))),Flow("88",Some(LabelValue("Choice",phraseFour))),Continuation("2"))),
                       )),
             None,
             Nil)
    }

    "Return no backlink, + PageHistory update for repitition of the last url, with single history, forceForward false (REFRESH)" in new FlowStackTest {
      verify(fsm("/start", session, false, "/start"),
             None,
             None,
             None,
             Nil)
    }

    "Return no backlink, PageHistory update for repitition of the last url, with single history, forceForward  true (REFRESH)" in new FlowStackTest {
      verify(fsm("/start", session, true, "/start"),
             None,
             None,
             None,
             Nil)
    }

    "Return backlink, + PageHistory update for repitition of the last url, with multiple history, forceForward false (REFRESH)" in new FlowStackTest {
      verify(fsm("/next", session.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), false, "/start"),
             Some("/start"),
             None,
             None,
             Nil)
    }

    "Return backlink, PageHistory update for repitition of the last url, with multiple history, forceForward  true (REFRESH)" in new FlowStackTest {
      verify(fsm("/next", session.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), true, "/start"),
             Some("/start"),
             None,
             None,
             Nil)
    }

    "Return no backlink, PageHistory update with two element history, forceForward false (BACK)" in new FlowStackTest {
        verify(fsm("/start", session.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), false, "/start"),
               None,
               Some(List(PageHistory("/start", Nil))),
               Some(Nil),
               Nil)
    }

    "Return backlink, PageHistory update with multiple element history, forceForward false (BACK)" in new FlowStackTest {
        verify(fsm("/next", session.copy(pageHistory = List(PageHistory("/start", Nil),
                                                                   PageHistory("/next", Nil),
                                                                   PageHistory("/another", Nil))), false, "/start"),
               Some("/start"),
               Some(List(PageHistory("/start", Nil),
                         PageHistory("/next", Nil))),
               Some(Nil),
               Nil)
    }

    "Return backlink, PageHistory update and flow label update with multiple element history, forceForward false (BACK)" in new FlowStackTest {
        val sp: Session = session.copy(
          pageHistory = List(PageHistory("/fourth",List(Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2"))),
                             PageHistory("/third",List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2"))),
                             PageHistory("/start",List())).reverse,
          flowStack = List(Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2"))
        )
        verify(fsm("/third", sp, false, "/start"),
               Some("/start"),
               Some(List(PageHistory("/start",List()),
                         PageHistory("/third",List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2"))))),
               Some(List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2"))),
               List(ScalarLabel("Choice",List(phraseThree.english),List(phraseThree.welsh))))
    }

    "Return backlink, PageHistory update with multiple element history, forceForward true (FORWARD to HISTORIC)" in new FlowStackTest {
        verify(fsm("/next",
               session.copy(
                 pageHistory = List(PageHistory("/start", Nil),
                                    PageHistory("/next", List(Flow("8",Some(LabelValue("Choice",phraseThree))),
                                                              Flow("88",Some(LabelValue("Choice",phraseFour))),
                                                              Continuation("2"))),
                                    PageHistory("/another", Nil))), true, "/start"),
               Some("/another"),
               Some(List(PageHistory("/start",List()),
                         PageHistory("/next",List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2"))),
                         PageHistory("/another",List()),
                         PageHistory("/next",List(Flow("8",Some(LabelValue("Choice",phraseThree))), Flow("88",Some(LabelValue("Choice",phraseFour))), Continuation("2")))
                         )),
               None,
               Nil)
    }

    "Return no backlink, PageHistory update with multiple element history, when returning to first page forceForward false (FORWARD)" in new FlowStackTest {
        verify(fsm("/start", session.copy(pageHistory = List(PageHistory("/start", Nil),
                                                                    PageHistory("/next", Nil),
                                                                    PageHistory("/another", Nil))), true, "/start"),
               None,
               Some(List(PageHistory("/start", Nil))),
               Some(Nil),
               Nil)
    }

    "Return no backlink, PageHistory update with multiple element history, when returning to first page when no first page visited" in new Test {
        verify(fsm("/start", session.copy(pageHistory = List(PageHistory("/next", Nil), PageHistory("/another", Nil))), true, "/start"),
               None,
               Some(List(PageHistory("/start", Nil))),
               Some(Nil),
               Nil)
    }

    "Return backlink, PageHistory update with multiple element history, when returning to a page not in a sequence flow, forceForward false (FORWARD to HISTORIC)" in new FlowStackTest {
        verify(fsm("/somepage", session.copy(pageHistory = List(PageHistory("/start", Nil),
                                                                       PageHistory("/next", List(Flow("8",None),Continuation("2"))),
                                                                       PageHistory("/somepage", Nil),
                                                                       PageHistory("/another", Nil))), true, "/start"),
               Some("/another"),
               Some(List(PageHistory("/start", Nil),
                         PageHistory("/next", List(Flow("8",None),Continuation("2"))),
                         PageHistory("/somepage", Nil),
                         PageHistory("/another", Nil),
                         PageHistory("/somepage", Nil))),
               Some(Nil),
               Nil)
    }

    "Return backlink, PageHistory update with multiple element history, when returning to a page within a sequence flow, forceForward false (FORWARD to HISTORIC)" in new FlowStackTest {
        val sp = session.copy(pageHistory = List(PageHistory("/start", Nil),
                                                        PageHistory("/next", List(Flow("8",None),Continuation("2"))),
                                                        PageHistory("/somepage", Nil),
                                                        PageHistory("/another", Nil)))

        verify(fsm("/next", sp, true, "/start"),
               Some("/another"),
               Some(List(PageHistory("/start", Nil),
                         PageHistory("/next", List(Flow("8",None),Continuation("2"))),
                         PageHistory("/somepage", Nil),
                         PageHistory("/another", Nil),
                         PageHistory("/next", sp.flowStack))),
               None,
               Nil)
    }

  }

}