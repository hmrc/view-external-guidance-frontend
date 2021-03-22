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
import core.models.ocelot.{Process, ProcessJson, SequenceJson, FlowStage, ScalarLabel, ListLabel, Flow, Continuation, Label, LabelValue}
import DefaultSessionRepository._
import java.time.Instant

class SessionProcessFSMSpec extends BaseSpec {
  type BackLinkAndStateUpdate = (Option[String], Option[List[PageHistory]], Option[List[FlowStage]], Option[Label])
  val fsm = new SessionProcessFSM
  def verify(fsmOutput: BackLinkAndStateUpdate, bl: Option[String], ph: Option[List[PageHistory]], fs: Option[List[FlowStage]], l: Option[Label]): Unit = {
    fsmOutput._1 shouldBe bl
    fsmOutput._2 shouldBe ph
    fsmOutput._3 shouldBe fs
    fsmOutput._4 shouldBe l
  }

  trait Test extends ProcessJson {
    val sessionProcess: SessionProcess =
      new SessionProcess(
        "id",
        "processId",
        validOnePageJson.as[Process],
        Map(),
        Nil,
        Map(),
        Map(),
        Map(),
        List(PageHistory("/start", Nil)),
        Instant.now
      )
  }

  "SessionProcessFSM with no flowStack" must {
    "Return no backlink or updates for any url with no page history, forceForward false (Nil)" in new Test {
      verify(fsm("/start", SessionProcess("id", "processId", validOnePageJson.as[Process]), false),
             None,
             None,
             None,
             None)
    }

    "Return no backlink or updates for any url with no page history, forceForward true (Nil)" in new Test {
      verify(fsm("/start", SessionProcess("id", "processId", validOnePageJson.as[Process]), true),
             None,
             None,
             None,
             None)
    }

    "Return backlink and no updates for any url with single history, forceForward false (FORWARD)" in new Test {
      verify(fsm("/next", sessionProcess, false),
             Some("/start"),
             None,
             None,
             None)
    }

    "Return backlink and no updates for any url with single history, forceForward true (FORWARD)" in new Test {
      verify(fsm("/next", sessionProcess, true),
             Some("/start"),
             None,
             None,
             None)
    }

    "Return no backlink, + PageHistory update for repitition of the last url, with single history, forceForward false (REFRESH)" in new Test {
      verify(fsm("/start", sessionProcess, false),
             None,
             Some(List(PageHistory("/start", Nil))),
             None,
             None)
    }

    "Return no backlink, PageHistory update for repitition of the last url, with single history, forceForward  true (REFRESH)" in new Test {
      verify(fsm("/start", sessionProcess, true),
             None,
             Some(List(PageHistory("/start", Nil))),
             None,
             None)
    }

    "Return backlink, + PageHistory update for repitition of the last url, with multiple history, forceForward false (REFRESH)" in new Test {
      verify(fsm("/next", sessionProcess.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), false),
             Some("/start"),
             Some(List(PageHistory("/start", Nil), PageHistory("/next", Nil))),
             None,
             None)
    }

    "Return backlink, PageHistory update for repitition of the last url, with multiple history, forceForward  true (REFRESH)" in new Test {
      verify(fsm("/next", sessionProcess.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), true),
             Some("/start"),
             Some(List(PageHistory("/start", Nil), PageHistory("/next", Nil))),
             None,
             None)
    }

    "Return no backlink, PageHistory update with two element history, forceForward false (BACK)" in new Test {
        verify(fsm("/start", sessionProcess.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), false),
               None,
               Some(List(PageHistory("/start", Nil))),
               None,
               None)
    }

    "Return backlink, PageHistory update with multiple element history, forceForward false (BACK)" in new Test {
        verify(fsm("/next", sessionProcess.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil), PageHistory("/another", Nil))), false),
               Some("/start"),
               Some(List(PageHistory("/start", Nil), PageHistory("/next", Nil))),
               None,
               None)
    }

    "Return backlink, No updates with two element history, forceForward true (FORCE FORWARD)" in new Test {
        verify(fsm("/start", sessionProcess.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), true),
               Some("/next"),
               None,
               None,
               None)
    }
  }

  trait FlowStackTest extends SequenceJson {
    val sessionProcess: SessionProcess =
      new SessionProcess(
        "id",
        "processId",
        nestedSeqJson.as[Process],
        Map("Choice" -> ScalarLabel("Choice",List("Third"),List()), "Choice_seq" -> ListLabel("Choice_seq",List("Third", "Fourth"),List())),
        List(Flow("8",Some(LabelValue("Choice","Third"))), Flow("88",Some(LabelValue("Choice","Fourth"))), Continuation("2")),
        Map("6" -> ValueStanza(List(Value(ScalarType,"SecondSeqChoice","Loop value = [label:Choice]")),Vector("end"),false)),
        Map("/done" -> "2", "/one" -> "4", "/third" -> "8", "/start" -> "start", "/fourth" -> "88"),
        Map("/start" -> "2,3"),
        List(PageHistory("/start", Nil)),
        Instant.now
      )
  }

  "SessionProcessFSM with flowStack" must {
    "Return no backlink or updates for any url with no page history, forceForward false (Nil)" in new FlowStackTest {
      verify(fsm("/start", SessionProcess("id", "processId", nestedSeqJson.as[Process]), false),
             None,
             None,
             None,
             None)
    }

    "Return no backlink or updates for any url with no page history, forceForward true (Nil)" in new FlowStackTest {
      verify(fsm("/start", SessionProcess("id", "processId", nestedSeqJson.as[Process]), true),
             None,
             None,
             None,
             None)
    }

    "Return backlink and no updates for any url with single history, forceForward false (FORWARD)" in new FlowStackTest {
      verify(fsm("/next", sessionProcess, false),
             Some("/start"),
             Some(List(PageHistory("/start",List()),
                       PageHistory("/next",
                          List(Flow("8",Some(LabelValue("Choice","Third"))),Flow("88",Some(LabelValue("Choice","Fourth"))),Continuation("2"))),
                       )),
             None,
             None)
    }

    "Return backlink and no updates for any url with single history, forceForward true (FORWARD)" in new FlowStackTest {
      verify(fsm("/next", sessionProcess, true),
             Some("/start"),
             Some(List(PageHistory("/start",List()),
                       PageHistory("/next",
                          List(Flow("8",Some(LabelValue("Choice","Third"))),Flow("88",Some(LabelValue("Choice","Fourth"))),Continuation("2"))),
                       )),
             None,
             None)
    }

    "Return no backlink, + PageHistory update for repitition of the last url, with single history, forceForward false (REFRESH)" in new FlowStackTest {
      verify(fsm("/start", sessionProcess, false),
             None,
             Some(List(PageHistory("/start", Nil))),
             None,
             None)
    }

    "Return no backlink, PageHistory update for repitition of the last url, with single history, forceForward  true (REFRESH)" in new FlowStackTest {
      verify(fsm("/start", sessionProcess, true),
             None,
             Some(List(PageHistory("/start", Nil))),
             None,
             None)
    }

    "Return backlink, + PageHistory update for repitition of the last url, with multiple history, forceForward false (REFRESH)" in new FlowStackTest {
      verify(fsm("/next", sessionProcess.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), false),
             Some("/start"),
             Some(List(PageHistory("/start", Nil),
                       PageHistory("/next", Nil))),
             None,
             None)
    }

    "Return backlink, PageHistory update for repitition of the last url, with multiple history, forceForward  true (REFRESH)" in new FlowStackTest {
      verify(fsm("/next", sessionProcess.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), true),
             Some("/start"),
             Some(List(PageHistory("/start", Nil),
                       PageHistory("/next", Nil))),
             None,
             None)
    }

    "Return no backlink, PageHistory update with two element history, forceForward false (BACK)" in new FlowStackTest {
        verify(fsm("/start", sessionProcess.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), false),
               None,
               Some(List(PageHistory("/start", Nil))),
               Some(Nil),
               None)
    }

    "Return backlink, PageHistory update with multiple element history, forceForward false (BACK)" in new FlowStackTest {
        verify(fsm("/next", sessionProcess.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil), PageHistory("/another", Nil))), false),
               Some("/start"),
               Some(List(PageHistory("/start", Nil),
                         PageHistory("/next", Nil))),
               Some(Nil),
               None)
    }

    "Return backlink, PageHistory update and flow label update with multiple element history, forceForward false (BACK)" in new FlowStackTest {
        val sp: SessionProcess = sessionProcess.copy(
          pageHistory = List(PageHistory("/fourth",List(Flow("88",Some(LabelValue("Choice","Fourth"))), Continuation("2"))),
                             PageHistory("/third",List(Flow("8",Some(LabelValue("Choice","Third"))), Flow("88",Some(LabelValue("Choice","Fourth"))), Continuation("2"))),
                             PageHistory("/start",List())).reverse,
          flowStack = List(Flow("88",Some(LabelValue("Choice","Fourth"))), Continuation("2"))
        )
        verify(fsm("/third", sp, false),
               Some("/start"),
               Some(List(PageHistory("/start",List()),
                         PageHistory("/third",List(Flow("8",Some(LabelValue("Choice","Third"))), Flow("88",Some(LabelValue("Choice","Fourth"))), Continuation("2"))))),
               Some(List(Flow("8",Some(LabelValue("Choice","Third"))), Flow("88",Some(LabelValue("Choice","Fourth"))), Continuation("2"))),
               Some(ScalarLabel("Choice",List("Third"),List())))
    }

    "Return backlink, No updates with two element history, forceForward true (FORCE FORWARD)" in new FlowStackTest {
        verify(fsm("/start", sessionProcess.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), true),
               Some("/next"),
               Some(List(PageHistory("/start",List()),
                         PageHistory("/next",List()),
                         PageHistory("/start",List(Flow("8",Some(LabelValue("Choice","Third"))), Flow("88",Some(LabelValue("Choice","Fourth"))), Continuation("2")))
                         )),
               None,
               None)
    }
  }

}
