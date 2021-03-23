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

import javax.inject.{Inject, Singleton}
import core.models.ocelot.{Flow, FlowStage}
import DefaultSessionRepository._
import core.models.ocelot.{Label, ScalarLabel}



@Singleton
class SessionProcessFSM @Inject() () {
  type BackLinkAndStateUpdate = (Option[String], Option[List[PageHistory]], Option[List[FlowStage]], Option[Label])
  // Input
  // url ,incoming url
  // priorSp, prior SessionProcess corresponding to the previous url processed. Note. The db record will have the head of the page history updated
  //          to include incoming url and flowStack Nil, this update is not within priorSp. This is an optimisation, as the most common transition
  //          is forward in a process containing no Sequences (i.e. the flowStack will always be empty), this transition will result in
  //          no page history or flowstack updates given the update described has already taken place.
  // forceForward, true indicates that a url similar to head of prior history (looks like a BACK) should be treated as a forward movement
  // sentinelUrl. generally url of the first page, arrival here will always clear down the page history

  // New state == (priorSp + new Pagehistory head) ++ pageHistory and flowStack output updates
  //
  // Output
  // optional backlink to be displayed on page with incoming url
  // optional page history update
  // optional flowStack update
  def apply(url: String, priorSp: SessionProcess, forceForward: Boolean, sentinelUrl: String): BackLinkAndStateUpdate =
    priorSp.pageHistory.reverse match {
      // Initial page
      case Nil =>
        println(s"*** NIL")
        (None, None, None, None)

      // REFRESH: new url equals current url
      case x :: xs if x.url == url =>
        println(s"*** REFRESH")
        (xs.headOption.map(_.url), Some(priorSp.pageHistory), None, None)

      // BACK: new url equals previous url and prior flowStack equals the previous flowStack
      case _ :: y :: xs if y.url == url && !forceForward && priorSp.flowStack == y.flowStack =>
        println(s"*** BACK: new url equals previous url")
        (xs.headOption.map(_.url), Some((y :: xs).reverse), None, None)

      // BACK: flowStack change
      case _ :: y :: xs if y.url == url && !forceForward =>
        println(s"*** BACK: flowStack change")
        val labelValue: Option[ScalarLabel] = y.flowStack.headOption.collect{case Flow(_, Some(lv)) => ScalarLabel(lv.name, List(lv.value), Nil)}
        (xs.headOption.map(_.url), Some((y :: xs).reverse), Some(y.flowStack), labelValue)

      // FORWARD to first page of guidance
      case x :: xs if url == sentinelUrl =>
        println(s"*** FORWARD")
        findPreviousFlowAndLabelState(url, priorSp.pageHistory).fold[BackLinkAndStateUpdate]((None, Some(List(PageHistory(url, Nil))), Some(Nil), None)){t =>
          val (labelValue, flowStack) = t
          (None, Some(List(PageHistory(url, flowStack))), Some(flowStack), labelValue)
        }

      // FORWARD with a non-empty flowStack
      case x :: xs if priorSp.flowStack.nonEmpty =>
        println(s"*** FORWARD with a non-empty flowStack")
        // Check for forward  movement to a previous page (possibly from CYA)
        findPreviousFlowAndLabelState(url, priorSp.pageHistory).fold[BackLinkAndStateUpdate]((Some(x.url), Some((PageHistory(url, priorSp.flowStack) :: x :: xs).reverse), None, None)){t =>
          val (labelValue, flowStack) = t
          (Some(x.url), Some((PageHistory(url, flowStack) :: x :: xs).reverse), Some(flowStack), labelValue)
        }

      // FORWARD with empty flowStack
      case x :: xs =>
        println(s"*** FORWARD with empty flowStack")
        // Check for forward  movement to a previous page (CYA)
        findPreviousFlowAndLabelState(url, priorSp.pageHistory).fold[BackLinkAndStateUpdate]((Some(x.url), None, None, None)){
          case (labelValue, Nil) =>
            (Some(x.url), None, None, None)
          case (labelValue, flowStack) =>
            (Some(x.url), Some((PageHistory(url, flowStack) :: x :: xs).reverse), Some(flowStack), labelValue)
        }
    }

  private def findPreviousFlowAndLabelState(url: String, pageHistory: List[PageHistory]): Option[(Option[Label], List[FlowStage])] =
    pageHistory.find(_.url == url).fold[Option[(Option[Label], List[FlowStage])]](None){ph =>
      val labelValue: Option[Label] = ph.flowStack.headOption.collect{case Flow(_, Some(lv)) => ScalarLabel(lv.name, List(lv.value), Nil)}
      Some((labelValue, ph.flowStack))
    }
}

