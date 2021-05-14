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

import scala.annotation.tailrec
import javax.inject.{Inject, Singleton}
import core.models.ocelot.{Flow, Continuation, LabelValue, FlowStage}
import DefaultSessionRepository._
import core.models.ocelot.{Label, ScalarLabel}


@Singleton
class SessionProcessFSM @Inject() () {
  type BackLinkAndStateUpdate = (Option[String], Option[List[PageHistory]], Option[List[FlowStage]], List[Label])
  // Input
  // url: incoming url
  // priorSp: prior SessionProcess corresponding to the previous url processed. Note. The db record will have the head of the page history updated
  //          to include incoming url and flowStack Nil, this update is not within priorSp.
  //          The most common transition will generally be forward in a process containing no Sequences (i.e. the flowStack will always be empty),
  //          this transition will result in no page history or flowstack updates given the update described has already taken place.
  //
  // forceForward: true indicates that a url similar to head of prior history (looks like a BACK) should be treated as a forward movement
  // sentinelUrl: generally url of the first page, arrival here will always clear down the page history

  // New state == (priorSp + new Pagehistory head) ++ pageHistory and flowStack output updates
  //
  // Output
  // optional backlink to be displayed on page with incoming url
  // optional page history update
  // optional flowStack update
  // Flow Labels update (Nil unless flowstack active in from or to page)
  def apply(url: String, priorSp: SessionProcess, forceForward: Boolean, sentinelUrl: String): BackLinkAndStateUpdate =
    priorSp.pageHistory.reverse match {
      // Initial page
      case Nil =>
        (None, Some(List(PageHistory(url, Nil))), None, Nil)

      // REFRESH: new url equals current url
      case x :: xs if x.url == url =>
        (xs.headOption.map(_.url), None, None, Nil)

      // BACK: new url equals previous url and prior flowStack equals the previous flowStack
      case _ :: y :: xs if y.url == url && !forceForward && priorSp.flowStack == y.flowStack =>
        (xs.headOption.map(_.url), Some((y :: xs).reverse), None, Nil)

      // BACK: flowStack change
      case _ :: y :: xs if y.url == url && !forceForward =>
        (xs.headOption.map(_.url), Some((y :: xs).reverse), Some(y.flowStack), pageHistoryLabelValues(y.flowStack))

      // FORWARD to first page of guidance
      case x :: xs if url == sentinelUrl =>
        findPreviousFlowAndLabelState(url, priorSp.pageHistory).fold[BackLinkAndStateUpdate](
          (None, Some(List(PageHistory(url, Nil))), Some(Nil), Nil)
        ){t =>
          val (labelValue, flowStack) = t
          (None, Some(List(PageHistory(url, flowStack))), Some(flowStack), labelValue)
        }

      // FORWARD from a non-empty flowStack
      case x :: xs if priorSp.flowStack.nonEmpty => // Check for forward  movement to a previous page (possibly from CYA)
        findPreviousFlowAndLabelState(url, priorSp.pageHistory).fold[BackLinkAndStateUpdate]{
          (Some(x.url), Some((PageHistory(url, priorSp.flowStack) :: x :: xs).reverse), None, Nil)
        }{
          case (_, Nil) =>
            (Some(x.url), Some((PageHistory(url, Nil) :: x :: xs).reverse), Some(Nil), Nil)
          case (_, flowStack) =>
            (Some(x.url), Some((PageHistory(url, priorSp.flowStack) :: x :: xs).reverse), None, Nil)
        }

      // FORWARD from empty flowStack
      case x :: xs => // Check for forward  movement to a previous page (CYA)
        findPreviousFlowAndLabelState(url, priorSp.pageHistory).fold[BackLinkAndStateUpdate]{
          (Some(x.url), Some((PageHistory(url, priorSp.flowStack) :: x :: xs).reverse), None, Nil)
        }{
          case (_, Nil) =>
            (Some(x.url), Some((PageHistory(url, Nil) :: x :: xs).reverse), None, Nil)
          case (labels, flowStack) =>
            (Some(x.url), Some((PageHistory(url, flowStack) :: x :: xs).reverse), Some(flowStack), labels)
        }
  }

  private type LabelAndFlowStack = Option[(List[Label], List[FlowStage])]

  private def findPreviousFlowAndLabelState(url: String, pageHistory: List[PageHistory]): LabelAndFlowStack =
    pageHistory.find(_.url == url).fold[LabelAndFlowStack](None){ph => Some((pageHistoryLabelValues(ph.flowStack), ph.flowStack))}

  // Pull out the current Flow label values from a given flow stack
  private def pageHistoryLabelValues(fs: List[FlowStage]): List[Label] =
    labelList(fs, Nil).map(lv => ScalarLabel(lv.name, List(lv.value), Nil))

  @tailrec
  private def labelList(fs: List[FlowStage], acc: List[LabelValue]): List[LabelValue] =
    fs match {
      case Nil => acc
      case Flow(_, Some(lv)) :: xs => labelList(dropFlow(xs), lv :: acc)
      case _ :: xs => labelList(xs, acc)
    }

  @tailrec
  private def dropFlow(fs: List[FlowStage]): List[FlowStage] =
    fs match {
      case Nil => Nil
      case (_: Continuation) :: xs => xs
      case _ :: xs => dropFlow(xs)
    }
}
