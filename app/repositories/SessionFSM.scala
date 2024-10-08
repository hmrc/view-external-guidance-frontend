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

import scala.annotation.tailrec
import javax.inject.{Inject, Singleton}
import core.models.ocelot.{Flow, Continuation, LabelValue, FlowStage, LabelOperation}
import core.models.ocelot.{Label, ScalarLabel}
import models._

@Singleton
class SessionFSM @Inject() () {
  type BackLinkAndStateUpdate = (Option[String], Option[List[PageHistory]], Option[List[FlowStage]], List[Label], List[LabelOperation])
  // Input
  // url: incoming url
  // priorHistory: prior Session pageHistory corresponding to the previous url processed.
  // priorFlowStack: prior Session FlowStack corresponding to the previous url processed.
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
  def apply(url: String, priorHistory: List[PageHistory], priorFlowStack: List[FlowStage], forceForward: Boolean, sentinelUrl: String): BackLinkAndStateUpdate =
    priorHistory.reverse match {
      // Initial page
      case Nil => (None, Some(List(PageHistory(url, Nil, Nil))), None, Nil, Nil)

      // REFRESH: new url equals current url with same flowPath
      case x :: xs if x.url == url => (xs.headOption.map(_.url), None, None, Nil, x.revertOps)

      // BACK: new url equals previous url and prior flowStack equals the previous flowStack
      case x :: y :: xs if y.url == url && !forceForward && priorFlowStack == y.flowStack =>
        (xs.headOption.map(_.url), Some((y :: xs).reverse), None, Nil, mergeRevertOperations(x.revertOps, y.revertOps))

      // BACK: flowStack change
      case x :: y :: xs if y.url == url && !forceForward =>
        (xs.headOption.map(_.url), Some((y :: xs).reverse), Some(y.flowStack), pageHistoryLabelValues(y.flowStack), mergeRevertOperations(x.revertOps, y.revertOps))

      // FORWARD to first page of guidance
      case x :: _ if url == sentinelUrl =>
        findPreviousFlowAndLabelState(url, priorHistory).fold[BackLinkAndStateUpdate](
          (None, Some(List(PageHistory(url, Nil, Nil))), Some(Nil), Nil, x.revertOps)
        ){t =>
          val (labelValue, flowStack) = t
          (None, Some(List(PageHistory(url, Nil, flowStack))), Some(flowStack), labelValue, Nil)
        }

      // FORWARD from a non-empty flowStack
      case x :: xs if priorFlowStack.nonEmpty => // Check for forward  movement to a previous page (possibly from CYA)
        findPreviousFlowAndLabelState(url, priorHistory).fold[BackLinkAndStateUpdate]{
          (Some(x.url), Some((PageHistory(url, Nil, priorFlowStack) :: x :: xs).reverse), None, Nil, Nil)
        }{
          case (_, Nil) => (Some(x.url), Some((PageHistory(url, Nil, Nil) :: x :: xs).reverse), Some(Nil), Nil, Nil)
          case _ => (Some(x.url), Some((PageHistory(url, Nil, priorFlowStack) :: x :: xs).reverse), None, Nil, Nil)
        }

      // FORWARD from empty flowStack
      case x :: xs => // Check for forward  movement to a previous page (CYA)
        findPreviousFlowAndLabelState(url, priorHistory).fold[BackLinkAndStateUpdate]{
          (Some(x.url), Some((PageHistory(url, Nil, priorFlowStack) :: x :: xs).reverse), None, Nil, Nil)
        }{
          case (_, Nil) =>
            (Some(x.url), Some((PageHistory(url, Nil, Nil) :: x :: xs).reverse), None, Nil, Nil)
          case (labels, flowStack) =>
            (Some(x.url), Some((PageHistory(url, Nil, flowStack) :: x :: xs).reverse), Some(flowStack), labels, Nil)
        }
  }

  private type LabelAndFlowStack = Option[(List[Label], List[FlowStage])]

  // Merge two lists of revert operations without duplicates with one (priorityOps) taking precedence
  private[repositories] def mergeRevertOperations(priorityOps: List[LabelOperation], othersOps: List[LabelOperation]): List[LabelOperation] =
    priorityOps ++ othersOps.filterNot(o => priorityOps.exists(_.name == o.name))

  private def findPreviousFlowAndLabelState(url: String, pageHistory: List[PageHistory]): LabelAndFlowStack =
    pageHistory.find(_.url == url).fold[LabelAndFlowStack](None){ph => Some((pageHistoryLabelValues(ph.flowStack), ph.flowStack))}

  // Pull out the current Flow label values from a given flow stack
  private def pageHistoryLabelValues(fs: List[FlowStage]): List[Label] =
    labelList(fs, Nil).map(lv => ScalarLabel(lv.name, List(lv.value.english), List(lv.value.welsh)))

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
