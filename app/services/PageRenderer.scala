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

package services

import javax.inject.{Inject, Singleton}
import scala.annotation.tailrec
import core.models.ocelot.stanzas.{PageStanza, EndStanza, VisualStanza, Stanza, Evaluate, DataInput}
import core.models.ocelot.{Page, Labels, Process}

@Singleton
class PageRenderer @Inject() () {

  def renderPage(page: Page, labels: Labels): (Seq[VisualStanza], Labels, Option[DataInput]) = {
    implicit val stanzaMap: Map[String, Stanza] = page.keyedStanzas.map(ks => (ks.key, ks.stanza)).toMap ++ labels.continuationPool
    val (visualStanzas, newLabels, _, _, optionalInput) = evaluateStanzas(stanzaMap(page.id).next.head, labels)
    (visualStanzas, newLabels, optionalInput)
  }

  def renderPagePostSubmit(page: Page, labels: Labels, answer: String): (Option[String], Labels) = {

    @tailrec
    def evaluatePostInputStanzas(next: String, labels: Labels, seen: Seq[String])(implicit stanzaMap: Map[String, Stanza]): (Option[String], Labels) =
      if (seen.contains(next)) {(None, labels)}   // next indicates any seen id
      else {stanzaMap.get(next) match {
        case None => (Some(next), labels)
        case Some(s) => s match {
          case _: PageStanza => (Some(next), labels)
          case EndStanza => labels.nextFlow match {
              case Some((nxt, updatedLabels)) => evaluatePostInputStanzas(nxt, updatedLabels, seen)
              // Encountering an EndStanza following input suggests incomplete guidance, i.e. a form page which accepts input and stops
              // Therefore handle as if guidance indicated a Value error and cause current form page to be re-displayed. Logically also
              // an EndStanza indicates there is no Next!
              case None => (None, labels)
            }
          case s: Stanza with Evaluate =>
            val (next, updatedLabels) = s.eval(labels)
            evaluatePostInputStanzas(next, updatedLabels, seen)
        }
      }}



    implicit val stanzaMap: Map[String, Stanza] = page.keyedStanzas.map(ks => (ks.key, ks.stanza)).toMap ++ labels.continuationPool
    val (_, newLabels, seen, nextPageId, optionalInput) = evaluateStanzas(stanzaMap(page.id).next.head, labels)

    optionalInput.fold[(Option[String], Labels)]((Some(nextPageId), newLabels)){dataInputStanza =>
      dataInputStanza.eval(answer, page, newLabels) match {
        case (Some(Process.EndStanzaId), updatedLabels) => updatedLabels.nextFlow match {
            case Some((next, updatedLabels)) => evaluatePostInputStanzas(next, updatedLabels, seen)
            // Encountering an EndStanza following input suggests incomplete guidance, i.e. a form page which accepts input and stops
            // Therefore handle as if guidance indicated a Value error and cause current form page to be re-displayed. Logically also
            // an EndStanza indicates there is no Next!
            case None => (None, updatedLabels)
          }
        case (Some(next), updatedLabels) => evaluatePostInputStanzas(next, updatedLabels, seen)
        case (None, updatedLabels) => (None, updatedLabels)
       }
    }
  }

  @tailrec
   private def evaluateStanzas(stanzaId: String, labels: Labels, visualStanzas: Seq[VisualStanza] = Nil, seen: Seq[String] = Nil)
                              (implicit stanzaMap: Map[String, Stanza]): (Seq[VisualStanza], Labels, Seq[String], String, Option[DataInput]) =
    stanzaMap.get(stanzaId) match {
      case None => (visualStanzas, labels, seen, stanzaId, None)
      case Some(s) => s match {
        case EndStanza => (visualStanzas, labels, seen :+ stanzaId, stanzaId, None)
        case s: VisualStanza with DataInput => (visualStanzas :+ s, labels, seen :+ stanzaId, stanzaId, Some(s))
        case s: Stanza with Evaluate =>
          val (next, updatedLabels) = s.eval(labels)
          evaluateStanzas(next, updatedLabels, visualStanzas, seen :+ stanzaId)
        case s: VisualStanza => evaluateStanzas(s.next.head, labels, visualStanzas :+ s, seen :+ stanzaId)
      }
    }
}