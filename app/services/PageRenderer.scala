/*
 * Copyright 2022 HM Revenue & Customs
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
import config.AppConfig
import core.models.RequestOutcome
import core.models.errors.NonTerminatingPageError
import core.models.ocelot.stanzas.{PageStanza, EndStanza, VisualStanza, Stanza, Evaluate, DataInput}
import core.models.ocelot.{Page, Labels, Process}

@Singleton
class PageRenderer @Inject() (appConfig: AppConfig) {

  def renderPage(page: Page, labels: Labels): RequestOutcome[(Seq[VisualStanza], Labels, Option[DataInput])] = {
    implicit val stanzaMap: Map[String, Stanza] = page.keyedStanzas.map(ks => (ks.key, ks.stanza)).toMap ++ labels.continuationPool
    evaluateStanzas(stanzaMap(page.id).next.head, labels) match {
      case Right((visualStanzas, newLabels, _, _, optionalInput)) => Right((visualStanzas, newLabels, optionalInput))
      case Left(err) => Left(err)
    }
  }

  def renderPagePostSubmit(page: Page, labels: Labels, answer: String): RequestOutcome[(Option[String], Labels)] = {

    @tailrec
    def evaluatePostInputStanzas(next: String, labels: Labels, seen: Seq[String], stanzaCount: Int = 0)
                                (implicit stanzaMap: Map[String, Stanza]): RequestOutcome[(Option[String], Labels)] =
      if (seen.contains(next)) {Right((None, labels))}   // next indicates any seen id
      else {stanzaMap.get(next) match {
        case None => Right((Some(next), labels))
        // Limit stanzas within page to catch non-terminating loops in guidance
        case Some(s) if stanzaCount < appConfig.pageStanzaLimit => s match { // Limit stanzas within page to catch non-terminating loops in guidance
          case _: PageStanza => Right((Some(next), labels))
          case EndStanza => labels.nextFlow match {
              case Some((nxt, updatedLabels)) => evaluatePostInputStanzas(nxt, updatedLabels, seen, stanzaCount+1)
              // Encountering an EndStanza following input suggests incomplete guidance, i.e. a form page which accepts input and stops
              // Therefore handle as if guidance indicated a Value error and cause current form page to be re-displayed. Logically also
              // an EndStanza indicates there is no Next!
              case None => Right((None, labels))
            }
          case s: Stanza with Evaluate =>
            val (next, updatedLabels) = s.eval(labels)
            evaluatePostInputStanzas(next, updatedLabels, seen, stanzaCount+1)
        }
        case Some(s) => Left(NonTerminatingPageError)
      }}

    implicit val stanzaMap: Map[String, Stanza] = page.keyedStanzas.map(ks => (ks.key, ks.stanza)).toMap ++ labels.continuationPool
    evaluateStanzas(stanzaMap(page.id).next.head, labels) match {
      case Right((_, newLabels, seen, nextPageId, optionalInput)) =>
        optionalInput.fold[RequestOutcome[(Option[String], Labels)]](Right((Some(nextPageId), newLabels))){dataInputStanza =>
          dataInputStanza.eval(answer, page, newLabels) match {
            case (Some(Process.EndStanzaId), updatedLabels) => updatedLabels.nextFlow match {
                case Some((next, updatedLabels)) => evaluatePostInputStanzas(next, updatedLabels, seen)
                // Encountering an EndStanza following input suggests incomplete guidance, i.e. a form page which accepts input and stops
                // Therefore handle as if guidance indicated a Value error and cause current form page to be re-displayed. Logically also
                // an EndStanza indicates there is no Next!
                case None => Right((None, updatedLabels))
            }
            case (Some(next), updatedLabels) => evaluatePostInputStanzas(next, updatedLabels, seen)
            case (None, updatedLabels) => Right((None, updatedLabels))
           }
        }
      case Left(err) => Left(err)
    }
  }

  @tailrec
   private def evaluateStanzas(stanzaId: String, labels: Labels, visualStanzas: Seq[VisualStanza] = Nil, seen: Seq[String] = Nil, stanzaCount: Int = 0)
                              (implicit stanzaMap: Map[String, Stanza]): RequestOutcome[(Seq[VisualStanza], Labels, Seq[String], String, Option[DataInput])] =
    stanzaMap.get(stanzaId) match {
      case None => Right((visualStanzas, labels, seen, stanzaId, None))
      case Some(s) if stanzaCount < appConfig.pageStanzaLimit  => s match { // Limit stanzas within page to catch non-terminating loops in guidance
        case EndStanza => Right((visualStanzas, labels, seen :+ stanzaId, stanzaId, None))
        case s: VisualStanza with DataInput => Right((visualStanzas :+ s, labels, seen :+ stanzaId, stanzaId, Some(s)))
        case s: Stanza with Evaluate =>
          val (next, updatedLabels) = s.eval(labels)
          evaluateStanzas(next, updatedLabels, visualStanzas, seen :+ stanzaId, stanzaCount+1)
        case s: VisualStanza => evaluateStanzas(s.next.head, labels, visualStanzas :+ s, seen :+ stanzaId, stanzaCount+1)
      }
      case Some(s) => Left(NonTerminatingPageError)
    }
}