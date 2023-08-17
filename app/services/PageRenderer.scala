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

import config.AppConfig
import core.models.RequestOutcome
import core.models.ocelot.errors.{NonTerminatingPageError, ProgrammingError, RuntimeError}
import core.models.ocelot.stanzas._
import core.models.ocelot.{Labels, Page, PageReview, Process}
import models.errors._
import play.api.Logger
import play.api.i18n.Messages

import javax.inject.{Inject, Singleton}
import scala.annotation.tailrec

@Singleton
class PageRenderer @Inject() (appConfig: AppConfig) {
  val logger: Logger = Logger(getClass)

  def renderPage(page: Page, labels: Labels)(implicit messages: Messages): RequestOutcome[(List[VisualStanza], Labels, Option[DataInput])] = {
    implicit val stanzaMap: Map[String, Stanza] = page.keyedStanzas.map(ks => (ks.key, ks.stanza)).toMap ++ labels.continuationPool
    evaluateStanzas(stanzaMap(page.id).next.head, labels) match {
      case Right((visualStanzas, newLabels, _, _, optionalInput)) => Right((visualStanzas.toList, newLabels, optionalInput))
      case Left(err) => Left(err)
    }
  }

  def renderPagePostSubmit(page: Page, labels: Labels, answer: String)(implicit messages: Messages): RequestOutcome[(Option[String], Labels)] = {

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
              case Some((nxt, updatedLabels)) => evaluatePostInputStanzas(nxt, updatedLabels, seen, stanzaCount + 1)
              // Encountering an EndStanza following input suggests incomplete guidance, i.e. a form page which accepts input and stops
              // Therefore handle as if guidance indicated a Value error and cause current form page to be re-displayed. Logically also
              // an EndStanza indicates there is no Next!
              case None => Right((None, labels))
            }
          case s: Stanza with Evaluate =>
            evalStanza(s,labels) match {
              case (nxt, updatedLabels, Nil) => evaluatePostInputStanzas(nxt, updatedLabels, seen, stanzaCount + 1)
              case (_, _, errs) => Left(executionError(errs, next, labels.runMode))
            }
          case _ => Left(executionError(ProgrammingError("Visual stanzas found after input"), next, labels.runMode))
        }
        case Some(s) => Left(executionError(NonTerminatingPageError, next, labels.runMode))
      }}

    implicit val stanzaMap: Map[String, Stanza] = page.keyedStanzas.map(ks => (ks.key, ks.stanza)).toMap ++ labels.continuationPool
    evaluateStanzas(stanzaMap(page.id).next.head, labels) match {
      case Right((_, newLabels, seen, nextPageId, optionalInput)) =>
        optionalInput.fold[RequestOutcome[(Option[String], Labels)]](Right((Some(nextPageId), newLabels))){dataInputStanza =>
          dataInputStanza.eval(answer, page, newLabels) match {
            case (Some(Process.EndStanzaId), updatedLabels) => updatedLabels.nextFlow match {
                case Some((nxt, updatedLabels)) => evaluatePostInputStanzas(nxt, updatedLabels, seen)
                // Encountering an EndStanza following input suggests incomplete guidance, i.e. a form page which accepts input and stops
                // Therefore handle as if guidance indicated a Value error and cause current form page to be re-displayed. Logically also
                // an EndStanza indicates there is no Next!
                case None => Right((None, updatedLabels))
            }
            case (Some(nxt), updatedLabels) => evaluatePostInputStanzas(nxt, updatedLabels, seen)
            case (None, updatedLabels) => Right((None, updatedLabels))
           }
        }
      case Left(err) => Left(err)
    }
  }

  private def evalStanza(s: Evaluate, labels: Labels):(String, Labels, List[RuntimeError]) =
    (s.eval(labels), labels.runMode) match {
      // Ignore evaluation errors when running in PageReview run mode
      case ((nxt, updatedLabels, errs), PageReview) => (nxt, updatedLabels, Nil)
      case ((nxt, updatedLabels, errs), _) => (nxt, updatedLabels, errs)
    }

  @tailrec
   private def evaluateStanzas(stanzaId: String, labels: Labels, visualStanzas: Seq[VisualStanza] = Nil, seen: Seq[String] = Nil, stanzaCount: Int = 0)
                              (implicit stanzaMap: Map[String, Stanza], messages: Messages): RequestOutcome[(Seq[VisualStanza], Labels, Seq[String], String, Option[DataInput])] =
    stanzaMap.get(stanzaId) match {
      case None => Right((visualStanzas, labels, seen, stanzaId, None))
      case Some(s) if stanzaCount < appConfig.pageStanzaLimit  => s match { // Limit stanzas within page to catch non-terminating loops in guidance
        case EndStanza => Right((visualStanzas, labels, seen :+ stanzaId, stanzaId, None))
        case s: DataInputStanza =>
          val es: DataInputStanza = s.rendered(TextBuilder.expandLabels(labels))
          Right((visualStanzas :+ es, labels, seen :+ stanzaId, stanzaId, Some(es)))
        case s: Stanza with Evaluate =>
          evalStanza(s,labels) match {
            case (nxt, updatedLabels, Nil) => evaluateStanzas(nxt, updatedLabels, visualStanzas, seen :+ stanzaId, stanzaCount + 1)
            case (_, _, errs) => Left(executionError(errs, stanzaId, labels.runMode))
          }
        case s: VisualStanza => evaluateStanzas(s.next.head, labels, visualStanzas :+ s.rendered(TextBuilder.expandLabels(labels)), seen :+ stanzaId, stanzaCount + 1)
        case _ => Left(executionError(ProgrammingError("Unknown stanza without Evaluate"), stanzaId, labels.runMode))
      }
      case Some(s) => Left(executionError(NonTerminatingPageError, stanzaId, labels.runMode))
    }
}