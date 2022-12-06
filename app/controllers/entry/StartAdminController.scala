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

package controllers.entry

import config.{AppConfig, ErrorHandler}
import javax.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc._
import services.RetrieveAndCacheService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import play.api.Logger
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.actions.SessionIdAction

import views.html.process_map
import core.models.ocelot.{Process, Page}
import core.models.ocelot.stanzas.{TitleCallout, Input, YourCallCallout}

sealed trait ProcessRowEntryType

case object PageEntry extends ProcessRowEntryType {
  override def toString(): String = "Page"
}

case object LinkEntry extends ProcessRowEntryType {
  override def toString(): String = "=>"
}

case class ProcessMapRow(typ: ProcessRowEntryType, id: String, url: String, title: Option[String])


@Singleton
class StartAdminController @Inject() (
    errorHandler: ErrorHandler,
    service: RetrieveAndCacheService,
    view: process_map,
    sessionIdAction: SessionIdAction,
    mcc: MessagesControllerComponents,
    appConfig: AppConfig
) extends FrontendController(mcc)
    with I18nSupport {

  val logger: Logger = Logger(getClass)

  private def buildPageRows(p: Page, pageMap: Map[String, Page]): Seq[ProcessMapRow] = {
    ProcessMapRow(PageEntry, p.id, p.url, pageTitle(p)) +: p.linked.map{ id =>
      ProcessMapRow(LinkEntry, id, pageMap(id).url, pageTitle(pageMap(id)))
    }
  }

  def publishedPageMap(processId: String): Action[AnyContent] = Action.async { implicit request =>
    logger.info(s"Starting published pageMap")
    service.retrieveOnlyApproval(processId).map{
      case Right(pages) =>
        val pageMap: Map[String, Page] = pages.map(p => (p.id, p)).toMap
        val pageRows: Seq[ProcessMapRow] = buildPageRows(pageMap(Process.StartStanzaId), pageMap) ++
          (pageMap.keys.filterNot(_.equals(Process.StartStanzaId)).toSeq.flatMap{ id =>
            buildPageRows(pageMap(id), pageMap)
          })

        pageRows.foreach(println)
        // val startPageRows = pageMap(Process.StartStanzaId).stanzas
        // // val urlMap: Map[String, ProcessMapRow] = pages.map{ p =>
        //   (p.id, ProcessMapRow(PageEntry, p.id, p.url, pageTitle(p)))
        // }.toMap
        Ok(view())

      case Left(err) =>
        InternalServerError(errorHandler.internalServerErrorTemplate)
     }
  }

  def approvalPageMap(processId: String): Action[AnyContent] = Action.async { implicit request =>
    logger.info(s"Starting approval pageMap")
    service.retrieveOnlyApproval(processId).map{
      case Right(pages) =>
        Ok(view())
      case Left(err) =>
        InternalServerError(errorHandler.internalServerErrorTemplate)
     }
  }

  private def pageTitle(page: Page): Option[String] =
    page.stanzas.collectFirst{
      case c: TitleCallout => c.text.english
      case i: Input => i.name.english
      case yc: YourCallCallout => yc.text.english
    }

}
