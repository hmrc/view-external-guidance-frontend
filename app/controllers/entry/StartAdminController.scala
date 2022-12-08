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
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.actions.SessionIdAction
import models._
import core.models.RequestOutcome
import views.html.process_map
import core.models.ocelot.{Process, Page}
import core.models.ocelot.stanzas.{TitleCallout, Input, YourCallCallout, Question, Sequence}
import scala.concurrent.Future

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
    //println(p.stanzas)
    ProcessMapRow(PageEntry, p.id, p.url, pageTitle(p)) +: (p.next.map{ id =>
      ProcessMapRow(NextEntry, id, pageMap(id).url, pageTitle(pageMap(id)))
    } ++ p.linked.map{ id =>
      ProcessMapRow(LinkEntry, id, pageMap(id).url, pageTitle(pageMap(id)))
    })
  }

  def publishedPageMap(processId: String): Action[AnyContent] = Action.async { implicit request =>
    retrieveAndView(processId, service.retrieveOnlyPublished)
  }

  def approvalPageMap(processId: String): Action[AnyContent] = Action.async { implicit request =>
   retrieveAndView(processId, service.retrieveOnlyApproval)
  }

  private def retrieveAndView(processCode: String, retrieve: String => Future[RequestOutcome[(Process, Seq[Page])]])(implicit request: Request[_]): Future[Result] =
    retrieve(processCode).map{
      case Right((process, pages)) =>
        val pageMap: Map[String, Page] = pages.map(p => (p.id, p)).toMap
        val pageRows: List[Seq[ProcessMapRow]] = buildPageRows(pageMap(Process.StartStanzaId), pageMap) ::
          (pageMap.keys.filterNot(_.equals(Process.StartStanzaId)).toList.map{ id => buildPageRows(pageMap(id), pageMap)})
        Ok(view(process.title.english, pageRows))

      case Left(err) =>
        InternalServerError(errorHandler.internalServerErrorTemplate)
     }

  private def pageTitle(page: Page): Option[String] =
    page.stanzas.collectFirst{
      case i: Input => i.name.english
      case i: Question => i.text.english
      case i: Sequence => i.text.english
      case c: TitleCallout => c.text.english
      case yc: YourCallCallout => yc.text.english
    }

}
