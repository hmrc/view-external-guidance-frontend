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
import models.admin._
import core.models.RequestOutcome
import views.html._
import core.models.errors.NotFoundError
import core.models.ocelot.{Process, Page}
import core.models.ocelot.stanzas.{TitleCallout, Input, YourCallCallout, Question, Sequence}
import scala.concurrent.Future
import scala.annotation.tailrec

@Singleton
class StartAdminController @Inject() (
    errorHandler: ErrorHandler,
    service: RetrieveAndCacheService,
    view: admin.process_map,
    sessionIdAction: SessionIdAction,
    mcc: MessagesControllerComponents,
    appConfig: AppConfig
) extends FrontendController(mcc)
    with I18nSupport {

  val logger: Logger = Logger(getClass)

  def publishedPageMap(processId: String): Action[AnyContent] = Action.async { implicit request =>
    retrieveAndView(processId, service.retrieveOnlyPublished)
  }

  def approvalPageMap(processId: String): Action[AnyContent] = Action.async { implicit request =>
   retrieveAndView(processId, service.retrieveOnlyApproval)
  }

  private[entry] def retrieveAndView(processCode: String,
                                     retrieve: String => Future[RequestOutcome[(Process, Seq[Page])]])(implicit request: Request[_]): Future[Result] =
    retrieve(processCode).map{
      case Right((process, Nil)) => Ok(view(process.title.english, Nil))
      case Right((process, pages)) =>
        val pageMap: Map[String, Page] = pages.map(p => (p.id, p)).toMap
        val pageRows: List[ProcessMapPage] = buildPageRows(Seq(Process.StartStanzaId), pageMap)
        Ok(view(process.title.english, pageRows))

      case Left(NotFoundError) => NotFound(errorHandler.notFoundTemplate)

      case Left(err) => InternalServerError(errorHandler.internalServerErrorTemplate)
     }

  @tailrec
  final private[entry] def buildPageRows(pageIds: Seq[String],
                                         pageMap: Map[String, Page],
                                         seen: List[String] = Nil,
                                         rows: List[ProcessMapPage] = Nil): List[ProcessMapPage] =
    pageIds match {
      case Nil => rows.reverse
      case x :: xs if !seen.contains(x) =>
        val page = pageMap(x)
        val nexts = page.next.map(n => LinkedPage(n, pageMap(n).url, pageTitle(pageMap(n))))
        val linked = page.linked.map(l => LinkedPage(l, pageMap(l).url, pageTitle(pageMap(l))))
        val linkedFrom = pageMap.values.filter(p => p.linked.contains(x) || p.next.contains(x)).map(_.id).toSeq
        buildPageRows(
          xs ++ page.next ++ page.linked,
          pageMap,
          x :: seen,
          ProcessMapPage(x, page.url, pageTitle(page), page.keyedStanzas, nexts, linked, linkedFrom) :: rows
        )
      case x :: xs =>
        buildPageRows(xs, pageMap, seen, rows)
    }

  private[entry] def pageTitle(page: Page): Option[String] =
    page.stanzas.collectFirst{
      case i: Input => i.name.english
      case i: Question => i.text.english
      case i: Sequence => i.text.english
      case c: TitleCallout => c.text.english
      case yc: YourCallCallout => yc.text.english
    }

}
