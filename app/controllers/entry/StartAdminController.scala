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

package controllers.entry

import config.ErrorHandler
import javax.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc._
import services.RetrieveAndCacheService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import play.api.Logger
import scala.concurrent.ExecutionContext.Implicits.global
import models.admin._
import core.models.RequestOutcome
import views.html._
import core.models.errors.NotFoundError
import core.models.ocelot.{Process, Page, SecuredProcess}
import core.models.ocelot.stanzas.{TitleCallout, Input, YourCallCallout, Question, Sequence}
import scala.concurrent.Future

@Singleton
class StartAdminController @Inject() (
    errorHandler: ErrorHandler,
    service: RetrieveAndCacheService,
    view: admin.process_map,
    mcc: MessagesControllerComponents
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
      case Right((process, pages)) => Ok(view(process.title.english, toProcessMapPages(pages, pages.map(p => (p.id, p)).toMap)))
      case Left(NotFoundError) => NotFound(errorHandler.notFoundTemplate)
      case Left(err) => InternalServerError(errorHandler.internalServerErrorTemplate)
     }

  private[entry] def toProcessMapPages(pages: Seq[Page], pageMap: Map[String, Page]): List[ProcessMapPage] =
    pages.map{page =>
      val nexts = page.next.distinct.map(n => LinkedPage(n, pageMap(n).url, pageTitle(pageMap(n))))
      val linked = page.linked.distinct.map(l => LinkedPage(l, pageMap(l).url, pageTitle(pageMap(l))))
      val linkedFrom = pageMap.values
                              .filter(p => p.linked.contains(page.id) || p.next.contains(page.id))
                              .map(_.id)
                              .filterNot(_.equals(SecuredProcess.PassPhrasePageId)).toSeq
      ProcessMapPage(page.id, page.url, pageTitle(page), page.keyedStanzas, nexts, linked, linkedFrom)
    }.toList

  private[entry] def pageTitle(page: Page): Option[String] =
    page.stanzas.collectFirst{
      case i: Input => i.name.english
      case i: Question => i.text.english
      case i: Sequence => i.text.english
      case c: TitleCallout => c.text.english
      case yc: YourCallCallout => yc.text.english
    }

}
