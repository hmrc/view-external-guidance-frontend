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
import core.models.RequestOutcome
import core.models.errors.NotFoundError
import core.models.ocelot.{Page, Process}
import models.admin._
import play.api.Logger
import play.api.i18n.I18nSupport
import play.api.mvc._
import services.{DebugService, RetrieveAndCacheService}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import views.html._
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StartAdminController @Inject() (
    errorHandler: ErrorHandler,
    service: RetrieveAndCacheService,
    debugService: DebugService,
    view: admin.process_structure,
    mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext) extends FrontendController(mcc)
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
        val processPageMaps: List[ProcessPageStructure] = pages.map(debugService.mapPage(_, pages.map(p => (p.id, p)).toMap)).toList
        Ok(view(process.title.english, processPageMaps))
      case Left(NotFoundError) => NotFound(errorHandler.notFoundTemplate)
      case Left(err) => InternalServerError(errorHandler.internalServerErrorTemplate)
    }

}
