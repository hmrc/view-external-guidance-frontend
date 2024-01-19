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

package controllers.apis

import base.BaseSpec
import java.time.Instant
import mocks.{MockProcessCacheRepository, MockAppConfig}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import models.admin.CachedProcessSummary
import scala.concurrent.Future
import play.api.http.Status
import repositories.{CacheKey, CachedProcess}
import core.models.ocelot._
import models.PageNext


class AdminApiControllerSpec extends BaseSpec with SequenceJson with GuiceOneAppPerSuite {

  trait TestData {
    val summaries = List(
      CachedProcessSummary("id1", 123456L, Some(123456L), None, "Title", Instant.now),
      CachedProcessSummary("id2", 123456L, Some(123456L), None, "Other Title", Instant.now),
    )

    val process = seqJson.as[Process]

    val cachedProcess = CachedProcess(
      CacheKey("id1", 123456L, Some(123456L), None),
      process,
      Map("/start" -> PageNext("id1")),
      Instant.now
    )
  }

  trait ApiTest extends MockProcessCacheRepository with TestData {
    lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")

    lazy val target = new AdminApiController(
        MockAppConfig,
        mockProcessCacheRepository,
        stubMessagesControllerComponents()
      )
  }

  "Calling the list endpoint" should {
    "return a list of CachedProcessSummary" in new ApiTest {
      MockProcessCacheRepository.listSummaries().returns(Future.successful(Right(summaries)))
      val result = target.listActiveProcessSummaries()(fakeRequest)
      status(result) shouldBe Status.OK
      contentAsJson(result).as[List[CachedProcessSummary]] match {
        case x :: y :: xs if x == summaries(0) && y == summaries(1) => succeed
        case x => fail()
      }
    }
  }

  "Calling the list endpoint" should {
    "Return ISE on database error" in new ApiTest {
      MockProcessCacheRepository.listSummaries().returns(Future.successful(Left(core.models.errors.DatabaseError)))
      val result = target.listActiveProcessSummaries()(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }
  }

  "Calling the get endpoint" should {
    "return the identified json" in new ApiTest {
      MockProcessCacheRepository.get("id1", 123456L, Some(123456L), None).returns(Future.successful(Right(cachedProcess)))
      val result = target.getActive("id1", 123456L, Some(123456L), None)(fakeRequest)
      status(result) shouldBe Status.OK
      contentAsJson(result).as[Process] shouldBe process
    }
  }

  "Calling the get endpoint" should {
    "Return ISE on database error" in new ApiTest {
      MockProcessCacheRepository.get("id1", 123456L, Some(123456L), None).returns(Future.successful(Left(core.models.errors.DatabaseError)))
      val result = target.getActive("id1", 123456L, Some(123456L), None)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }
  }

  "Calling the get endpoint" should {
    "Return BadRequest on unknown process" in new ApiTest {
      MockProcessCacheRepository.get("id1", 123456L, Some(123456L), None).returns(Future.successful(Left(core.models.errors.CachedProcessNotFoundError)))
      val result = target.getActive("id1", 123456L, Some(123456L), None)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }
  }

}
