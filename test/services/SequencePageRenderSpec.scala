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

import core.services._
import base.BaseSpec
import core.models.ocelot._
import mocks.MockAppConfig
import play.api.inject.Injector
import play.api.i18n.{Messages, MessagesApi}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite

class SequencePageRenderSpec extends BaseSpec with ProcessJson with GuiceOneAppPerSuite {

  // Define instance of class used in testing
  val pageBuilder = new PageBuilder(new Timescales(new DefaultTodayProvider))
  val renderer: PageRenderer = new PageRenderer(MockAppConfig)
  private def injector: Injector = app.injector
  val messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
  implicit val messages: Messages = messagesApi.preferred(Seq())
  implicit val ctx: UIContext = UIContext(LabelCache(), Map(), messages)

  trait FlowTest extends SequenceJson {
    val emptyLabels = LabelCache()
    def followNext(next: Option[String], l: Labels, process: Process, f: (Page, Labels) => Unit): Unit =
      next.map(nxt => pageBuilder.buildPage(nxt, process).fold(e => fail(e.toString), p => f(p,l)))
    def renderPagePostSubmit(p: Page, l: Labels, a: String): (Option[String], Labels) = {
      renderer.renderPagePostSubmit(p, l, a).fold(_ => fail, res => res)
    }
  }

  trait SimpleFlowTest extends FlowTest {
    val process = seqJson.as[Process]
  }

  trait InputFlowTest extends FlowTest {
    val process = seqInputJson.as[Process]
  }

  trait NestedFlowTest extends FlowTest {
    val process = nestedSeqJson.as[Process]
  }

  "Sequence stanza process" must {
    "build into pages which include all sub-flows" in new SimpleFlowTest {
      pageBuilder.pages(process) match {
        case Left(err) => fail(err.toString)
        case Right(pages) => pages.length shouldBe 6
      }
    }

    "Create a subflow for each selected flow from sequence" in new SimpleFlowTest {
      pageBuilder.pages(process) match {
        case Left(err) => fail(err.toString)
        case Right(pages) =>
          val (next, labels) = renderPagePostSubmit(pages.head, emptyLabels, "0, 2,3")

          next shouldBe Some("4")
          labels.value("Choice") shouldBe Some("First")
          labels.valueAsList("Choice_seq") shouldBe Some(List("First", "Third", "Fourth"))
          labels.flowStack.length shouldBe 4
      }
    }

    "Follow each chosen subflow and then return to main flow next, till end" in new InputFlowTest {

      pageBuilder.pages(process).fold(e => fail(e.toString),
        pages => {
          val (next, labels) = renderPagePostSubmit(pages.head, emptyLabels, "0, 3")

          next shouldBe Some("4")
          labels.valueAsList("Choice_seq") shouldBe Some(List("First", "Fourth"))
          labels.value("Choice") shouldBe Some("First")
          labels.value("YesNo") shouldBe None

          followNext(next, labels, process, (p,l) => {
            val (next, labels) = renderPagePostSubmit(p, l, "0")

            next shouldBe Some("88")
            labels.value("YesNo") shouldBe Some("Yes")
            labels.value("Choice") shouldBe Some("Fourth")

            followNext(next, labels, process, (p, l) => {
              val (next, labels) = renderPagePostSubmit(p, l, "hello")

              next shouldBe Some("2")
              labels.value("FlowInput") shouldBe Some("hello")

              followNext(next, labels, process, (p, l) => {p.next shouldBe Nil})
            })
          })
        }
      )
    }

    "Follow a single subflow containging no Pages" in new InputFlowTest {

      pageBuilder.pages(process).fold(e => fail(e.toString),
        pages => {
          val (next, labels) = renderPagePostSubmit(pages.head, emptyLabels, "1")

          next shouldBe Some("2")

          labels.value("SecondSeqChoice") shouldBe Some("Second")
        }
      )
    }

    "Follow each chosen subflow and nested unpaged subflow, till end" in new NestedFlowTest {

      pageBuilder.pages(process).fold(e => fail(e.toString),
        pages => {
          val (next, labels) = renderPagePostSubmit(pages.head, emptyLabels, "3")

          next shouldBe Some("88")
          labels.valueAsList("Choice_seq") shouldBe Some(List("Fourth"))
          labels.value("Choice") shouldBe Some("Fourth")

          followNext(next, labels, process, (p,l) => {
            val (next, labels) = renderPagePostSubmit(p, l, "1")

            next shouldBe Some("2")
            labels.value("NestedSeqComplete") shouldBe Some("Yes")

            labels.flowStack shouldBe Nil
          })
        }
      )
    }

    "Follow each chosen subflow and nested paged subflow, till end" in new NestedFlowTest {

      pageBuilder.pages(process).fold(e => fail(e.toString),
        pages => {
          val (next, labels) = renderPagePostSubmit(pages.head, emptyLabels, "3")

          next shouldBe Some("88")
          labels.valueAsList("Choice_seq") shouldBe Some(List("Fourth"))
          labels.value("Choice") shouldBe Some("Fourth")

          followNext(next, labels, process, (p,l) => {
            val (next, labels) = renderPagePostSubmit(p, l, "0")

            next shouldBe Some("12")

            followNext(next, labels, process, (p, l) => {
              val (next, labels) = renderPagePostSubmit(p, l, "0")

              next shouldBe Some("2")
              labels.value("NestedSeqComplete") shouldBe Some("Yes")

              labels.flowStack shouldBe Nil
            })

          })
        }
      )
    }

  }
}
