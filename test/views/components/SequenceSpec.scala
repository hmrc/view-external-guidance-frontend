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

package views.components

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.data.Form
import play.api.inject.Injector
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.test.FakeRequest
import views.html._
import forms.SubmittedListAnswerFormProvider
import models.PageContext
import models.ui.{FormPage, H2, Paragraph, RequiredErrorMsg, Sequence, SequenceAnswer, SubmittedListAnswer, Text}
import core.models.ocelot.{LabelCache, Labels}
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements
import base.{ViewFns, ViewSpec}

import scala.collection.JavaConverters._

class SequenceSpec extends AnyWordSpec with Matchers with ViewSpec with ViewFns with GuiceOneAppPerSuite {

  trait Test {
    private def injector: Injector = app.injector

    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    def formProvider: SubmittedListAnswerFormProvider = injector.instanceOf[SubmittedListAnswerFormProvider]

    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))

    val fakeRequest = FakeRequest("GET", "/")

    implicit val labels: Labels = LabelCache()

    val path: String = "path"

    // Create test sequence data
    val sequenceTitle: Text = Text("Select your favourite sweets")
    val sequenceHint: Text = Text("Consider both chocolate and sugar style sweets")
    val sweetOptions = Seq(
      SequenceAnswer(Text("Wine gums"), None),
      SequenceAnswer(Text("Munchies"), None)
    )
    val sweetOptionsOneOptionHint = Seq(
      SequenceAnswer(Text("Wine gums"), None),
      SequenceAnswer(Text("Munchies"), Some(Text("Munchies are chocolatey")))
    )
    val errorMsg: RequiredErrorMsg = RequiredErrorMsg(Text("An input error has occurred"))

    val exclusiveHolidayOption: Text = Text("Elsewhere")
    val exclusiveHint: Text = Text("Selecting this option will deselect all the other checkboxes")
    val exclusiveSequenceTitle: Text = Text("Where are you going on your holidays?")
    val holidayOptions = Seq(
      SequenceAnswer(Text("The UK"), None),
      SequenceAnswer(Text("Europe"), None)
    )
    val exclusiveHolidayAnswer: SequenceAnswer = SequenceAnswer(exclusiveHolidayOption, Some(exclusiveHint))

    val h2: H2 = H2(Text("Subtitle"))
    val p: Paragraph = Paragraph(Text("Introduction to sweets"))

    val sequenceWithoutHint: Sequence = Sequence(
      sequenceTitle,
      None,
      sweetOptions,
      None,
      Seq.empty,
      Seq.empty
    )

    val sequenceWithoutHintButOptionWithHint: Sequence = Sequence(
      sequenceTitle,
      None,
      sweetOptionsOneOptionHint,
      None,
      Seq.empty,
      Seq.empty
    )

    val sequenceWithHint: Sequence = Sequence(
      sequenceTitle,
      Some(sequenceHint),
      sweetOptions,
      None,
      Seq.empty,
      Seq.empty
    )

    val sequenceWithBody: Sequence = Sequence(
      sequenceTitle,
      None,
      sweetOptions,
      None,
      Seq(h2, p),
      Seq.empty
    )

    val sequenceWithError: Sequence = Sequence(
      sequenceTitle,
      None,
      sweetOptions,
      None,
      Seq.empty,
      Seq(errorMsg)
    )

    val exclusiveSequence: Sequence = Sequence(
      exclusiveSequenceTitle,
      None,
      holidayOptions,
      Some(exclusiveHolidayAnswer),
      Seq(h2, p),
      Seq.empty
    )

    val pageWithoutHint: FormPage = FormPage("/start", sequenceWithoutHint)
    val pageWithHint: FormPage = FormPage("/start", sequenceWithHint)
    val pageWithBody: FormPage = FormPage("/start", sequenceWithBody)
    val pageWithError: FormPage = FormPage("/start", sequenceWithError)
    val pageWithExclusiveSequence: FormPage = FormPage("/start", exclusiveSequence)

    val pageWithoutHintCtx: PageContext = PageContext(
      pageWithoutHint,
      Seq.empty,
      None,
      "sessionId",
      None,
      Text(),
      "processId",
      "processCode",
      labels
    )

    val pageWithHintCtx: PageContext = PageContext(
      pageWithHint,
      Seq.empty,
      None,
      "sessionId",
      None,
      Text(),
      "processId",
      "processCode",
      labels
    )

    val pageWithBodyCtx: PageContext = PageContext(
      pageWithBody,
      Seq.empty,
      None,
      "sessionId",
      None,
      Text(),
      "processId",
      "processCode",
      labels
    )

    val pageWithErrorCtx: PageContext = PageContext(
      pageWithError,
      Seq.empty,
      None,
      "sessionId",
      None,
      Text(),
      "processId",
      "processCode",
      labels
    )

    val pageWithExclusiveSequenceCtx: PageContext = PageContext(
    pageWithExclusiveSequence,
    Seq.empty,
    None,
    "sessionId",
    None,
    Text(),
    "processId",
    "processCode",
    labels
    )
  }

  "Sequence component" must {

    "render sequence title for sequence without body inside legend" in new Test {

      val doc: Document = asDocument(
        components.sequence(sequenceWithoutHint, path, formProvider(path))
        (fakeRequest, messages, pageWithoutHintCtx)
      )

      val legends: Elements = doc.getElementsByTag("legend")

      legends.size shouldBe 1

      val headings: Elements = legends.first.getElementsByTag("h1")

      headings.size shouldBe 1

      headings.first.text() shouldBe sequenceTitle.asString

      elementAttrs(headings.first)("class") shouldBe "govuk-fieldset__heading"
    }

    "render sequence hint for sequence when present" in new Test {

      val doc: Document = asDocument(
        components.sequence(sequenceWithHint, path, formProvider(path))
        (fakeRequest, messages, pageWithHintCtx)
      )

      val hint: Option[Element] = getElementById(doc, "sequence-hint")

      hint match {
        case Some(elem) =>
          elem.text() shouldBe sequenceHint.asString
          elementAttrs(elem)("class") shouldBe "govuk-hint"
        case None => fail("Sequence should contain hint text")
      }

    }

    "sequence with hint should include hint id in aria-describedBy on field set" in new Test {

      val doc: Document = asDocument(
        components.sequence(sequenceWithHint, path, formProvider(path))
        (fakeRequest, messages, pageWithHintCtx)
      )

      val fieldSets: Elements = doc.getElementsByTag("fieldset")

      fieldSets.size shouldBe 1

      elementAttrs(fieldSets.first)("aria-describedby") shouldBe "sequence-hint"
    }

    "render title for sequence with body outside legend" in new Test {

      val doc: Document = asDocument(
        components.sequence(sequenceWithBody, path, formProvider(path))
        (fakeRequest, messages, pageWithBodyCtx)
      )

      val legends: Elements = doc.getElementsByTag("legend")

      legends.size shouldBe 1

      val headingsInsideLegend: Elements = legends.first.getElementsByTag("h1")

      headingsInsideLegend.size shouldBe 0

      val headings: Elements = doc.getElementsByTag("h1")

      headings.size shouldBe 1

      headings.first.text shouldBe sequenceTitle.asString

      elementAttrs(headings.first)("class") shouldBe "govuk-heading-xl"

      val h2Elements: Elements = doc.getElementsByTag("h2")

      h2Elements.size shouldBe 1

      h2Elements.first.text() shouldBe h2.text.asString
      elementAttrs(h2Elements.first)("class") shouldBe "govuk-heading-l"

      val paragraphElements: Elements = doc.getElementsByTag("p")

      paragraphElements.size shouldBe 1

      paragraphElements.first.text() shouldBe p.text.asString
      elementAttrs(paragraphElements.first)("class") shouldBe "govuk-body"
    }

    "render a checkbox for each sequence option" in new Test {

      val doc: Document = asDocument(
        components.sequence(sequenceWithoutHintButOptionWithHint, path, formProvider(path))
        (fakeRequest, messages, pageWithoutHintCtx)
      )

      val checkboxContainerDiv: Element = getSingleElementByClass(doc, "govuk-checkboxes")

      val checkboxDivs: Elements = checkboxContainerDiv.getElementsByClass("govuk-checkboxes__item")

      checkboxDivs.size shouldBe 2

      // Check constant attributes on first checkbox
      val checkbox1Inputs: Elements = checkboxDivs.first.getElementsByTag("input")

      checkbox1Inputs.size shouldBe 1

      val checkbox1InputAttrs: Map[String, String] = elementAttrs(checkbox1Inputs.first)

      val checkbox1Labels: Elements = checkboxDivs.first.getElementsByTag("label")

      checkbox1Labels.size shouldBe 1

      val checkbox1LabelAttrs: Map[String, String] = elementAttrs(checkbox1Labels.first)

      checkbox1InputAttrs("class") shouldBe "govuk-checkboxes__input"
      checkbox1InputAttrs("name") shouldBe "path[0]"

      checkbox1LabelAttrs("class") shouldBe "govuk-label govuk-checkboxes__label"

      // Check variable attributes on all checkboxes
      val checkbox2Inputs: Elements = checkboxDivs.last.getElementsByTag("input")

      checkbox2Inputs.size shouldBe 1

      val checkbox2InputAttrs: Map[String, String] = elementAttrs(checkbox2Inputs.first)

      checkbox2InputAttrs("class") shouldBe "govuk-checkboxes__input"
      checkbox2InputAttrs("name") shouldBe "path[1]"

      val checkbox2Labels: Elements = checkboxDivs.last.getElementsByTag("label")

      checkbox2Labels.size shouldBe 1

      val checkbox2Hints: Elements = checkboxDivs.last.getElementsByClass("govuk-checkboxes__hint")

      checkbox2Hints.size shouldBe 1

      val checkbox2LabelAttrs: Map[String, String] = elementAttrs(checkbox2Labels.first)

      checkbox1InputAttrs("id") shouldBe "path-0"
      checkbox1InputAttrs("name") shouldBe "path[0]"
      checkbox1InputAttrs("value") shouldBe "0"
      checkbox1LabelAttrs("for") shouldBe "path-0"
      checkbox1Labels.first.text() shouldBe sweetOptions.head.text.asString

      checkbox2InputAttrs("id") shouldBe "path-1"
      checkbox2InputAttrs("name") shouldBe "path[1]"
      checkbox2InputAttrs("value") shouldBe "1"
      checkbox2LabelAttrs("for") shouldBe "path-1"
      checkbox2Labels.first.text shouldBe sweetOptions.last.text.asString
    }

    "not render a paragraph element inside the field set of a non-exclusive sequence" in new Test {

      val doc: Document = asDocument(
        components.sequence(sequenceWithoutHint, path, formProvider(path))
      (fakeRequest, messages, pageWithoutHintCtx)
      )

      val fieldSet: Element = getSingleElementByClass(doc, "govuk-fieldset")

      val paragraphs: Elements = fieldSet.getElementsByTag("p")

      paragraphs.size shouldBe 0
    }

    "render checkboxes as 'checked' when they have been selected" in new Test {

      val form: Form[SubmittedListAnswer] = formProvider(path)

      val populatedForm: Form[SubmittedListAnswer] = form.fill(SubmittedListAnswer(List("0", "1")))

      val doc: Document = asDocument(
        components.sequence(sequenceWithoutHint, path, populatedForm)
        (fakeRequest, messages, pageWithoutHintCtx)
      )

      val checkboxContainerDiv: Element = getSingleElementByClass(doc, "govuk-checkboxes")

      val checkboxDivs: Elements = checkboxContainerDiv.getElementsByClass("govuk-checkboxes__item")

      checkboxDivs.size shouldBe 2

      // Check 'checked' attribute on first checkbox
      val checkbox1Inputs: Elements = checkboxDivs.first.getElementsByTag("input")

      checkbox1Inputs.size shouldBe 1

      val checkbox1InputAttrs: Map[String, String] = elementAttrs(checkbox1Inputs.first)

      checkbox1InputAttrs.contains("checked") shouldBe true

      // Check 'checked' attribute on second checkbox
      val checkbox2Inputs: Elements = checkboxDivs.last.getElementsByTag("input")

      checkbox2Inputs.size shouldBe 1

      val checkbox2InputAttrs: Map[String, String] = elementAttrs(checkbox2Inputs.first)

      checkbox2InputAttrs.contains("checked") shouldBe true

    }

    "render an error message if an input error occurs" in new Test {

      val doc: Document = asDocument(
        components.sequence(sequenceWithError, path, formProvider(path))
        (fakeRequest, messages, pageWithErrorCtx)
      )

      val formGroupDiv: Elements = doc.getElementsByClass("govuk-form-group")

      formGroupDiv.size shouldBe 1

      elementAttrs(formGroupDiv.first)("class") shouldBe "govuk-form-group govuk-form-group--error"

      // Check aria described by on field set
      val fieldSets: Elements = doc.getElementsByTag("fieldset")

      fieldSets.size shouldBe 1

      elementAttrs(fieldSets.first)("aria-describedby") shouldBe "required-error"

      // Check error message above checkboxes is present
      val outerErrorMsgSpan: Option[Element] = getElementById(doc, "required-error")

      outerErrorMsgSpan match {
        case Some(elem) =>
          val innerErrorMsgs: Elements = elem.getElementsByTag("p")
          innerErrorMsgs.size shouldBe 1
          innerErrorMsgs.first.text() shouldBe s"Error: ${errorMsg.text.asString}"
        case None => fail("Error message is missing")
      }
    }

    "render checkboxes with a dividing paragraph element for an exclusive sequence" in new Test {

      val doc: Document = asDocument(
        components.sequence(exclusiveSequence, path, formProvider(path))
        (fakeRequest, messages, pageWithExclusiveSequenceCtx)
      )

      val checkboxContainerDiv: Element = getSingleElementByClass(doc, "govuk-checkboxes")

      val containerChildren: List[Element] = checkboxContainerDiv.children().asScala.toList

      containerChildren.size shouldBe 4

      containerChildren.head.tagName() shouldBe "div"
      elementAttrs(containerChildren.head)("class") shouldBe "govuk-checkboxes__item"

      containerChildren(1).tagName() shouldBe "div"
      elementAttrs(containerChildren(1))("class") shouldBe "govuk-checkboxes__item"

      containerChildren(2).tagName() shouldBe "p"
      elementAttrs(containerChildren(2))("class").contains("govuk-body")

      containerChildren.last.tagName() shouldBe "div"
      elementAttrs(containerChildren.last)("class") shouldBe "govuk-checkboxes__item"

      val exclusiveCheckboxChildren: List[Element] = containerChildren.last.children().asScala.toList

      exclusiveCheckboxChildren.size shouldBe 3

      exclusiveCheckboxChildren.head.tagName() shouldBe "input"
      elementAttrs(exclusiveCheckboxChildren.head)("aria-describedby") shouldBe "exclusive-hint"

      exclusiveCheckboxChildren(1).tagName() shouldBe "label"
      exclusiveCheckboxChildren(1).text() shouldBe exclusiveHolidayOption.asString

      exclusiveCheckboxChildren.last.tagName() shouldBe "div"
      elementAttrs(exclusiveCheckboxChildren.last)("id") shouldBe "exclusive-hint"
      exclusiveCheckboxChildren.last.text() shouldBe exclusiveHint.asString
    }

  }

}
