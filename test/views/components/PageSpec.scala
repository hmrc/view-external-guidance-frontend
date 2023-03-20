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

package views.components

import base.{ViewFns, ViewSpec}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.data.Forms.nonEmptyText
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest
import forms.FormProvider
import forms.providers.{StringListFormProvider, StringFormProvider}
import models.PageContext
import models.ui.{Answer, BulletPointList, Details, ConfirmationPanel, CurrencyInput, CyaSummaryList}
import models.ui.{FormPage, H1, InsetText, NumberedCircleList, NumberedList, Sequence, SequenceAnswer}
import models.ui.{Paragraph, Question, RequiredErrorMsg, StandardPage, Text, WarningText}
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements

import scala.collection.JavaConverters._

class PageSpec extends AnyWordSpec with Matchers with ViewSpec with ViewFns with GuiceOneAppPerSuite {

  trait Test {
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))
    val fakeRequest = FakeRequest("GET", "/")

    val standardPageView = app.injector.instanceOf[views.html.standard_page]
    val formPageView = app.injector.instanceOf[views.html.form_page]
    val title: Text = Text("Telling HMRC about extra income")

    val openingPara: Text = Text("Check if you need to tell HMRC about extra money you’ve made by selling goods or services, or renting land or property.")

    val bulletPointLeadingText: Text = Text("For example:")

    val bulletPointOne: Text = Text("selling items online or face to face")

    val bulletPointTwo: Text =
      Text("selling freelance services (such as gardening or babysitting)")
    val bulletPointThree: Text = Text("hiring out personal equipment (such as power tools)")

    val para: Paragraph = Paragraph(openingPara)
    val bulletPointList: BulletPointList = BulletPointList(bulletPointLeadingText, Seq(bulletPointOne, bulletPointTwo, bulletPointThree))
    val simplePage: StandardPage = StandardPage("root", Seq(para, H1(title), bulletPointList))

    val confirmationPanelLeadingText: Text = Text("Calculation Complete")
    val confirmationPanelOne:Text = Text("you need to pay IHT")
    val confirmationPanelTwo: Text = Text("£325,000")
    val confirmationPanel: ConfirmationPanel = ConfirmationPanel(confirmationPanelLeadingText, Seq(confirmationPanelOne, confirmationPanelTwo))
    val listOne: Text = Text("Line 1")
    val listTwo: Text = Text("Line 2")
    val listThree: Text = Text("Line 2")
    val numberedList: NumberedList = NumberedList(Seq(listOne, listTwo, listThree))
    val numberedCircleList: NumberedCircleList = NumberedCircleList(Seq(listOne, listTwo, listThree))
    val insetOne: Text = Text("Inset 1")
    val insetTwo: Text = Text("Inset 2")
    val insetText: InsetText = InsetText(Seq(insetOne, insetTwo))
    val warningOne: Text = Text("Warning 1")
    val warningTwo: Text = Text("Warning 2")
    val warningText: WarningText = WarningText(Seq(warningOne, warningTwo))
    val summaryList: CyaSummaryList = CyaSummaryList(Seq(Seq(listOne, listOne), Seq(listTwo, listThree)))
    val outcomePage: StandardPage = StandardPage("root", Seq(confirmationPanel, numberedList, insetText, numberedCircleList, summaryList, warningText))

    val a1 = Answer(Text("Yes"), None)
    val a2 = Answer(Text("No"), None)
    val answers = Seq(a1, a2)
    val questionText = Text("Do you agree?")
    val question = Question(questionText, None, Seq(para, bulletPointList), answers)
    val errorMsg = RequiredErrorMsg(Text("An error has occurred"))
    val questionWithErrors = Question(questionText, None, Seq(para, bulletPointList), answers, Seq(errorMsg))
    val textFormProvider = new StringFormProvider
    val listFormProvider = new StringListFormProvider
    val questionPage = FormPage("root", question)
    val questionPageWithErrors = FormPage("root", questionWithErrors)

    val inputText = Text("What is value of your house?")
    val inputHint = Text("use market value")
    val input = CurrencyInput(inputText, Some(inputHint), Seq(para, bulletPointList))
    val inputWithErrors = CurrencyInput(inputText, Some(inputHint), Seq(para, bulletPointList), Seq(errorMsg))
    val inputPage = FormPage("root", input)
    val inputPageWithErrors = FormPage("root", inputWithErrors)

    val sequenceTitle: Text = Text("Select your fruit")
    val fruitOptions: Seq[SequenceAnswer] = Seq(
      SequenceAnswer(Text("Oranges"), None),
      SequenceAnswer(Text("Pears"), None),
      SequenceAnswer(Text("Mangoes"), None)
    )
    val fruitSequence: Sequence = Sequence(sequenceTitle, None, fruitOptions, None, Seq.empty, Seq.empty)
    val sequencePage: FormPage = FormPage("/selectFruit", fruitSequence)

    val exclusiveSequenceTitle: Text = Text("What kind of car would you like?")
    val carTypeOptions: Seq[SequenceAnswer] = Seq(
      SequenceAnswer(Text("Sports car"), None),
      SequenceAnswer(Text("SUV"), None),
      SequenceAnswer(Text("People carrier"), None)
    )
    val carTypeExclusiveAnswer: SequenceAnswer = SequenceAnswer(Text("Other"), Some(Text("Selecting this option will deselect all the other checkboxes")))
    var exclusiveSequence: Sequence = Sequence(
      exclusiveSequenceTitle,
      None,
      carTypeOptions,
      Some(carTypeExclusiveAnswer),
      Seq.empty,
      Seq.empty
    )
    var exclusiveSequencePage: FormPage = FormPage("/cars-you-like", exclusiveSequence)

    // Details with single bullet point
    val caption: Text = Text("Title")

    val bpl1LeadingText: Text = Text("Choose your favourite sweets")

    val bpl1ListItem1: Text = Text("Wine gums")
    val bpl1ListItem2: Text = Text("Bonbons")
    val bpl1ListItem3: Text = Text("Fruit pastilles")

    val bpl1TextGroup1: Seq[Text] = Seq(
      bpl1ListItem1,
      bpl1ListItem2,
      bpl1ListItem3
    )
    val bpList1 = BulletPointList(bpl1LeadingText, bpl1TextGroup1)

    val details: Details = Details(caption, Seq(bpList1))

    val detailsPage: StandardPage = StandardPage("/complex-details-page", Seq(details))

    def expectedTitleText(h1Text: String, section: Option[String] = None): String =
      section.fold(s"${h1Text} – ${messages("service.name")} – ${messages("service.govuk")}"){s =>
        s"${h1Text} – ${s} – ${messages("service.name")} – ${messages("service.govuk")}"
      }

    def checkTitle(doc: Document, section: Option[String] = None, prefix: Option[String] = None): Unit =
      Option(doc.getElementsByTag("h1").first).fold(fail("Missing H1")){ h1 =>
        Option(doc.getElementsByTag("title").first).fold(fail("Missing title")){title =>
          prefix.fold(title.text shouldBe expectedTitleText(h1.text, section)){ prefx =>
            title.text shouldBe s"$prefx ${expectedTitleText(h1.text, section)}"
          }
        }
      }

    val pageCtx: PageContext = PageContext(simplePage, Seq.empty, None, "sessionId", Some("/"), Text("Title"), "processId", "processCode")
    val questionPageContext: PageContext = PageContext(questionPage, Seq.empty, None, "sessionId", Some("/here"), Text("Title"), "processId", "processCode")
    val inputPageContext: PageContext = PageContext(inputPage, Seq.empty, None, "sessionId", Some("/here"), Text("Title"), "processId", "processCode")
    val sequenceContext: PageContext = PageContext(
      sequencePage,
      Seq.empty,
      None,
      "sessionId",
      Some("selectFruit"),
      Text("Get your fruit"),
      "processId",
      "processCode")
    val exclusiveSequenceContext: PageContext = PageContext(
      exclusiveSequencePage,
      Seq.empty,
      None,
      "sessionId",
      Some("/cars-you-like"),
      Text("Select a car you like"),
      "processId",
      "processCode"
    )
    val detailsContext: PageContext = PageContext(
      detailsPage,
      Seq.empty,
      None,
      "sessionId",
      Some("/details"),
      Text(),
      "processId",
      "processCode"
    )
  }

  "Standard Page component" should {

    "generate English html containing an H1, a text only paragraph and a test only bullet point list" in new Test {
      val doc = asDocument(standardPageView(simplePage, pageCtx)(fakeRequest, messages))

      val h1s = doc.getElementsByTag("h1")
      h1s.size shouldBe 1
      h1s.first.text shouldBe title.asString

      val paras = doc.select("main.govuk-main-wrapper p")

      paras.size shouldBe 2

      val firstPara = paras.eq(0)
      firstPara.first.text shouldBe openingPara.asString

      val secondPara = paras.eq(1)
      secondPara.first.text shouldBe bulletPointLeadingText.asString

      val actualListItems = doc.select("main.govuk-main-wrapper li").asScala.toList
      actualListItems.size shouldBe 3

      val expectedListItems: List[String] = List(bulletPointOne.asString, bulletPointTwo.asString, bulletPointThree.asString)

      assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
    }

    "generate English html containing a confirmation panel, an inset text, a warning text and a numbered list" in new Test {
      val doc = asDocument(standardPageView(outcomePage, pageCtx)(fakeRequest, messages))

      val h1s = doc.getElementsByTag("h1")
      h1s.size shouldBe 1
      h1s.first.text shouldBe confirmationPanelLeadingText.asString

      val paras = doc.select("main.govuk-main-wrapper p")

      paras.size shouldBe 2

      val div = doc.getElementsByClass("govuk-inset-text")
      div.size shouldBe 1

      val insetInfo = div.first().getElementsByTag("p")
      insetInfo.size shouldBe 2

      val firstPara = insetInfo.eq(0)
      firstPara.first.text shouldBe insetOne.asString

      val secondPara = insetInfo.eq(1)
      secondPara.first.text shouldBe insetTwo.asString

      val divWarning = doc.getElementsByClass("govuk-warning-text")
      divWarning.size shouldBe 1

      val warningInfo = divWarning.first().getElementsByClass("govuk-warning-text__text")
      warningInfo.size shouldBe 1

      val numberedListItem = doc.getElementsByClass("govuk-list--number")
      numberedListItem.size shouldBe 2

      val actualListItems = numberedListItem.first().getElementsByTag("li").asScala.toList

      val expectedListItems: List[String] =
        List(listOne.asString, listTwo.asString, listThree.asString)

      assert(actualListItems.map(_.text) == expectedListItems, "\nActual numbered list items do not match those expected")

      val numberedCircleListItem = doc.getElementsByClass("steps")
      numberedCircleListItem.size shouldBe 1

      val actualCircleListItems = numberedCircleListItem.first().getElementsByTag("li").asScala.toList

      assert(actualCircleListItems.map(_.text) == expectedListItems, "\nActual numbered circle list items do not match those expected")

      val summaryListItem = doc.getElementsByClass("govuk-summary-list")
      summaryListItem.size shouldBe 1

      val summaryListRows = summaryListItem.first().getElementsByClass("govuk-summary-list__row")
      summaryListRows.size shouldBe 2

    }

  }

  "Question Page component" should {

    "generate English html containing an H1, a text only paragraph and a text only bullet point list" in new Test {

      val doc = asDocument(formPageView(questionPage, pageCtx, "question", textFormProvider("url") )(fakeRequest, messages))

      checkTitle(doc)

      val h1s = doc.getElementsByTag("h1")
      h1s.size shouldBe 1
      h1s.first.text shouldBe questionText.asString

      val paras = doc.select("main.govuk-main-wrapper p")

      paras.size shouldBe 2

      val firstPara = paras.eq(0)
      firstPara.first.text shouldBe openingPara.asString

      val secondPara = paras.eq(1)
      secondPara.first.text shouldBe bulletPointLeadingText.asString

      val actualListItems = doc.select("main.govuk-main-wrapper li").asScala.toList
      actualListItems.size shouldBe 3

      val expectedListItems: List[String] =
        List(bulletPointOne.asString, bulletPointTwo.asString, bulletPointThree.asString)

      assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
    }

    "generate Englsh title prefixed by Error: when errors are displayed" in new Test {

      val questionPageContextWithErrs = questionPageContext.copy(page = questionPageWithErrors)

      val doc = asDocument(formPageView(questionPageWithErrors, questionPageContextWithErrs, "question", textFormProvider("url") )(fakeRequest, messages))

      checkTitle(doc, None, Some(messages("error.browser.title.prefix")))
    }

    "set radios fieldset aria-describedby correctly whn error occurs" in new Test {

      val questionPageContextWithErrs = questionPageContext.copy(page = questionPageWithErrors)

      val doc = asDocument(formPageView(questionPageWithErrors, questionPageContextWithErrs, "question", textFormProvider("url") )(fakeRequest, messages))

      val fieldset: Element = doc.getElementsByTag("fieldset").first
      Option(fieldset).fold(fail("Missing fieldset")){fset =>
        elementAttrs(fset)("aria-describedby").contains("required-error") shouldBe true
      }
    }

  }

  "Input Page component" should {

    "generate English html containing an H1, a text only paragraph and a text only bullet point list" in new Test {

      val doc = asDocument(formPageView(inputPage, pageCtx, "input",  textFormProvider("12000")) (fakeRequest, messages))

      checkTitle(doc)

      val h1s = doc.getElementsByTag("h1")
      h1s.size shouldBe 1
      h1s.first.text shouldBe inputText.asString

      val paras = doc.select("main.govuk-main-wrapper p")

      paras.size shouldBe 2

      val firstPara = paras.eq(0)
      firstPara.first.text shouldBe openingPara.asString

      val secondPara = paras.eq(1)
      secondPara.first.text shouldBe bulletPointLeadingText.asString

      val actualListItems = doc.select("main.govuk-main-wrapper li").asScala.toList
      actualListItems.size shouldBe 3

      val expectedListItems: List[String] =
        List(bulletPointOne.asString, bulletPointTwo.asString, bulletPointThree.asString)

      assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
    }

    "generate English title prefixed by Error: when errors are displayed" in new Test {

      val inputPageContextWithErrs = inputPageContext.copy(page = inputPageWithErrors)

      val doc = asDocument(formPageView(inputPageWithErrors, inputPageContextWithErrs, "input", textFormProvider("12000") )(fakeRequest, messages))

      checkTitle(doc, None, Some(messages("error.browser.title.prefix")))
    }

    "set input aria-describedby correctly when error occurs" in new Test {

      val inputPageContextWithErrs = inputPageContext.copy(page = inputPageWithErrors)

      val doc = asDocument(formPageView(inputPageWithErrors, inputPageContextWithErrs, "input", textFormProvider("12000") )(fakeRequest, messages))

      val inputField: Element = doc.getElementsByTag("input").first
      Option(inputField).fold(fail("Missing fieldset")){inp =>
        elementAttrs(inp)("aria-describedby").contains("required-error") shouldBe true
      }
    }
  }

  "sequence page" should {

    "generate a sequence component with a title and multiple check boxes" in new Test {

      val doc: Document = asDocument(
        formPageView(
          sequencePage,
          sequenceContext,
          "fruit",
          listFormProvider("fruit")
        )(fakeRequest, messages)
      )

      checkTitle(doc)

      val headings: Elements = doc.getElementsByTag("h1")

      headings.size shouldBe 1

      headings.first.text() shouldBe sequenceTitle.asString

      val checkboxContainerDiv: Element = getSingleElementByClass(doc, "govuk-checkboxes")

      val checkboxDivs: Elements = checkboxContainerDiv.getElementsByClass("govuk-checkboxes__item")

      checkboxDivs.size() shouldBe 3
    }
  }

  "exclusive sequence page" should {

    "generate a sequence component with a title, multiple check boxes and divider" in new Test {

      val doc:Document = asDocument(
        formPageView(
          exclusiveSequencePage,
          exclusiveSequenceContext,
          "cars",
          listFormProvider("cars")
        )(fakeRequest, messages)
      )

      checkTitle(doc)

      val headings: Elements = doc.getElementsByTag("h1")

      headings.size shouldBe 1

      headings.first.text() shouldBe exclusiveSequenceTitle.asString

      val checkboxContainerDiv: Element = getSingleElementByClass(doc, "govuk-checkboxes")

      val containerChildren: List[Element] = checkboxContainerDiv.children().asScala.toList

      containerChildren.size shouldBe 5

      elementAttrs(containerChildren.head)("class") shouldBe "govuk-checkboxes__item"
      elementAttrs(containerChildren(1))("class") shouldBe "govuk-checkboxes__item"
      elementAttrs(containerChildren(2))("class") shouldBe "govuk-checkboxes__item"

      containerChildren(3).tagName() shouldBe "p"
      elementAttrs(containerChildren(3))("class").contains("govuk-body")

      elementAttrs(containerChildren.last)("class") shouldBe "govuk-checkboxes__item"
    }
  }

  "Details page" should {

    "generate a Details page with a single bullet point list" in new Test {

      val doc: Document = asDocument(standardPageView(detailsPage, detailsContext)(fakeRequest, messages))

      val detailsElements: Elements = doc.getElementsByTag("details")

      detailsElements.size shouldBe 1

      val summaries: Elements = detailsElements.first.getElementsByTag("summary")

      summaries.size shouldBe 1

      summaries.text() shouldBe caption.asString

      val divs: Elements = detailsElements.first.getElementsByTag("div")

      divs.size shouldBe 1

      val div: Element = divs.first

      val divChildren: Elements = div.children()

      divChildren.first.tag.toString shouldBe "p"
      divChildren.first.text() shouldBe bpl1LeadingText.asString

      divChildren.last.tag.toString shouldBe "ul"

      val listItems: List[Element] = divChildren.last.getElementsByTag("li").asScala.toList

      listItems.size shouldBe 3
    }
  }
}
