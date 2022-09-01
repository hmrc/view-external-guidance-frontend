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

package models.ui


import base.{BaseSpec, TestConstants}

class UIComponentsSpec extends BaseSpec with TestConstants {
  val h1Str: String = "Heading level 1 text"
  val h2Str: String = "Heading level 2 text"
  val h3Str: String = "Heading level 3 text"
  val h4Str: String = "Heading level 4 text"
  val h1 = H1(Text(h1Str))
  val h2 = H2(Text(h2Str))
  val h3 = H3(Text(h3Str))
  val h4 = H4(Text(h4Str))
  val engLeadingText: String = "Leading text"
  val engBulletPointOneText = "Bullet point 1"
  val engBulletPointTwoText = "Bullet point 2"

  "UIComponents" must {

    "use text components with an overriding implementation of the method toString" in {
      val english = "Example text"

      Text(english).toString shouldBe s"[$english]"
    }

    "support isEmpty within Text" in {
      Text().isEmpty shouldBe true
      Text("").isEmpty shouldBe false
      Text("").items.forall(_.isEmpty) shouldBe true
    }

    "Detect numeric value" in {
      Text("23").isNumeric shouldBe true

      Text("2 3").isNumeric shouldBe true

      Text("yes").isNumeric shouldBe false

      Text("").isNumeric shouldBe false

      Text(" ").isNumeric shouldBe false

      Text("This is a sentence with 10 a number").isNumeric shouldBe false
    }

    "Support text in HTML h1 elements" in {
      h1.text.items shouldBe Seq(Words(h1Str))
    }

    "Support text in HTML h2 elements" in {
      h2.text.items shouldBe Seq(Words(h2Str))
    }

    "Support text in HTML h3 elements" in {
      h3.text.items shouldBe Seq(Words(h3Str))
    }

    "Support text in HTML h4 elements" in {
      h4.text.items shouldBe Seq(Words(h4Str))
    }

    "Support the identification of heading components in a list of UIComponents" in {

      val uiComponents: List[UIComponent] = List(h1, h2, h3, h4)

      uiComponents.head match {
        case matchedH1: H1 => matchedH1 shouldBe h1
        case _ => fail("H1 Html component not matched")
      }

      uiComponents(1) match {
        case matchedH2: H2 => matchedH2 shouldBe h2
        case _ => fail("H2 Html component not matched")
      }

      uiComponents(2) match {
        case matchedH3: H3 => matchedH3 shouldBe h3
        case _ => fail("H3 Html component not matched")
      }

      uiComponents(3) match {
        case matchedH4: H4 => matchedH4 shouldBe h4
        case _ => fail("H4 Html component not matched")
      }

    }

    "return language text from bullet point list" in {

      val text: Text = Text(engLeadingText)
      val bulletPointOne: Text = Text(engBulletPointOneText)
      val bulletPointTwo: Text = Text(engBulletPointTwoText)
      val bulletPointList: BulletPointList = BulletPointList(text, Seq(bulletPointOne, bulletPointTwo))

      bulletPointList.text match {
        case text: Text => text shouldBe text
        case _ => fail("The first text item in leading text is not an instance of the class Text")
      }

      bulletPointList.listItems.head match {
        case text: Text => text shouldBe bulletPointOne
        case _ => fail("The first text item in the first bullet point is not an instance of the class Text")
      }

      bulletPointList.listItems(1) match {
        case text: Text => text shouldBe bulletPointTwo
        case _ => fail("The first text item in the second bullet point is not an instance of the class Text")
      }
    }

    "support bullet point lists containing both text and embedded links" in {

      // Create bullet point list with both text and embedded links
      val leadingTxt1: Words = Words("Leading text 1")
      val leadingLink: Link = Link("http://textUrl", "Leading text link", false)
      val leadingTxt2: Words = Words("Leading text 2")
      val bp1Txt: Words = Words("Bullet point 1 text")
      val bp1Link: Link = Link("http://bulletPointOneUrl", "Bullet point 1 link", false)
      val bp2Txt: Words = Words("Bullet point 2 text")
      val bp2Link: Link = Link("http://bulletPointTwoUrl", "Bullet point 2 link", true)
      val bpListLeadingText: Text =
        Text(Seq(leadingTxt1, leadingLink, leadingTxt2))
      val bulletPointListItems: Seq[Text] = Seq(
        Text(Seq(bp1Txt, bp1Link)),
        Text(Seq(bp2Txt, bp2Link))
      )

      val bulletPointList: BulletPointList = BulletPointList(bpListLeadingText, bulletPointListItems)

      // Test components of bullet point list
      bulletPointList.text.items(0).toString shouldBe "Leading text 1"
      bulletPointList.text.items(1).toString shouldBe "[link:Leading text link:http://textUrl:false:None]"
      bulletPointList.text.items(2).toString shouldBe "Leading text 2"

      bulletPointList.listItems.head.items(0).toString shouldBe "Bullet point 1 text"
      bulletPointList.listItems.head.items(1).toString shouldBe "[link:Bullet point 1 link:http://bulletPointOneUrl:false:None]"

      bulletPointList.listItems(1).items(0).toString shouldBe "Bullet point 2 text"
      bulletPointList.listItems(1).items(1).toString shouldBe "[link:Bullet point 2 link:http://bulletPointTwoUrl:true:None]"
    }

    "use Link components with an implementation of the toString method for use in debugging" in {

      val englishLinkText = "Str link text"
      val destination = "http://my.com/page"
      val link: Link = Link(destination, englishLinkText, false)

      link.toString shouldBe s"[link:$englishLinkText:$destination:false:None]"
    }

    "use Button Link components with an implementation of the toString method for use in debugging" in {

      val englishLinkText = "Str link text"
      val destination = "http://my.com/page"
      val link: Link = Link(destination, englishLinkText, false, true)

      link.toString shouldBe s"[button:$englishLinkText:$destination:false:None]"
    }

    "build into a page of text and link paragraph" in {

      val txt1 = Words("Hello my name is ....")
      val txt2 = Words(" and today is Wednesday")
      val link1 = Link("/secondpage", "Unsure?")
      val paraWithLink = Paragraph(Text(Seq(txt1, link1, txt2)))
      val components = Seq(paraWithLink)
      val page = Page("/firstpage", components)

      page.components.length shouldBe 1
      page.components.foreach {
        case p: Paragraph => p.text.items.length shouldBe 3
        case _ => fail("unknown ParagraphItem")
      }
    }

    "use Link components which correctly support isEmpty" in {
      Link("/secondpage", "").isEmpty shouldBe true
      Link("/secondpage", "Hello").isEmpty shouldBe false
    }

    "use Link components which correctly support toString" in {
      val en: String = "Hello"
      Link("4", en).toString shouldBe s"[link:${en}:4:false:None]"
    }

    "use link components that add a query string marker to the destination when it matches the previous page in the guidance" in {

      val destination: String = "/guidance/test/page-2"

      val link: Link = Link(destination, "See page 2", window = false)

      val backLink: Option[String] = Some(destination)

      link.getDest(backLink) shouldBe s"$destination?$PreviousPageLinkQuery"
    }

    "use link components that do not add a query string marker to the destination when it does not match the previous page" in {

      val destination: String = "/guidance/test/page-5"

      val link: Link = Link(destination, "See page 5", window = false)

      val backLink: Option[String] = Some("/guidance/test/page-3")

      link.getDest(backLink) shouldBe destination
    }

    "use link components that do not add a query string marker to the destination when the previous page is not defined" in {

      val destination: String = "/guidance/test/page-4"

      val link: Link = Link(destination, "See page 4", window = true, asButton = true, hint = Some("Hint"))

      link.getDest(None) shouldBe destination
    }

    "use LabelRef components which correctly support isEmpty" in {
      LabelRef("Blah").isEmpty shouldBe false
    }

    "use LabelRef components which correctly support toString" in {
      LabelRef("BLAH").toString shouldBe s"[label:BLAH:Txt]"
    }

    "use LabelRef components which correctly support toWords" in {
      LabelRef("This is a label").toWords shouldBe List("This", "is", "a", "label")
    }

    "allow creation of Text object containing a simple LabelRef with labelRef() helper" in {
      Text.labelRef("BLAH") shouldBe Text(Seq(LabelRef("BLAH")))
    }

    "build a complete page" in {
      // Define page title
      val h1Page: H1 = H1(Text("Title text"))
      // Define opening paragraph
      val openingParagraphText: Text = Text("Welcome")
      val openingParagraph: Paragraph = Paragraph(openingParagraphText)
      // Define sub-title
      val h2Page: H2 = H2(Text("Subtitle"))
      // Define second paragraph with embedded hyperlink
      val secondParagraphLeadingText: Text = Text("Opening section of second paragraph")
      val secondLink: Link = Link("http://secondParagraphUrl", "Second paragraph link text", false)
      val link = Text(secondLink)
      val secondParagraphClosingText: Text = Text("Closing section of second paragraph")
      val secondParagraph: Paragraph = Paragraph(secondParagraphLeadingText + link + secondParagraphClosingText)
      // Define text of some note
      val h3Page: H3 = H3(Text("Note"))
      // Define bullet point list
      val text: Text = Text("Leading text for bullet point list")
      val bulletPointOne: Text = Text("Bullet point 1")
      val bulletPointTwo: Text = Text("Bullet point 2")
      val bulletPointThree: Text = Text("Bullet point 3")
      val bp3Link: Link = Link("http://thirdBulletPointUrl", "Third bullet point link", false)
      val bulletPointList: BulletPointList = BulletPointList(
        text,
        Seq(bulletPointOne, bulletPointTwo, bulletPointThree + Text(bp3Link))
      )

      val components: Seq[UIComponent] = Seq(h1Page, openingParagraph, h2Page, secondParagraph, h3Page, bulletPointList)

      val page: Page = Page("/pageUrl", components)

      page.components.length shouldBe 6

      page.components.head match {
        case matchedH1: H1 => matchedH1 shouldBe h1Page
        case _ => fail("First component in page is not a H1 component")
      }

      page.components(1) match {
        case matchedOpeningParagraph: Paragraph => matchedOpeningParagraph shouldBe openingParagraph
        case _ => fail("Second component in page is not a paragraph")
      }

      page.components(2) match {
        case matchedH2: H2 => matchedH2 shouldBe h2Page
        case _ => fail("Third component in page is not a H2 component")
      }

      page.components(3) match {
        case matchedSecondParagraph: Paragraph => matchedSecondParagraph shouldBe secondParagraph
        case _ => fail("Fourth component in page is not a paragraph")
      }

      page.components(four) match {
        case matchedH3: H3 => matchedH3 shouldBe h3Page
        case _ => fail("Fifth component in page is not a H3 component")
      }

      page.components(five) match {
        case matchedBulletPointList: BulletPointList => matchedBulletPointList shouldBe bulletPointList
        case _ => fail("Sixth component in page is not a bullet point list")
      }
    }

  }

}
