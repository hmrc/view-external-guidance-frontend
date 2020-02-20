/*
 * Copyright 2020 HM Revenue & Customs
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

import base.BaseSpec
import models.ocelot.stanzas._
import models.ocelot._
import utils.StanzaHelper
import models.ui.{Text, HyperLink, PageLink}

class UIBuilderSpec extends BaseSpec with ProcessJson with StanzaHelper {

  trait Test extends ProcessJson {

    val lang0 = Vector("Some Text","Welsh, Some Text")
    val lang1 = Vector("Some Text1","Welsh, Some Text1")
    val lang2 = Vector("Some Text2","Welsh, Some Text2")
    val lang3 = Vector("Some Text3","Welsh, Some Text3")
    val lang4 = Vector("Some Text4","Welsh, Some Text4")

    val ltxt1 = Text("This is a ","Welsh, This is a ", true)
    val ltxt2 = Text(" followed by "," Welsh, followed by ")
    val ltxt3 = Text(" and nothing"," Welsh, and nothing")
    val link1Txt = Text("A link","Welsh, A link")
    val link2Txt = Text("Another Link","Welsh, Another Link")

    val link1Txt2 = Text("A link at start of phrase","Welsh, A link at start of phrase")
    val link2Txt2 = Text("Another Link at end of phrase","Welsh, Another Link at end of phrase")

    val pageLink1Text = Text("A page link","Welsh, A page link")
    val pageLink2Text = Text("Another page link","Welsh, Another page link")

    val link1 = HyperLink("https://www.bbc.co.uk", link1Txt, false)
    val link2 = HyperLink("https://www.gov.uk", link2Txt, false)
    val link2_1 = HyperLink("https://www.bbc.co.uk", link1Txt2, false)
    val link2_2 = HyperLink("https://www.gov.uk", link2Txt2, false)

    val pageLink1 = PageLink("34", pageLink1Text)
    val pageLink2 = PageLink("3", pageLink2Text)

    val txtWithLinks = Phrase(
      Vector("[bold:This is a ][link:A link:https://www.bbc.co.uk] followed by [link:Another Link:https://www.gov.uk] and nothing",
      "[bold:Welsh, This is a ][link:Welsh, A link:https://www.bbc.co.uk] Welsh, followed by [link:Welsh, Another Link:https://www.gov.uk] Welsh, and nothing")
    )
    val txtWithLinks2 = Phrase(
      Vector("[link:A link at start of phrase:https://www.bbc.co.uk] followed by [link:Another Link at end of phrase:https://www.gov.uk]",
      "[link:Welsh, A link at start of phrase:https://www.bbc.co.uk] Welsh, followed by [link:Welsh, Another Link at end of phrase:https://www.gov.uk]")
    )
    val txtWithPageLinks = Phrase(
      Vector("[bold:This is a ][link:A page link:34] followed by [link:Another page link:3] and nothing",
      "[bold:Welsh, This is a ][link:Welsh, A page link:34] Welsh, followed by [link:Welsh, Another page link:3] Welsh, and nothing")
    )
    val txtWithAllLinks = Phrase(
      Vector("[link:A link at start of phrase:https://www.bbc.co.uk] followed by [link:Another Link at end of phrase:34]",
      "[link:Welsh, A link at start of phrase:https://www.bbc.co.uk] Welsh, followed by [link:A page link:34]")
    )

    val linkInstructionStanza = Instruction(Phrase(lang4), Seq("end"), Some(Link(7,"/somewhere","",false)), false)
    val embeddedLinkInstructionStanza = Instruction(txtWithLinks, Seq("end"), None, false)
    val embeddedLinkInstructionStanza2 = Instruction(txtWithLinks2, Seq("end"), None, false)
    val embeddedPageLinkInstructionStanza = Instruction(txtWithPageLinks, Seq("end"), None, false)
    val embeddedAllLinkInstructionStanza = Instruction(txtWithAllLinks, Seq("end"), None, false)

    val initialStanza = Seq(
      ValueStanza(List(Value(Scalar, "PageUrl", "/")), Seq("1"), false),
      Instruction(Phrase(lang2), Seq("2"), None, false),
      Callout(Title, Phrase(lang0), Seq("3"), false),
      Callout(SubTitle, Phrase(lang1), Seq("4"), false),
      Callout(Lede, Phrase(lang2), Seq("5"), false),
      Instruction(Phrase(lang3), Seq("end"), None, false)
    )
    val stanzas = initialStanza ++ Seq(linkInstructionStanza, EndStanza)
    val stanzasWithEmbeddedLinks = initialStanza ++ Seq(embeddedLinkInstructionStanza, EndStanza)
    val stanzasWithEmbeddedLinks2 = initialStanza ++ Seq(embeddedLinkInstructionStanza2, EndStanza)
    val stanzasWithEmbeddedPageLinks = initialStanza ++ Seq(embeddedPageLinkInstructionStanza, EndStanza)
    val stanzasWithEmbeddedAllLinks = initialStanza ++ Seq(embeddedAllLinkInstructionStanza, EndStanza)
    val page = Page("start", "/test-page", stanzas, Seq(""), Nil)

    val textItems = Seq(ltxt1, link1, ltxt2, link2, ltxt3)
    val textItems2 = Seq(link2_1, ltxt2, link2_2)
    val pageLinkTextItems = Seq(ltxt1, pageLink1, ltxt2, pageLink2, ltxt3)
    val AllLinksTextItems = Seq(link2_1, ltxt2, pageLink1)


    val pageWithEmbeddLinks = page.copy(stanzas = stanzasWithEmbeddedLinks)
    val pageWithEmbeddLinks2 = page.copy(stanzas = stanzasWithEmbeddedLinks2)
    val pageWithEmbeddPageLinks = page.copy(stanzas = stanzasWithEmbeddedPageLinks)
    val pageWithEmbeddAllLinks = page.copy(stanzas = stanzasWithEmbeddedAllLinks)
  }

  "UIBuilder placeholder parsing" must {
    "Convert a Text with link placeholders in lang strings to Seq[TextItem]" in new Test {

      val txtItems = UIBuilder.fromText(txtWithLinks)

      txtItems(0) mustBe ltxt1
      txtItems(1) mustBe link1
      txtItems(2) mustBe ltxt2
      txtItems(3) mustBe link2
      txtItems(4) mustBe ltxt3
    }
  }

  "UIBuilder" must {

    "convert and Ocelot page into a UI page with the same url" in new Test{

      UIBuilder.fromStanzaPage(page) match {
        case p if p.urlPath == page.url => succeed
        case p => fail(s"UI page urlPath set incorrectly to ${p.urlPath}")
      }
    }

    "convert 1st Callout type Title to H1" in new Test{
      val uiPage = UIBuilder.fromStanzaPage(page)
      uiPage.components(1) mustBe models.ui.H1(Text(lang0))
    }

    "convert 2nd Callout type SubTitle to H2" in new Test{

      val uiPage = UIBuilder.fromStanzaPage(page)
      uiPage.components(2) mustBe models.ui.H2(Text(lang1))
    }

    "convert Callout type Lede to lede Paragraph" in new Test{

      val uiPage = UIBuilder.fromStanzaPage(page)
      uiPage.components(3) mustBe models.ui.Paragraph(Seq(Text(lang2)), true)
    }

    "convert Simple instruction to Paragraph" in new Test{

      val uiPage = UIBuilder.fromStanzaPage(page)
      uiPage.components(4) mustBe models.ui.Paragraph(Seq(Text(lang3)), false)
    }

    "convert Link instruction to Paragraph" in new Test{

      val uiPage = UIBuilder.fromStanzaPage(page)
      uiPage.components(5) mustBe models.ui.Paragraph(Seq(models.ui.HyperLink("/somewhere", Text(lang4), false)), false)
    }

    "convert page with instruction stanza containing a sequence of Text and HyperLink items" in new Test{

      val uiPage = UIBuilder.fromStanzaPage(pageWithEmbeddLinks)
      uiPage.components(5) mustBe models.ui.Paragraph(textItems, false)
    }

    "convert page with instruction stanza containing a sequence of TextItems beginning and ending with HyperLinks" in new Test{

      val uiPage = UIBuilder.fromStanzaPage(pageWithEmbeddLinks2)
      uiPage.components(5) mustBe models.ui.Paragraph(textItems2, false)
    }

    "convert page with instruction stanza text containing PageLinks and Text" in new Test{

      val uiPage = UIBuilder.fromStanzaPage(pageWithEmbeddPageLinks)
      uiPage.components(5) mustBe models.ui.Paragraph(pageLinkTextItems, false)
    }

  }


}
