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

package services

import base.{BaseSpec, WelshLanguage}
import core.models.ocelot._
import models.PageDesc
import models.ui.{Link, Text, Words}
import play.api.i18n.{Messages, MessagesApi}
import core.services.EncrypterService
import mocks.MockAppConfig

class WelshTextBuilderSpec extends BaseSpec with WelshLanguage {

  trait Test extends ProcessJson {

    val messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    implicit val messages: Messages = messagesApi.preferred(Seq(TextBuilder.Welsh))
    val encrypter = new EncrypterService(MockAppConfig)

    val words1 = Words("Welsh: This is a ", true)
    val words2 = Words(" Welsh: followed by ")
    val words3 = Words(" Welsh: and nothing")
    val link1EnWords = "A link"
    val link1CyWords = "Welsh: A link"
    val link2EnWords = "Another Link"
    val link2CyWords = "Welsh: Another Link"

    val link1 = Link("https://www.bbc.co.uk", link1CyWords, false)
    val link2 = Link("https://www.gov.uk", link2CyWords, false)
    val linkPrint = "javascript:window.print()"
    val testEmail = "test@example.com"
    val emailLink = "mailto:test@example.com"
    val urlMap1: Map[String, PageDesc] = Map("start" -> PageDesc("start",  "dummy-path/start", Nil),
                                             "3" -> PageDesc("3", "dummy-path", Nil),
                                             "5" -> PageDesc("5","dummy-path/blah", Nil),
                                             "34" -> PageDesc("34","dummy-path/next", Nil))

    val txtWithLinks = Phrase(
      Vector(
        s"[bold:This is a ][link:${link1EnWords}:https://www.bbc.co.uk] followed by [link:${link2EnWords}:https://www.gov.uk] and nothing",
        s"[bold:Welsh: This is a ][link:${link1CyWords}:https://www.bbc.co.uk] Welsh: followed by [link:${link2CyWords}:https://www.gov.uk] Welsh: and nothing"
      )
    )

    val txtWithPrintLink = Phrase(
      Vector(
        s"Click [link:here:$linkPrint] to print this page for your records",
        s"Welsh: Click [link:here:$linkPrint] to print this page for your records"
      )
    )

    val txtWithEmailLink = Phrase(
      Vector(
        s"Contact us at [link:$testEmail:$emailLink] to get help",
        s"Welsh: Contact us at [link:$testEmail:$emailLink] to get help"
      )
    )

    val brokenLinkPhrase = Phrase(Vector("Hello [link:Blah Blah:htts://www.bbc.co.uk]", "Welsh: Hello [link:Blah Blah:htts://www.bbc.co.uk]"))

    val labelsMap = Map(
      "BLAH" -> ScalarLabel("BLAH", List("2.0")),
      "When" -> ScalarLabel("When", List("21/09/1973")),
      "X"->ScalarLabel("X", List("33.5")),
      "Y"->ScalarLabel("Y"),
      "Name" -> ScalarLabel("Name", List("Coltrane")),
      "Colours" -> ListLabel("Colours", List("Red", "Green", "Blue"))
    )

    implicit val ctx: UIContext = UIContext(LabelCache(labelsMap), urlMap1, messages)

    val answerWithNoHint = Phrase("Yes", "Welsh: Yes")
    val answerWithHint = Phrase("Yes[hint:You agree with the assertion]", "Welsh: Yes[hint:Welsh: You agree with the assertion]")

    val answer = Text("Welsh: Yes")
    val hint = Text("Welsh: You agree with the assertion")
  }

  "TextBuilder placeholder parsing" must {

    "Convert label reference with default output format placeholders within phrase to expanded TextItems" in new Test {
      val p = Phrase("""Sentence containing [label:BLAH] (label reference)""", """Welsh: Sentence containing [label:BLAH] (label reference)""")
      val expanded = TextBuilder.expandLabels(p, ctx.labels)
      TextBuilder.fromPhrase(expanded).items shouldBe Seq(Words("Welsh: Sentence containing 2.0 (label reference)"))
      TextBuilder.fromPhraseWithOptionalHint(expanded)._1.items shouldBe Seq(Words("Welsh: Sentence containing 2.0 (label reference)"))
    }

    "Convert label reference with currency output format placeholders within phrase to expanded TextItems" in new Test {
      val p = Phrase("""Sentence containing [label:BLAH:currency] (label reference)""",
                     """Welsh: Sentence containing [label:BLAH:currency] (label reference)""")
      val expanded = TextBuilder.expandLabels(p, ctx.labels)
      TextBuilder.fromPhrase(expanded).items shouldBe Seq(Words("Welsh: Sentence containing £2.00 (label reference)"))
      TextBuilder.fromPhraseWithOptionalHint(expanded)._1.items shouldBe Seq(Words("Welsh: Sentence containing £2.00 (label reference)"))
    }

    "Convert label reference with date output format placeholders within phrase to expanded TextItems" in new Test {
      val p = Phrase("""Sentence with a [label:When:date] label reference""", """Welsh: Sentence with a [label:When:date] label reference""")
      val expanded = TextBuilder.expandLabels(p, ctx.labels)
      TextBuilder.fromPhrase(expanded).items shouldBe Seq(Words("Welsh: Sentence with a 21 Medi 1973 label reference"))
      TextBuilder.fromPhraseWithOptionalHint(expanded)._1.items shouldBe Seq(Words("Welsh: Sentence with a 21 Medi 1973 label reference"))
    }

    "Convert a label placeholder within a bold placeholder to a bold label ref" in new Test {
      val p = Phrase("""Sentence with a [bold:[label:BLAH]] label reference""", """Welsh: Sentence with a [bold:[label:BLAH]] label reference""")
      val expanded = TextBuilder.expandLabels(p, ctx.labels)
      TextBuilder.fromPhrase(expanded).items shouldBe Seq(Words("Welsh: Sentence with a "), Words("2.0", true), Words(" label reference"))
      TextBuilder.fromPhraseWithOptionalHint(expanded)._1.items shouldBe Seq(Words("Welsh: Sentence with a "), Words("2.0", true), Words(" label reference"))
    }

    "Convert a label placeholderwith currency output format within a bold placeholder to a bold label ref" in new Test {
      val p = Phrase("""Sentence with a [bold:[label:BLAH:currencyPoundsOnly]] label reference""",
                     """Welsh: Sentence with a [bold:[label:BLAH:currencyPoundsOnly]] label reference""")
      val expanded = TextBuilder.expandLabels(p, ctx.labels)
      TextBuilder.fromPhrase(expanded).items shouldBe Seq(Words("Welsh: Sentence with a "), Words("£2", true), Words(" label reference"))
      TextBuilder.fromPhraseWithOptionalHint(expanded)._1.items shouldBe Seq(Words("Welsh: Sentence with a "), Words("£2", true), Words(" label reference"))
    }

    "Convert button link placeholders within phrase to Link as button TextItems" in new Test {
      val p = Phrase("""Sentence with a [button:BLAH:3] label reference""", """Welsh: Sentence with a [button:BLAH:3] label reference""")
      TextBuilder.fromPhrase(p).items shouldBe Seq(Words("Welsh: Sentence with a "), Link("dummy-path", "BLAH", false, true), Words(" label reference"))
      TextBuilder.fromPhraseWithOptionalHint(p)._1.items shouldBe
        Seq(Words("Welsh: Sentence with a "), Link("dummy-path", "BLAH", false, true), Words(" label reference"))
    }

    "Convert button link in tab placeholders within phrase to Link as button TextItems" in new Test {
      val p = Phrase("""Sentence with a [button-tab:BLAH:3] label reference""", """Welsh: Sentence with a [button-tab:BLAH:3] label reference""")
      TextBuilder.fromPhrase(p).items shouldBe Seq(Words("Welsh: Sentence with a "), Link("dummy-path", "BLAH", true, true), Words(" label reference"))
      TextBuilder.fromPhraseWithOptionalHint(p)._1.items shouldBe
        Seq(Words("Welsh: Sentence with a "), Link("dummy-path", "BLAH", true, true), Words(" label reference"))
    }

    "Convert [list:<label>:length] placeholders within phrase to Words TextItems containing the list label length" in new Test {
      val p = Phrase("""The number of colours is [list:Colours:length]""", """Welsh: The number of colours is [list:Colours:length]""")
      val expanded = TextBuilder.expandLabels(p, ctx.labels)
      TextBuilder.fromPhrase(expanded).items shouldBe Seq(Words("Welsh: The number of colours is 3"))
      TextBuilder.fromPhraseWithOptionalHint(expanded)._1.items shouldBe Seq(Words("Welsh: The number of colours is 3"))
    }

    "Convert a Text with link placeholders in lang strings to Seq[TextItem]" in new Test {
      val txt = TextBuilder.fromPhrase(txtWithLinks)
      txt.items(0) shouldBe words1
      txt.items(1).toWords shouldBe Link("https://www.bbc.co.uk", link1CyWords).toWords
      txt.items(2) shouldBe words2
      txt.items(3).toWords shouldBe Link("https://www.gov.uk", link2CyWords).toWords
      txt.items(4) shouldBe words3

      val altTxt = TextBuilder.fromPhraseWithOptionalHint(txtWithLinks)._1
      altTxt.items(0) shouldBe words1
      altTxt.items(1).toWords shouldBe Link("https://www.bbc.co.uk", link1CyWords).toWords
      altTxt.items(2) shouldBe words2
      altTxt.items(3).toWords shouldBe Link("https://www.gov.uk", link2CyWords).toWords
      altTxt.items(4) shouldBe words3
    }

    "leave syntactically incorrect link placeholders as text within a phrase" in new Test {
      val txt: Text = TextBuilder.fromPhrase(brokenLinkPhrase)
      txt.items.length shouldBe 1
      txt.items(0) shouldBe Words("Welsh: Hello [link:Blah Blah:htts://www.bbc.co.uk]")

      val altTxt: Text = TextBuilder.fromPhraseWithOptionalHint(brokenLinkPhrase)._1
      altTxt.items.length shouldBe 1
      altTxt.items(0) shouldBe Words("Welsh: Hello [link:Blah Blah:htts://www.bbc.co.uk]")
    }

    "convert syntactically correct link placeholders into Link objects" in new Test {
      val linkPhrase = Phrase("Hello [link:Blah Blah:https://www.bbc.co.uk] [link:Blah Blah:5]",
                              "Welsh: Hello [link:Blah Blah:https://www.bbc.co.uk] [link:Blah Blah:5]")
      val txt: Text = TextBuilder.fromPhrase(linkPhrase)

      txt.items.length shouldBe 4
      txt.items(0) shouldBe Words("Welsh: Hello ")
      txt.items(1) shouldBe Link("https://www.bbc.co.uk", "Blah Blah")
      txt.items(2) shouldBe Words(" ")
      txt.items(3) shouldBe Link("dummy-path/blah", "Blah Blah")

      val altTxt: Text = TextBuilder.fromPhraseWithOptionalHint(linkPhrase)._1

      altTxt.items.length shouldBe 4
      altTxt.items(0) shouldBe Words("Welsh: Hello ")
      altTxt.items(1) shouldBe Link("https://www.bbc.co.uk", "Blah Blah")
      altTxt.items(2) shouldBe Words(" ")
      altTxt.items(3) shouldBe Link("dummy-path/blah", "Blah Blah")
    }

    "Convert all link placeholders into appropriate Link objects" in new Test {
      val linkPhrase = Phrase("Hello [link:Blah Blah:https://www.bbc.co.uk] [link:Blah Blah:5] [link:Blah Blah:start]",
                              "Welsh: Hello [link:Blah Blah:https://www.bbc.co.uk] [link:Blah Blah:5] [link:Blah Blah:start]")
      val txt = TextBuilder.fromPhrase(linkPhrase)

      txt.items.length shouldBe 6
      txt.items(1) shouldBe Link("https://www.bbc.co.uk", "Blah Blah", false)
      txt.items(3) shouldBe Link("dummy-path/blah", "Blah Blah", false)
      txt.items(5) shouldBe Link("dummy-path/start", "Blah Blah", false)

      val altTxt = TextBuilder.fromPhraseWithOptionalHint(linkPhrase)._1

      altTxt.items.length shouldBe 6
      altTxt.items(1) shouldBe Link("https://www.bbc.co.uk", "Blah Blah", false)
      altTxt.items(3) shouldBe Link("dummy-path/blah", "Blah Blah", false)
      altTxt.items(5) shouldBe Link("dummy-path/start", "Blah Blah", false)
    }

    "Convert all link-same placeholders into appropriate Link objects" in new Test {
      val linkSamePhrase = Phrase("Hello [link-same:Blah Blah:https://www.bbc.co.uk] [link-same:Blah Blah:5] [link-same:Blah Blah:start]",
                                  "Welsh: Hello [link-same:Blah Blah:https://www.bbc.co.uk] [link-same:Blah Blah:5] [link-same:Blah Blah:start]")
      val txt = TextBuilder.fromPhrase(linkSamePhrase)

      txt.items.length shouldBe 6
      txt.items(1) shouldBe Link("https://www.bbc.co.uk", "Blah Blah", false)
      txt.items(3) shouldBe Link("dummy-path/blah", "Blah Blah", false)
      txt.items(5) shouldBe Link("dummy-path/start", "Blah Blah", false)

      val altTxt = TextBuilder.fromPhraseWithOptionalHint(linkSamePhrase)._1

      altTxt.items.length shouldBe 6
      altTxt.items(1) shouldBe Link("https://www.bbc.co.uk", "Blah Blah", false)
      altTxt.items(3) shouldBe Link("dummy-path/blah", "Blah Blah", false)
      altTxt.items(5) shouldBe Link("dummy-path/start", "Blah Blah", false)
    }

    "Convert all link-tab placeholders into appropriate Link objects" in new Test {
      val linkTabPhrase = Phrase("Hello [link-tab:Blah Blah:https://www.bbc.co.uk] [link-tab:Blah Blah:5] [link-tab:Blah Blah:start]",
                                 "Welsh: Hello [link-tab:Blah Blah:https://www.bbc.co.uk] [link-tab:Blah Blah:5] [link-tab:Blah Blah:start]")
      val txt = TextBuilder.fromPhrase(linkTabPhrase)

      txt.items.length shouldBe 6
      txt.items(1) shouldBe Link("https://www.bbc.co.uk", "Blah Blah", true)
      txt.items(3) shouldBe Link("dummy-path/blah", "Blah Blah", true)
      txt.items(5) shouldBe Link("dummy-path/start", "Blah Blah", true)

      val altTxt = TextBuilder.fromPhraseWithOptionalHint(linkTabPhrase)._1

      altTxt.items.length shouldBe 6
      altTxt.items(1) shouldBe Link("https://www.bbc.co.uk", "Blah Blah", true)
      altTxt.items(3) shouldBe Link("dummy-path/blah", "Blah Blah", true)
      altTxt.items(5) shouldBe Link("dummy-path/start", "Blah Blah", true)
    }

    "Convert all javascript-link placeholders into appropriate link objects" in new Test {
      val txt = TextBuilder.fromPhrase(txtWithPrintLink)

      txt.items.length shouldBe 3

      txt.items(0) shouldBe Words("Welsh: Click ")
      txt.items(1) shouldBe Link(linkPrint, "here")
      txt.items(2) shouldBe Words(" to print this page for your records")

      val altTxt = TextBuilder.fromPhraseWithOptionalHint(txtWithPrintLink)._1

      altTxt.items.length shouldBe 3

      altTxt.items(0) shouldBe Words("Welsh: Click ")
      altTxt.items(1) shouldBe Link(linkPrint, "here")
      altTxt.items(2) shouldBe Words(" to print this page for your records")
    }

    "leave syntactically incorrect javascript link placeholders as text within a phrase" in new Test {
      val phrase = Phrase(Vector(
        "Click [link:here:Javascript:Window.print] to print this page for your records",
        "Welsh: Click [link:here:Javascript:Window.print] to print this page for your records"
      ))
      val txt = TextBuilder.fromPhrase(phrase)

      txt.items.length shouldBe 1
      txt.items(0) shouldBe Words("Welsh: Click [link:here:Javascript:Window.print] to print this page for your records")

      val altTxt = TextBuilder.fromPhraseWithOptionalHint(phrase)._1

      altTxt.items.length shouldBe 1
      altTxt.items(0) shouldBe Words("Welsh: Click [link:here:Javascript:Window.print] to print this page for your records")
    }

    "Convert all email-link placeholders into appropriate link objects" in new Test {
      val txt = TextBuilder.fromPhrase(txtWithEmailLink)

      txt.items.length shouldBe 3

      txt.items(0) shouldBe Words("Welsh: Contact us at ")
      txt.items(1) shouldBe Link(emailLink, "test@example.com")
      txt.items(2) shouldBe Words(" to get help")

      val altTxt = TextBuilder.fromPhraseWithOptionalHint(txtWithEmailLink)._1

      altTxt.items.length shouldBe 3

      altTxt.items(0) shouldBe Words("Welsh: Contact us at ")
      altTxt.items(1) shouldBe Link(emailLink, "test@example.com")
      altTxt.items(2) shouldBe Words(" to get help")
    }

    "leave syntactically incorrect email link placeholders as text within a phrase" in new Test {
      val phrase = Phrase(Vector(
        "Contact us at  [link:test@example.com:mailtotest@example.com] to get help",
        "Welsh: Contact us at  [link:test@example.com:mailtotest@example.com] to get help"
      ))
      val txt = TextBuilder.fromPhrase(phrase)

      txt.items.length shouldBe 1
      txt.items(0) shouldBe Words("Welsh: Contact us at  [link:test@example.com:mailtotest@example.com] to get help")

      val altTxt = TextBuilder.fromPhraseWithOptionalHint(phrase)._1

      altTxt.items.length shouldBe 1
      altTxt.items(0) shouldBe Words("Welsh: Contact us at  [link:test@example.com:mailtotest@example.com] to get help")
    }
  }

  trait ExpandTest extends Test {
    override val labelsMap = Map(
      "BLAH"->ScalarLabel("BLAH", List("33.5"), List("43.5")),
      "SomeDate"->ScalarLabel("SomeDate", List("22/9/1973"), List("23/9/1973")),
      "SomeList" -> ListLabel("SomeList", List("x", "y", "z")))

    val labels: Labels = LabelCache(labelsMap, Map(), Nil, Map(), Map(), message(lang), Published, encrypter)
    override implicit val ctx: UIContext = UIContext(labels, urlMap1, messages)
  }

  "TextBuilder expandLabels" must {

    "Convert label reference with default output format placeholders within phrase" in new ExpandTest {
      val phrase = Phrase("""Sentence with a [label:BLAH] label reference""", """Welsh: Sentence with a [label:BLAH] label reference""")
      val expectedPhrase = Phrase("""Sentence with a [label:BLAH] label reference""", """Welsh: Sentence with a 43.5 label reference""")
      TextBuilder.expandLabels(phrase, labels) shouldBe expectedPhrase
    }

    "Convert label reference with currency output format placeholders within phrase" in new ExpandTest {
      val phrase = Phrase("""Sentence with a [label:BLAH:currency] label reference""", """Welsh: Sentence with a [label:BLAH:currency] label reference""")
      val expectedPhrase = Phrase("""Sentence with a [label:BLAH:currency] label reference""", """Welsh: Sentence with a £43.50 label reference""")
      TextBuilder.expandLabels(phrase, labels) shouldBe expectedPhrase
    }

    "Convert label reference with date output format placeholders within phrase" in new ExpandTest {
      val phrase = Phrase("""Sentence with a [label:SomeDate:date] label reference""", """Welsh: Sentence with a [label:SomeDate:date] label reference""")
      val expectedPhrase = Phrase("""Sentence with a [label:SomeDate:date] label reference""", """Welsh: Sentence with a 23 Medi 1973 label reference""")
      TextBuilder.expandLabels(phrase, labels) shouldBe expectedPhrase
    }

    "Convert a label placeholder within a bold placeholder to a bold label ref" in new ExpandTest {
      val phrase = Phrase("""Sentence with a [bold:[label:BLAH]] label reference""", """Welsh: Sentence with a [bold:[label:BLAH]] label reference""")
      val expectedPhrase = Phrase("""Sentence with a [bold:[label:BLAH]] label reference""", """Welsh: Sentence with a [bold:43.5] label reference""")
      TextBuilder.expandLabels(phrase, labels) shouldBe expectedPhrase
    }

    "Convert a label placeholder with currency output format within a bold placeholder" in new ExpandTest {
      val phrase = Phrase("""Sentence with a [bold:[label:BLAH:currencyPoundsOnly]] label reference""",
                          """Welsh: Sentence with a [bold:[label:BLAH:currencyPoundsOnly]] label reference""")
      val expectedPhrase = Phrase("""Sentence with a [bold:[label:BLAH:currencyPoundsOnly]] label reference""", """Welsh: Sentence with a [bold:£43] label reference""")
      TextBuilder.expandLabels(phrase, labels) shouldBe expectedPhrase
    }

    "Convert a list length placeholder " in new ExpandTest {
      val phrase = Phrase("""SomeList has length [list:SomeList:length]""", """Welsh: SomeList has length [list:SomeList:length]""")
      val expectedPhrase = Phrase("""SomeList has length [list:SomeList:length]""", """Welsh: SomeList has length 3""")
      TextBuilder.expandLabels(phrase, labels) shouldBe expectedPhrase
    }

    "Convert a dateplacholder inside a phrase into a date" in new ExpandTest {
      val phrase = Phrase("""Some sentence with a date [date:12/12/2021:dow_name]""", """Welsh: Some sentence with a date [date:12/12/2021:dow_name]""")
      val expectedPhrase = Phrase("""Some sentence with a date [date:12/12/2021:dow_name]""", """Welsh: Some sentence with a date Dydd Sul""")
      TextBuilder.expandLabels(phrase, labels) shouldBe expectedPhrase
    }

    "Convert a dateplacholder inside a phrase into a date label" in new ExpandTest {
      val phrase = Phrase("""Some sentence with a date [date:[label:SomeDate]:dow_name]""", """Welsh: Some sentence with a date [date:[label:SomeDate]:dow_name]""")
      val expectedPhrase = Phrase("""Some sentence with a date [date:[label:SomeDate]:dow_name]""", """Welsh: Some sentence with a date Dydd Sul""")
      TextBuilder.expandLabels(phrase, labels) shouldBe expectedPhrase
    }
  }

  "TextBuilder.fromPhraseWithOptionalHint" must {
    "Return hint when present" in new Test {
      val (mainText, optionalHint): (Text, Option[Text]) = TextBuilder.fromPhraseWithOptionalHint(answerWithHint)
       mainText shouldBe Text("Welsh: Yes")
       optionalHint shouldBe Some(Text("Welsh: You agree with the assertion"))
    }

    "Return no hint when not present" in new Test {
      val (mainText, optionalHint): (Text, Option[Text]) = TextBuilder.fromPhraseWithOptionalHint(answerWithNoHint)
       mainText shouldBe Text("Welsh: Yes")
       optionalHint shouldBe None
    }
  }
}
