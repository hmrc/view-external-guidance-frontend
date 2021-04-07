/*
 * Copyright 2021 HM Revenue & Customs
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

import base.{EnglishLanguage, BaseSpec}
import core.models.ocelot._
import models.ui.{Currency, CurrencyPoundsOnly, DateStandard, LabelRef, Link, Text, Txt, Words}

class EnglishTextBuilderSpec extends BaseSpec {

  trait Test extends ProcessJson with EnglishLanguage {
    val words1 = Words("This is a ", true)
    val words2 = Words(" followed by ")
    val words3 = Words(" and nothing")
    val link1EnWords = "A link"
    val link1CyWords = "Welsh, A link"
    val link2EnWords = "Another Link"
    val link2CyWords = "Welsh, Another Link"

    val link1 = "https://www.bbc.co.uk/news/#nw-c-most-watched-heading__title"
    val link2 = "https://www.gov.uk"
    val urlMap1: Map[String, String] = Map("start" -> "dummy-path/start", "3" -> "dummy-path", "5" -> "dummy-path/blah", "34" -> "dummy-path/next")

    val txtWithLinks = Phrase(
      Vector(
        s"[bold:This is a ][link:${link1EnWords}:${link1}] followed by [link:${link2EnWords}:${link2}] and nothing",
        s"[bold:Welsh, This is a ][link:${link1CyWords}:${link1}] Welsh, followed by [link:${link2CyWords}:${link2}] Welsh, and nothing"
      )
    )

    val brokenLinkPhrase = Phrase(Vector("Hello [link:Blah Blah:htts://www.bbc.co.uk]", "Welsh, Hello [link:Blah Blah:htts://www.bbc.co.uk]"))
    implicit val stanzaIdToUrlMap: Map[String, String] = Map()

    val answerWithNoHint = Phrase("Yes", "Welsh, Yes")
    val answerWithHint = Phrase("Yes[hint:You agree with the assertion]", "Welsh, Yes[hint:Welsh, You agree with the assertion]")

    val answer = Text("Yes")
    val hint = Text("You agree with the assertion")
  }

  "TextBuilder placeholder parsing" must {

    "Convert label reference with default output format placeholders within phrase to LabelRef TextItems" in new Test {
      val p = Phrase("""Sentence with a [label:BLAH] label reference""", """Welsh, Sentence with a [label:BLAH] label reference""")
      TextBuilder.fromPhrase(p).items shouldBe Seq(Words("Sentence with a "), LabelRef("BLAH", Txt), Words(" label reference"))
      TextBuilder.fromPhraseWithOptionalHint(p)._1.items shouldBe Seq(Words("Sentence with a "), LabelRef("BLAH", Txt), Words(" label reference"))
    }

    "Convert label reference with currency output format placeholders within phrase to LabelRef TextItems" in new Test {
      val p = Phrase("""Sentence with a [label:BLAH:currency] label reference""", """Welsh, Sentence with a [label:BLAH:currency] label reference""")
      TextBuilder.fromPhrase(p).items shouldBe Seq(Words("Sentence with a "), LabelRef("BLAH", Currency), Words(" label reference"))
      TextBuilder.fromPhraseWithOptionalHint(p)._1.items shouldBe Seq(Words("Sentence with a "), LabelRef("BLAH", Currency), Words(" label reference"))
    }

    "Convert label reference with date output format placeholders within phrase to LabelRef TextItems" in new Test {
      val p = Phrase("""Sentence with a [label:BLAH:date] label reference""", """Welsh, Sentence with a [label:BLAH:date] label reference""")
      TextBuilder.fromPhrase(p).items shouldBe Seq(Words("Sentence with a "), LabelRef("BLAH", DateStandard), Words(" label reference"))
      TextBuilder.fromPhraseWithOptionalHint(p)._1.items shouldBe Seq(Words("Sentence with a "), LabelRef("BLAH", DateStandard), Words(" label reference"))
    }

    "Convert a label placeholder within a bold placeholder to a bold label ref" in new Test {
      val p = Phrase("""Sentence with a [bold:[label:BLAH]] label reference""", """Welsh, Sentence with a [bold:[label:BLAH]]] label reference""")
      TextBuilder.fromPhrase(p).items shouldBe Seq(Words("Sentence with a "), LabelRef("BLAH", Txt, true), Words(" label reference"))
      TextBuilder.fromPhraseWithOptionalHint(p)._1.items shouldBe Seq(Words("Sentence with a "), LabelRef("BLAH", Txt, true), Words(" label reference"))
    }

    "Convert a label placeholderwith currency output format within a bold placeholder to a bold label ref" in new Test {
      val p = Phrase("""Sentence with a [bold:[label:BLAH:currencyPoundsOnly]] label reference""", """Welsh, Sentence with a [bold:[label:BLAH]:currencyPoundsOnly]] label reference""")
      TextBuilder.fromPhrase(p).items shouldBe Seq(Words("Sentence with a "), LabelRef("BLAH", CurrencyPoundsOnly, true), Words(" label reference"))
      TextBuilder.fromPhraseWithOptionalHint(p)._1.items shouldBe Seq(Words("Sentence with a "), LabelRef("BLAH", CurrencyPoundsOnly, true), Words(" label reference"))
    }

    "Convert button link placeholders within phrase to Link as button TextItems" in new Test {
      val p = Phrase("""Sentence with a [button:BLAH:3] label reference""", """Welsh, Sentence with a [button:BLAH:3] label reference""")
      TextBuilder.fromPhrase(p)(urlMap1, lang).items shouldBe Seq(Words("Sentence with a "), Link("dummy-path", "BLAH", false, true), Words(" label reference"))
      TextBuilder.fromPhraseWithOptionalHint(p)(urlMap1, lang)._1.items shouldBe Seq(Words("Sentence with a "), Link("dummy-path", "BLAH", false, true), Words(" label reference"))
    }

    "Convert button link in tab placeholders within phrase to Link as button TextItems" in new Test {
      val p = Phrase("""Sentence with a [button-tab:BLAH:3] label reference""", """Welsh, Sentence with a [button-tab:BLAH:3] label reference""")
      TextBuilder.fromPhrase(p)(urlMap1, lang).items shouldBe Seq(Words("Sentence with a "), Link("dummy-path", "BLAH", true, true), Words(" label reference"))
      TextBuilder.fromPhraseWithOptionalHint(p)(urlMap1, lang)._1.items shouldBe Seq(Words("Sentence with a "), Link("dummy-path", "BLAH", true, true), Words(" label reference"))
    }

    "Convert a Text with link placeholders in lang strings to Seq[TextItem]" in new Test {
      val txt = TextBuilder.fromPhrase(txtWithLinks)
      txt.items(0) shouldBe words1
      txt.items(1).toWords shouldBe Link(link1, link1EnWords).toWords
      txt.items(2) shouldBe words2
      txt.items(3).toWords shouldBe Link(link2, link2EnWords).toWords
      txt.items(4) shouldBe words3

      val altTxt = TextBuilder.fromPhraseWithOptionalHint(txtWithLinks)._1
      altTxt.items(0) shouldBe words1
      altTxt.items(1).toWords shouldBe Link("https://www.bbc.co.uk", link1EnWords).toWords
      altTxt.items(2) shouldBe words2
      altTxt.items(3).toWords shouldBe Link("https://www.gov.uk", link2EnWords).toWords
      altTxt.items(4) shouldBe words3
    }

    "leave syntactically incorrect link placeholders as text within a phrase" in new Test {
      val txt: Text = TextBuilder.fromPhrase(brokenLinkPhrase)(urlMap1, lang)
      txt.items.length shouldBe 1
      txt.items(0) shouldBe Words("Hello [link:Blah Blah:htts://www.bbc.co.uk]")

      val altTxt: Text = TextBuilder.fromPhraseWithOptionalHint(brokenLinkPhrase)(urlMap1, lang)._1
      altTxt.items.length shouldBe 1
      altTxt.items(0) shouldBe Words("Hello [link:Blah Blah:htts://www.bbc.co.uk]")

    }

    "convert syntactically correct link placeholders into Link objects" in new Test {
      val linkPhrase = Phrase("Hello [link:Blah Blah:https://www.bbc.co.uk] [link:Blah Blah:5]",
                              "Welsh, Hello [link:Blah Blah:https://www.bbc.co.uk] [link:Blah Blah:5]")
      val txt: Text = TextBuilder.fromPhrase(linkPhrase)(urlMap1, lang)

      txt.items.length shouldBe 4
      txt.items(0) shouldBe Words("Hello ")
      txt.items(1) shouldBe Link("https://www.bbc.co.uk", "Blah Blah")
      txt.items(2) shouldBe Words(" ")
      txt.items(3) shouldBe Link("dummy-path/blah", "Blah Blah")

      val altTxt: Text = TextBuilder.fromPhraseWithOptionalHint(linkPhrase)(urlMap1, lang)._1

      altTxt.items.length shouldBe 4
      altTxt.items(0) shouldBe Words("Hello ")
      altTxt.items(1) shouldBe Link("https://www.bbc.co.uk", "Blah Blah")
      altTxt.items(2) shouldBe Words(" ")
      altTxt.items(3) shouldBe Link("dummy-path/blah", "Blah Blah")

    }

    "Convert all link placeholders into appropriate Link objects" in new Test {
      val linkPhrase = Phrase("Hello [link:Blah Blah:https://www.bbc.co.uk] [link:Blah Blah:5] [link:Blah Blah:start]",
                              "Welsh, Hello [link:Blah Blah:https://www.bbc.co.uk] [link:Blah Blah:5] [link:Blah Blah:start]")
      val txt = TextBuilder.fromPhrase(linkPhrase)(urlMap1, lang)

      txt.items.length shouldBe 6
      txt.items(1) shouldBe Link("https://www.bbc.co.uk", "Blah Blah", false)
      txt.items(3) shouldBe Link("dummy-path/blah", "Blah Blah", false)
      txt.items(5) shouldBe Link("dummy-path/start", "Blah Blah", false)

      val altTxt = TextBuilder.fromPhraseWithOptionalHint(linkPhrase)(urlMap1, lang)._1

      altTxt.items.length shouldBe 6
      altTxt.items(1) shouldBe Link("https://www.bbc.co.uk", "Blah Blah", false)
      altTxt.items(3) shouldBe Link("dummy-path/blah", "Blah Blah", false)
      altTxt.items(5) shouldBe Link("dummy-path/start", "Blah Blah", false)
    }

    "Convert all link-same placeholders into appropriate Link objects" in new Test {
      val linkSamePhrase = Phrase("Hello [link-same:Blah Blah:https://www.bbc.co.uk] [link-same:Blah Blah:5] [link-same:Blah Blah:start]",
                                  "Welsh, Hello [link-same:Blah Blah:https://www.bbc.co.uk] [link-same:Blah Blah:5] [link-same:Blah Blah:start]")
      val txt = TextBuilder.fromPhrase(linkSamePhrase)(urlMap1, lang)

      txt.items.length shouldBe 6
      txt.items(1) shouldBe Link("https://www.bbc.co.uk", "Blah Blah", false)
      txt.items(3) shouldBe Link("dummy-path/blah", "Blah Blah", false)
      txt.items(5) shouldBe Link("dummy-path/start", "Blah Blah", false)

      val altTxt = TextBuilder.fromPhraseWithOptionalHint(linkSamePhrase)(urlMap1, lang)._1

      altTxt.items.length shouldBe 6
      altTxt.items(1) shouldBe Link("https://www.bbc.co.uk", "Blah Blah", false)
      altTxt.items(3) shouldBe Link("dummy-path/blah", "Blah Blah", false)
      altTxt.items(5) shouldBe Link("dummy-path/start", "Blah Blah", false)
    }

    "Convert all link-tab placeholders into appropriate Link objects" in new Test {
      val linkTabPhrase = Phrase("Hello [link-tab:Blah Blah:https://www.bbc.co.uk] [link-tab:Blah Blah:5] [link-tab:Blah Blah:start]",
                                 "Welsh, Hello [link-tab:Blah Blah:https://www.bbc.co.uk] [link-tab:Blah Blah:5] [link-tab:Blah Blah:start]")
      val txt = TextBuilder.fromPhrase(linkTabPhrase)(urlMap1, lang)

      txt.items.length shouldBe 6
      txt.items(1) shouldBe Link("https://www.bbc.co.uk", "Blah Blah", true)
      txt.items(3) shouldBe Link("dummy-path/blah", "Blah Blah", true)
      txt.items(5) shouldBe Link("dummy-path/start", "Blah Blah", true)

      val altTxt = TextBuilder.fromPhraseWithOptionalHint(linkTabPhrase)(urlMap1, lang)._1

      altTxt.items.length shouldBe 6
      altTxt.items(1) shouldBe Link("https://www.bbc.co.uk", "Blah Blah", true)
      altTxt.items(3) shouldBe Link("dummy-path/blah", "Blah Blah", true)
      altTxt.items(5) shouldBe Link("dummy-path/start", "Blah Blah", true)
    }

    "Flatten all placeholders within a line of text to include the text only" in new Test {
      val txt = "Welsh, Hello [link-tab:An http link:https://www.bbc.co.uk] [link-tab:link to stanza 5:5] [link-tab:link to beginning of guidance:start]"
      val expected = List("Welsh, Hello ", "An http link", " ", "link to stanza 5", " ", "link to beginning of guidance")
      TextBuilder.flattenPlaceholders(txt) shouldBe expected
    }
  }

  "TextBuilder expandLabels" must {
    val labelsMap = Map(
      "BLAH"->ScalarLabel("BLAH", List("33.5")),
      "SomeDate"->ScalarLabel("SomeDate", List("22/9/1973")))
    val labels = LabelCache(labelsMap)

    "Convert label reference with default output format placeholders within phrase" in new Test {
      val phrase = Phrase("""Sentence with a [label:BLAH] label reference""", """Welsh, Sentence with a [label:BLAH] label reference""")
      val expectedPhrase = Phrase("""Sentence with a 33.5 label reference""", """Welsh, Sentence with a 33.5 label reference""")
      TextBuilder.expandLabels(phrase, labels) shouldBe expectedPhrase
    }

    "Convert label reference with currency output format placeholders within phrase" in new Test {
      val phrase = Phrase("""Sentence with a [label:BLAH:currency] label reference""", """Welsh, Sentence with a [label:BLAH:currency] label reference""")
      val expectedPhrase = Phrase("""Sentence with a £33.50 label reference""", """Welsh, Sentence with a £33.50 label reference""")
      TextBuilder.expandLabels(phrase, labels) shouldBe expectedPhrase
    }

    "Convert label reference with date output format placeholders within phrase" in new Test {
      val phrase = Phrase("""Sentence with a [label:SomeDate:date] label reference""", """Welsh, Sentence with a [label:SomeDate:date] label reference""")
      val expectedPhrase = Phrase("""Sentence with a 22 September 1973 label reference""", """Welsh, Sentence with a 22 September 1973 label reference""")
      TextBuilder.expandLabels(phrase, labels) shouldBe expectedPhrase
    }

    "Convert a label placeholder within a bold placeholder to a bold label ref" in new Test {
      val phrase = Phrase("""Sentence with a [bold:[label:BLAH]] label reference""", """Welsh, Sentence with a [bold:[label:BLAH]] label reference""")
      val expectedPhrase = Phrase("""Sentence with a [bold:33.5] label reference""", """Welsh, Sentence with a [bold:33.5] label reference""")
      TextBuilder.expandLabels(phrase, labels) shouldBe expectedPhrase
    }

    "Convert a label placeholder with currency output format within a bold placeholder" in new Test {
      val phrase = Phrase("""Sentence with a [bold:[label:BLAH:currencyPoundsOnly]] label reference""", """Welsh, Sentence with a [bold:[label:BLAH:currencyPoundsOnly]] label reference""")
      val expectedPhrase = Phrase("""Sentence with a [bold:£33] label reference""", """Welsh, Sentence with a [bold:£33] label reference""")
      TextBuilder.expandLabels(phrase, labels) shouldBe expectedPhrase
    }

  }

  "TextBuilder.fromPhraseWithOptionalHint" must {
    "Return hint when present" in new Test {
      val (mainText, optionalHint): (Text, Option[Text]) = TextBuilder.fromPhraseWithOptionalHint(answerWithHint)
       mainText shouldBe Text("Yes")
       optionalHint shouldBe Some(Text("You agree with the assertion"))
    }

    "Return no hint when not present" in new Test {
      val (mainText, optionalHint): (Text, Option[Text]) = TextBuilder.fromPhraseWithOptionalHint(answerWithNoHint)
       mainText shouldBe Text("Yes")
       optionalHint shouldBe None
    }
  }
}
