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

import base.BaseSpec

class TextBuilderSpec extends BaseSpec {

  import TextBuilder._

  "TextBuilder.fragment" must {
    "Fragment an empty string into a list containing a PartialMatch of an empty string" in {
      TextBuilder.fragment("") shouldBe List(PartialMatch(""))
    }

    "Fragment a string containing a bold placeholder into a list containing a TotalMatch" in {
      TextBuilder.fragment("[bold:Some Text]") shouldBe List(TotalMatch("[bold:Some Text]", "Some Text"))
    }

    "Fragment a string containing a link placeholder into a list containing a TotalMatch" in {
      TextBuilder.fragment("[link:The BBC:https://www.bbc.co.uk]") shouldBe List(TotalMatch("[link:The BBC:https://www.bbc.co.uk]", "The BBC"))
    }

    "Fragment a complex string into a list of fragments" in {
      val fragments: List[Fragment] = TextBuilder.fragment("Here is a [bold:link] to the [link:The BBC:https://www.bbc.co.uk], click to access.")
      val expectedFragments: List[Fragment] = List(PartialMatch("Here is a "),
                                                   TotalMatch("[bold:link]", "link"),
                                                   PartialMatch(" to the "),
                                                   TotalMatch("[link:The BBC:https://www.bbc.co.uk]", "The BBC"),
                                                   PartialMatch(", click to access."))

      fragments shouldBe expectedFragments
    }

  }

  "TextBuilder.flattenFragments" must {
    "Extract a list of an empty string from a PartialMatch of an empty string" in {
      TextBuilder.flattenFragments(List(PartialMatch(""))) shouldBe List("")
    }


    "Translate a list of Partial and Total match fragments into a list of their text tokens" in {
      val fragments: List[Fragment] = List(PartialMatch("Here is a "),
                                           TotalMatch("[bold:link]", "link"),
                                           PartialMatch(" to the "),
                                           TotalMatch("[link:The BBC:https://www.bbc.co.uk]", "The BBC"),
                                           PartialMatch(", click to access."))

      TextBuilder.flattenFragments(fragments) shouldBe List("Here", "is", "a", "link", "to", "the", "The", "BBC,", "click", "to", "access.")
    }
  }

  "TextBuilder.join" must {
    "reconstitute a PartialMatch of an empty string into an empty string" in {
      TextBuilder.join(List(PartialMatch(""))) shouldBe ""
    }

    "reconstitute a phrase from its fragments" in {
      val fragments: List[Fragment] = List(PartialMatch("Here is a "),
                                           TotalMatch("[bold:link]", "link"),
                                           PartialMatch(" to the "),
                                           TotalMatch("[link:The BBC:https://www.bbc.co.uk]", "The BBC"),
                                           PartialMatch(", click to access."))

      TextBuilder.join(fragments) shouldBe "Here is a [bold:link] to the [link:The BBC:https://www.bbc.co.uk], click to access."
    }
  }


  "TextBuilder.matchFragments" must {
    "match identical phrases" in {
      val f1: List[Fragment] = TextBuilder.fragment("Hello, today is [bold:Thursday]")
      val f2: List[Fragment] = TextBuilder.fragment("Hello, today is [bold:Thursday]")
      val expectedTokens: List[String] = List("Hello,", "today", "is", "Thursday")
      val expectedFragments: List[Fragment] = List(PartialMatch("Hello, today is " ), TotalMatch("[bold:Thursday]","Thursday"))

      val result = TextBuilder.matchFragments(f1, f2)

      result._1 shouldBe expectedTokens
      result._2 shouldBe expectedFragments
    }

    "Find longest match" in {
      val f1: List[Fragment] = TextBuilder.fragment("Hello, today is [bold:Thursday]")
      val f2: List[Fragment] = TextBuilder.fragment("Hello, today is warm")
      val expected: (List[String], List[Fragment]) = (List("Hello,", "today", "is"),
                                                      List(PartialMatch("Hello, today is ")))

      TextBuilder.matchFragments(f1, f2) shouldBe expected
    }

    "Find longest match where splitting a PartitalMatch is required" in {
      val f1: List[Fragment] = TextBuilder.fragment("[bold:Thursday] is today, Hello")
      val f2: List[Fragment] = TextBuilder.fragment("[bold:Thursday] is today, Warm!!")

      f1 shouldBe List(PartialMatch(""), TotalMatch("[bold:Thursday]", "Thursday"), PartialMatch(" is today, Hello"))
      f2 shouldBe List(PartialMatch(""), TotalMatch("[bold:Thursday]", "Thursday"), PartialMatch(" is today, Warm!!"))

      val result = TextBuilder.matchFragments(f1, f2)

      result._1 shouldBe List("Thursday", "is", "today,")
      result._2 shouldBe List(PartialMatch(""), TotalMatch("[bold:Thursday]", "Thursday"), PartialMatch(" is today, "))
    }

  }
}