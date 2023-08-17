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

import base.BaseSpec
import core.models.ocelot._
import play.api.i18n.{Messages, MessagesApi}

class TextBuilderSpec extends BaseSpec {

  import TextBuilder._

  trait Test {
    val messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    implicit val messages: Messages = messagesApi.preferred(Seq())
    private def messagesFn(k: String, args: Seq[Any])(implicit messages: Messages): String = messages(k, args: _*)

    val labelsMap = List(
                      ScalarLabel("X", List("43")),
                      ScalarLabel("Y", List("L")),
                      ScalarLabel("Z", List("Today is Monday")),
                      ListLabel("L", List("1","2","3")),
                      ScalarLabel("When", List("22/9/1973")),
                      ScalarLabel("DollarValue", List("Hello$World$")),
                      ScalarLabel("BackslashValue", List("Hello\\World\\"))
                    ).map(l => (l.name -> l)).toMap
    val timescales = Map("RDelay" -> 23)
    val labels: Labels = LabelCache(labelsMap, Map(), Nil, Map(), timescales, messagesFn, runMode = Published)
  }

  "Label expansion" must {
    "expand simple label" in new Test {
      expandLabels("X = [label:X]", labels) shouldBe "X = 43"
    }

    "expand mutliple labels" in new Test {
      expandLabels("X = [label:X] and X in pounds = [label:X:currency]", labels) shouldBe "X = 43 and X in pounds = £43.00"
    }

    "expand list lengths" in new Test {
      expandLabels("L has length [list:L:length]", labels) shouldBe "L has length 3"
    }

    "expand list lengths with embedded label for list name" in new Test {
      expandLabels("X = [label:X] and L has length [list:[label:Y]:length]", labels) shouldBe "X = 43 and L has length 3"
    }

    "expand date add" in new Test {
      expandLabels("1/1/2022 plus RDelay days = [date_add:1/1/2022:RDelay]", labels) shouldBe "1/1/2022 plus RDelay days = 24/1/2022"
    }

    "expand date add with embedded Label" in new Test {
      expandLabels("1/1/2022 plus RDelay days = [date_add:When:RDelay]", labels) shouldBe "1/1/2022 plus RDelay days = 15/10/1973"
    }

    "expand date placeholder" in new Test {
      expandLabels("The year is [date:1/1/2022:year]", labels) shouldBe "The year is 2022"
      expandLabels("The month is [date:1/1/2022:month]", labels) shouldBe "The month is 1"
      expandLabels("The month_start is [date:1/1/2022:month_start]", labels) shouldBe "The month_start is 1/1/2022"
      expandLabels("The month_end is [date:1/1/2022:month_end]", labels) shouldBe "The month_end is 31/1/2022"
      expandLabels("The month_name is [date:1/1/2022:month_name]", labels) shouldBe "The month_name is January"
      expandLabels("The dow is [date:1/1/2022:dow]", labels) shouldBe "The dow is 6"
      expandLabels("The dow_name is [date:1/1/2022:dow_name]", labels) shouldBe "The dow_name is Saturday"
      expandLabels("The day is [date:1/1/2022:day]", labels) shouldBe "The day is 1"
    }

    "expand date placeholder with embedded Label" in new Test {
      expandLabels("The year is [date:[label:When]:year]", labels) shouldBe "The year is 1973"
      expandLabels("The month is [date:[label:When]:month]", labels) shouldBe "The month is 9"
      expandLabels("The month_start is [date:[label:When]:month_start]", labels) shouldBe "The month_start is 1/9/1973"
      expandLabels("The month_end is [date:[label:When]:month_end]", labels) shouldBe "The month_end is 30/9/1973"
      expandLabels("The month_name is [date:[label:When]:month_name]", labels) shouldBe "The month_name is September"
      expandLabels("The dow is [date:[label:When]:dow]", labels) shouldBe "The dow is 6"
      expandLabels("The dow_name is [date:[label:When]:dow_name]", labels) shouldBe "The dow_name is Saturday"
      expandLabels("The day is [date:[label:When]:day]", labels) shouldBe "The day is 22"
    }

    "expand label values containing dollar symbols" in new Test {
      expandLabels("This label values contains £ characters: [label:DollarValue]", labels) shouldBe "This label values contains £ characters: Hello$World$"
    }

    "expand label values containing dollar symbols within text containing dollar symbols" in new Test {
      expandLabels("This label $ values \\$ contains £ characters: [label:DollarValue]", labels) shouldBe "This label $ values \\$ contains £ characters: Hello$World$"
    }

    "expand label values containing backslash symbols" in new Test {
      expandLabels("This label values contains £ characters: [label:BackslashValue]", labels) shouldBe "This label values contains £ characters: Hello\\World\\"
    }

    "expand label values containing backslash symbols within text containing backslash symbols" in new Test {
      expandLabels("This label \\ values \\\\ contains £ characters: [label:BackslashValue]", labels) shouldBe "This label \\ values \\\\ contains £ characters: Hello\\World\\"
    }

  }

  "fragment" must {
    "Fragment an empty string into a list containing a PartialMatch of an empty string" in {
      fragment("") shouldBe List(PartialMatch(""))
    }

    "Fragment a string containing a bold placeholder into a list containing a TotalMatch" in {
      fragment("[bold:Some Text]") shouldBe List(TotalMatch("[bold:Some Text]", "Some Text"))
    }

    "Fragment a string containing a link placeholder into a list containing a TotalMatch" in {
      fragment("[link:The BBC:https://www.bbc.co.uk]") shouldBe List(TotalMatch("[link:The BBC:https://www.bbc.co.uk]", "The BBC"))
    }

    "Fragment a complex string into a list of fragments" in {
      val fragments: List[Fragment] = fragment("Here is a [bold:link] to the [link:The BBC:https://www.bbc.co.uk], click to access.")
      val expectedFragments: List[Fragment] = List(PartialMatch("Here is a "),
                                                   TotalMatch("[bold:link]", "link"),
                                                   PartialMatch(" to the "),
                                                   TotalMatch("[link:The BBC:https://www.bbc.co.uk]", "The BBC"),
                                                   PartialMatch(", click to access."))

      fragments shouldBe expectedFragments
    }

  }

  "flattenFragments" must {
    "Extract a list of an empty string from a PartialMatch of an empty string" in {
      flattenFragments(List(PartialMatch(""))) shouldBe List("")
    }


    "Translate a list of Partial and Total match fragments into a list of their text tokens" in {
      val fragments: List[Fragment] = List(PartialMatch("Here is a "),
                                           TotalMatch("[bold:link]", "link"),
                                           PartialMatch(" to the "),
                                           TotalMatch("[link:The BBC:https://www.bbc.co.uk]", "The BBC"),
                                           PartialMatch(", click to access."))

      flattenFragments(fragments) shouldBe List("Here", "is", "a", "link", "to", "the", "The", "BBC,", "click", "to", "access.")
    }
  }

  "join" must {
    "reconstitute a PartialMatch of an empty string into an empty string" in {
      join(List(PartialMatch(""))) shouldBe ""
    }

    "reconstitute a phrase from its fragments" in {
      val fragments: List[Fragment] = List(PartialMatch("Here is a "),
                                           TotalMatch("[bold:link]", "link"),
                                           PartialMatch(" to the "),
                                           TotalMatch("[link:The BBC:https://www.bbc.co.uk]", "The BBC"),
                                           PartialMatch(", click to access."))

      join(fragments) shouldBe "Here is a [bold:link] to the [link:The BBC:https://www.bbc.co.uk], click to access."
    }
  }


  "matchFragments" must {
    "match identical phrases" in {
      val f1: List[Fragment] = fragment("Hello, today is [bold:Thursday]")
      val f2: List[Fragment] = fragment("Hello, today is [bold:Thursday]")
      val expectedTokens: List[String] = List("Hello,", "today", "is", "Thursday")
      val expectedFragments: List[Fragment] = List(PartialMatch("Hello, today is " ), TotalMatch("[bold:Thursday]","Thursday"))

      val result = matchFragments(f1, f2)

      result._1 shouldBe expectedTokens
      result._2 shouldBe expectedFragments
    }

    "Find longest match" in {
      val f1: List[Fragment] = fragment("Hello, today is [bold:Thursday]")
      val f2: List[Fragment] = fragment("Hello, today is warm")
      val expected: (List[String], List[Fragment]) = (List("Hello,", "today", "is"),
                                                      List(PartialMatch("Hello, today is ")))

      matchFragments(f1, f2) shouldBe expected
    }

    "Find longest match where splitting a PartitalMatch is required" in {
      val f1: List[Fragment] = fragment("[bold:Thursday] is today, Hello")
      val f2: List[Fragment] = fragment("[bold:Thursday] is today, Warm!!")

      f1 shouldBe List(PartialMatch(""), TotalMatch("[bold:Thursday]", "Thursday"), PartialMatch(" is today, Hello"))
      f2 shouldBe List(PartialMatch(""), TotalMatch("[bold:Thursday]", "Thursday"), PartialMatch(" is today, Warm!!"))

      val result = matchFragments(f1, f2)

      result._1 shouldBe List("Thursday", "is", "today,")
      result._2 shouldBe List(PartialMatch(""), TotalMatch("[bold:Thursday]", "Thursday"), PartialMatch(" is today, "))
    }

  }
}