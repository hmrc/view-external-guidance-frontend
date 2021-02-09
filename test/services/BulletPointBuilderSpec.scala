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

import core.services._
import base.BaseSpec
import core.models.ocelot._
import core.models.ocelot.stanzas._
import models.ocelot.stanzas._
import core.models.StanzaHelper

class BulletPointBuilderSpec extends BaseSpec with ProcessJson with StanzaHelper {

  def asString(elements: Seq[String]): String = elements.mkString

  // Define instance of class used in testing
  val pageBuilder: PageBuilder = new PageBuilder(new Timescales)

  case object DummyStanza extends Stanza {
    override val next: Seq[String] = Seq("1")
  }

  def createInstructionGroup(text1: String, text2: String): InstructionGroup = {

    val phrase1: Phrase = Phrase(Vector(text1, s"$welshPrefix $text1"))
    val phrase2: Phrase = Phrase(Vector(text2, s"$welshPrefix $text2"))

    val instruction1: Instruction = Instruction(phrase1, Seq("2"), None, stack = true)
    val instruction2: Instruction = Instruction(phrase2, Seq("3"), None, stack = true)

    InstructionGroup(Seq(instruction1, instruction2))
  }

  val welshPrefix: String = "Welsh - "

  "BulletPointBuilder bold text annotation removal processing" must {

    "Manage  a blank text string" in {

      val text = ""

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe text
    }

    "Return text unchanged when no bold text present" in {

      val text: String = "Today the weather is fine"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe text
    }

    "Return bold text only when normal text is not defined" in {

      val text: String = "[bold:Important]"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "Important"
    }

    "Return both normal and bold text for combination of leading text followed by bold text" in {

      val text: String = "This is [bold:Important]"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "This is Important"
    }

    "Return both normal text and bold text for combination of leading bold text followed by normal text" in {

      val text: String = "[bold:Important] do not do this"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "Important do not do this"
    }

    "Return both normal and bold text for text with single embedded bold text" in {

      val text: String = "Hello from [bold:Team Ocelot] in Greenland"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "Hello from Team Ocelot in Greenland"
    }

    "Return both normal and bold text with normal text embedded in bold text" in {

      val text: String = "[bold:Greetings from] our home in lovely [bold:Nova Scotia]"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "Greetings from our home in lovely Nova Scotia"
    }

    "Return both normal and bold text from mixed text starting with normal text" in {

      val text: String = "Today is [bold:Wednesday 10th May] and tomorrow is [bold:Thursday 11th May]"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "Today is Wednesday 10th May and tomorrow is Thursday 11th May"
    }

    "Return both normal and bold text from mixed text staring with bold text" in {

      val text: String = "[bold:Here and now] we must all [bold:try] to be calm"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "Here and now we must all try to be calm"
    }
  }

  "BulletPointBuilder link text annotation removal processing" must {

    "Manage a blank text string" in {

      val text = ""

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe text
    }

    "Return text unchanged when no link text present" in {

      val text: String = "Today the weather is fine"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe text
    }

    "Return link text only when normal text is not defined" in {

      val text: String = "[link:View options:https://mydomain/options]"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "View options"
    }

    "Return both normal and link text for combination of leading text followed by link text" in {

      val text: String = "View instructions for [link:mending a broken axle:http://mechanicsAreUs/axles]"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "View instructions for mending a broken axle"
    }

    "Return both normal and link text for combination of leading text followed by button text" in {

      val text: String = "View instructions for [button:mending a broken axle:http://mechanicsAreUs/axles]"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "View instructions for mending a broken axle"
    }

    "Return both normal and link text for combination of leading text followed by link-tab text" in {

      val text: String = "View instructions for [link-tab:mending a broken axle:http://mechanicsAreUs/axles]"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "View instructions for mending a broken axle"
    }

    "Return both normal text and link text for combination of leading link text followed by normal text" in {

      val text: String = "[link:Click here:https://my.com/details] for information"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "Click here for information"
    }

    "Return both normal and link text for text with single embedded link" in {

      val text: String = "For details [link:click here:https://info.co.uk/details] and follow the instructions shown"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "For details click here and follow the instructions shown"
    }

    "Return both normal and link text for text with single embedded button" in {

      val text: String = "For details [button:click here:https://info.co.uk/details] and follow the instructions shown"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "For details click here and follow the instructions shown"
    }

    "Return both normal and link text for text with single embedded link-tab" in {

      val text: String = "For details [link-tab:click here:https://info.co.uk/details] and follow the instructions shown"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "For details click here and follow the instructions shown"
    }

    "Return both normal and button text with normal text embedded in links" in {

      val text: String = "[button:Link 1 text:http://link1] and [button:link 2 text:https://link2]"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "Link 1 text and link 2 text"
    }

    "Return both normal and link-tab text with normal text embedded in links" in {

      val text: String = "[link-tab:Link 1 text:http://link1] and [link-tab:link 2 text:https://link2]"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "Link 1 text and link 2 text"
    }

    "Return both normal and link text with normal text embedded in links" in {

      val text: String = "[link:Link 1 text:http://link1] and [link:link 2 text:https://link2]"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "Link 1 text and link 2 text"
    }

    "Return both normal and link text from mixed text starting with normal text" in {

      val text: String = "Today is [link:Wednesday 10th May:http://my.com/calendar] and tomorrow is [link:Thursday 11th May:http://my.com/calendar]"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "Today is Wednesday 10th May and tomorrow is Thursday 11th May"
    }

    "Return both normal and button text from mixed text starting with normal text" in {

      val text: String = "Today is [link:Wednesday 10th May:http://my.com/calendar] and tomorrow is [link:Thursday 11th May:http://my.com/calendar]"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "Today is Wednesday 10th May and tomorrow is Thursday 11th May"
    }

    "Return both normal and link-tab text from mixed text starting with normal text" in {

      val text: String = "Today is [link-tab:Wednesday 10th May:http://my.com/calendar] and tomorrow is [link-tab:Thursday 11th May:http://my.com/calendar]"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "Today is Wednesday 10th May and tomorrow is Thursday 11th May"
    }

    "Return both normal and link text from mixed text staring with link" in {

      val text: String = "[link:Here and now:http://thisyear/today] we must all [link:try:https://explain] to be calm"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "Here and now we must all try to be calm"
    }

    "Return both normal and button text from mixed text staring with link" in {

      val text: String = "[button:Here and now:http://thisyear/today] we must all [button:try:https://explain] to be calm"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "Here and now we must all try to be calm"
    }

    "Return both normal and link-tab text from mixed text staring with link" in {

      val text: String = "[link-tab:Here and now:http://thisyear/today] we must all [link-tab:try:https://explain] to be calm"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "Here and now we must all try to be calm"
    }

    "Return correct text with back to back links" in {

      val text: String = "This should [link:be interesting:https://my.com/interesting?part=2] [link:and informative:http://my.com/inform]"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "This should be interesting and informative"
    }

    "Return correct text with back to back buttons" in {

      val text: String = "This should [button:be interesting:https://my.com/interesting?part=2] [button:and informative:http://my.com/inform]"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "This should be interesting and informative"
    }

    "Return correct text with back to back link-tabs" in {

      val text: String = "This should [link-tab:be interesting:https://my.com/interesting?part=2] [link-tab:and informative:http://my.com/inform]"

      asString(TextBuilder.flattenPlaceholders(text)) shouldBe "This should be interesting and informative"
    }

  }

  "Bullet point builder identification of bullet point list leading text" must {

    "Identify leading text in simple sentences" in {

      val text1: String = "Types of fruit you can buy: apples"
      val text2: String = "Types of fruit you can buy: oranges"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe "Types of fruit you can buy:"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe s"$welshPrefix Types of fruit you can buy:"
    }

    "Identify leading text in sentences starting with bold text" in {

      val text1: String = "[bold:Types of automobile] you can buy saloon"
      val text2: String = "[bold:Types of automobile] you can buy sports utility vehicle"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe "[bold:Types of automobile] you can buy"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe s"$welshPrefix [bold:Types of automobile] you can buy"
    }

    "Identify leading text in complex sentences" in {

      val text1: String =
        "The property allowance lets you earn up to \u00a311,000 in rental income, tax free, in each tax year. For example: renting a flat or house"
      val text2: String =
        "The property allowance lets you earn up to \u00a311,000 in rental income, tax free, in each tax year. For example: renting out a room in your home"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe
        "The property allowance lets you earn up to \u00a311,000 in rental income, tax free, in each tax year. For example: renting"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe
        s"$welshPrefix The property allowance lets you earn up to \u00a311,000 in rental income, tax free, in each tax year. For example: renting"
    }

    "Identify leading text is sentences where the leading text ends with bold text" in {

      // Note the final space after the bold text can be ignored

      val text1: String = "Things you might like [bold:TO DO] this very day"
      val text2: String = "Things you might like [bold:TO DO] on another day"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe "Things you might like [bold:TO DO]"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe s"$welshPrefix Things you might like [bold:TO DO]"
    }

    "Identify leading text in sentences where the leading text contains bold text items embedded in normal text" in {

      val text1: String = "Things [bold:to do] on sunny [bold:days] in the winter season"
      val text2: String = "Things [bold:to do] on sunny [bold:days] in the summer season"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe "Things [bold:to do] on sunny [bold:days] in the"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe s"$welshPrefix Things [bold:to do] on sunny [bold:days] in the"
    }

    "Identify leading text in sentences where the leading text contains normal text embedded in bold text" in {

      val text1: String = "[bold:How long] must we [bold:continue to] be [bold:stuck in] mud"
      val text2: String = "[bold:How long] must we [bold:continue to] be [bold:stuck in] snow"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe "[bold:How long] must we [bold:continue to] be [bold:stuck in]"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe s"$welshPrefix [bold:How long] must we [bold:continue to] be [bold:stuck in]"
    }

    "Identify leading text in simple sentences with multiple spaces between some words" in {

      val text1: String = "Types of  fruit you  can buy: apples"
      val text2: String = "Types of  fruit you  can buy: oranges"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe "Types of  fruit you  can buy:"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe s"$welshPrefix Types of  fruit you  can buy:"
    }

    "Identify leading text in sentences starting with bold text with multiple spaces between some of the bold words" in {

      val text1: String = "[bold:Types of  automobile] you can buy saloon"
      val text2: String = "[bold:Types of  automobile] you can buy sports utility vehicle"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe "[bold:Types of  automobile] you can buy"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe s"$welshPrefix [bold:Types of  automobile] you can buy"
    }

    "Identify leading text in sentences starting with link text" in {

      val text1: String = "[link:Types of automobile:http://mydomain/cars] you can buy saloon"
      val text2: String = "[link:Types of automobile:http://mydomain/cars] you can buy sports utility vehicle"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe "[link:Types of automobile:http://mydomain/cars] you can buy"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe s"$welshPrefix [link:Types of automobile:http://mydomain/cars] you can buy"
    }

    "Identify leading text is sentences where the leading text ends with link text" in {

      // Note the final space after the bold text can be ignored

      val text1: String = "Things you might like [link:to consider buying:https://mydomain/products?catalog=books] this very day"
      val text2: String = "Things you might like [link:to consider buying:https://mydomain/products?catalog=books] on another day"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe
        "Things you might like [link:to consider buying:https://mydomain/products?catalog=books]"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe
        s"$welshPrefix Things you might like [link:to consider buying:https://mydomain/products?catalog=books]"
    }

    "Identify leading text in sentences where the leading text contains link text items embedded in normal text" in {

      val text1: String = "Things to do on [link:sunny:5] days [link:at school:http://mydomain/schools] in the winter season"
      val text2: String = "Things to do on [link:sunny:5] days [link:at school:http://mydomain/schools] in the summer season"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe
        "Things to do on [link:sunny:5] days [link:at school:http://mydomain/schools] in the"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe
        s"$welshPrefix Things to do on [link:sunny:5] days [link:at school:http://mydomain/schools] in the"
    }

    "Identify leading text in sentences where the leading text contains normal text embedded in link text" in {

      val text1: String =
        "[link:How long:https://mydomain/duration/epochs] must we [link:continue to:2] be [link:stuck in://http://www.stuck.com/stuck] muddy lanes"
      val text2: String =
        "[link:How long:https://mydomain/duration/epochs] must we [link:continue to:2] be [link:stuck in://http://www.stuck.com/stuck] snow covered mountains"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe
        "[link:How long:https://mydomain/duration/epochs] must we [link:continue to:2] be [link:stuck in://http://www.stuck.com/stuck]"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe
        s"$welshPrefix [link:How long:https://mydomain/duration/epochs] must we [link:continue to:2] be [link:stuck in://http://www.stuck.com/stuck]"
    }

    "Identify leading text in sentences starting with link text with multiple spaces between some of the words" in {

      val text1: String = "[link:Types of  automobile:5] you  can buy saloon"
      val text2: String = "[link:Types of  automobile:5] you  can buy sports utility vehicle"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe "[link:Types of  automobile:5] you  can buy"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe s"$welshPrefix [link:Types of  automobile:5] you  can buy"
    }

    "Identify leading text in sentences starting with leading text with both links and bold text" in {

      val text1: String = "Today is a [bold:good day] to enjoy [link:motor racing:http://mydomain/motor-racing] at Silverstone"
      val text2: String = "Today is a [bold:good day] to enjoy [link:motor racing:http://mydomain/motor-racing] at Hednesford Raceway"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe
        "Today is a [bold:good day] to enjoy [link:motor racing:http://mydomain/motor-racing] at"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe
        s"$welshPrefix Today is a [bold:good day] to enjoy [link:motor racing:http://mydomain/motor-racing] at"
    }

    "Identify leading text in sentences where leading text and trailing text are both bold" in {

      val text1: String = "[bold:Today is the first day in ][bold:May]"
      val text2: String = "[bold:Today is the first day in ][bold:July]"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe "[bold:Today is the first day in ]"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe s"$welshPrefix [bold:Today is the first day in ]"
    }

    "Identify leading text in sentences where leading text and trailing text are both in links" in {

      val text1: String = "[link:Today is the first day in :https://mydomain/calendar/today][link:May:https://nydomain/calendar/may]"
      val text2: String = "[link:Today is the first day in :https://mydomain/calendar/today][link:July:https://mydomain/calendar/july]"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe
        "[link:Today is the first day in :https://mydomain/calendar/today]"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe
        s"$welshPrefix [link:Today is the first day in :https://mydomain/calendar/today]"
    }

    "Identify leading text where text includes a bold section followed immediately by a non-white space character" in {

      val text1: String = "You can buy the [bold:following]: apples"
      val text2: String = "You can buy the [bold:following]: oranges"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe "You can buy the [bold:following]:"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe s"$welshPrefix You can buy the [bold:following]:"
    }

    "Identify leading text where text includes a bold section followed immediately by a non-white space character and then further texts" in {

      val text1: String = "You can [bold:buy], things such as, various antiques"
      val text2: String = "You can [bold:buy], things such as, various trinkets"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe "You can [bold:buy], things such as, various"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe s"$welshPrefix You can [bold:buy], things such as, various"
    }

    "Identify leading text where text includes both bold and link placeholders immediately followed by non-whitespace characters" in {

      val text1: String = "You can [bold:buy], if you like, anything at [link:the general store:https://mydomain/store], and sell it to your friends"
      val text2: String = "You can [bold:buy], if you like, anything at [link:the general store:https://mydomain/store], and sell it to your acquaintances"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe
        "You can [bold:buy], if you like, anything at [link:the general store:https://mydomain/store], and sell it to your"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe
        s"$welshPrefix You can [bold:buy], if you like, anything at [link:the general store:https://mydomain/store], and sell it to your"
    }

    "Identify leading text where text includes a placeholder immediately following none-whitespace text" in {

      val text1: String = "You can buy[bold:-categories] fruit"
      val text2: String = "You can buy[bold:-categories] vegetables"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe "You can buy[bold:-categories]"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe s"$welshPrefix You can buy[bold:-categories]"
    }

    "Identify leading text where text includes a placeholder immediately following none-whitespace text followed by further matching text" in {

      val text1: String = "You can buy[bold:-categories] fruit and veg: potato"
      val text2: String = "You can buy[bold:-categories] fruit and veg: parsnip"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe "You can buy[bold:-categories] fruit and veg:"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe s"$welshPrefix You can buy[bold:-categories] fruit and veg:"
    }

    "Identify leading text where text includes both bold and link placeholders with leading text" in {

      val text1: String = "You can buy[bold:-categories] fruit and vegetables[link:<link>:http://mydomain/fruitAndVeg] : potato"
      val text2: String = "You can buy[bold:-categories] fruit and vegetables[link:<link>:http://mydomain/fruitAndVeg] : parsnip"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe
        "You can buy[bold:-categories] fruit and vegetables[link:<link>:http://mydomain/fruitAndVeg] :"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe
        s"$welshPrefix You can buy[bold:-categories] fruit and vegetables[link:<link>:http://mydomain/fruitAndVeg] :"
    }

    "Identify leading text containing both leading and trailing text with respect to place holders" in {

      val text1: String = "Today please note[bold:(Important)] we are selling[link:<link>:http://mydomain/items/fruitAndVeg] such as pears and apples"
      val text2: String = "Today please note[bold:(Important)] we are selling[link:<link>:http://mydomain/items/fruitAndVeg] such as carrots and turnips"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe
        "Today please note[bold:(Important)] we are selling[link:<link>:http://mydomain/items/fruitAndVeg] such as"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe
        s"$welshPrefix Today please note[bold:(Important)] we are selling[link:<link>:http://mydomain/items/fruitAndVeg] such as"
    }

    "Identify leading text where text includes both leading and trailing text for a placeholder" in {

      val text1: String = "You can buy fruit and vegetables[link:<link>:http://mydomain/fruitAndVeg]: potato"
      val text2: String = "You can buy fruit and vegetables[link:<link>:http://mydomain/fruitAndVeg]: parsnip"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.english) shouldBe
        "You can buy fruit and vegetables[link:<link>:http://mydomain/fruitAndVeg]:"

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe
        s"$welshPrefix You can buy fruit and vegetables[link:<link>:http://mydomain/fruitAndVeg]:"
    }

    "Identify leading text where text includes both leading and trailing text for a placeholder and following text" in {

      val text1: String = "You can buy fruit and vegetables[link:<link>:http://mydomain/fruitAndVeg], such as, potatoes"
      val text2: String = "You can buy fruit and vegetables[link:<link>:http://mydomain/fruitAndVeg], such as, oranges"

      val instructionGroup: InstructionGroup = createInstructionGroup(text1, text2)

      BulletPointBuilder.determineMatchedLeadingText(instructionGroup, _.welsh) shouldBe
        s"$welshPrefix You can buy fruit and vegetables[link:<link>:http://mydomain/fruitAndVeg], such as,"
    }

    "Method locateTextsAndMatchesContainingLeadingText" must {

      "Handle so far theoretical case when no text or match components are present" in {

        val text1: String = "Today is a [bold:good day] to enjoy [link:motor racing:http://mydomain/motor-racing] at Silverstone"

        val (texts, matches) = TextBuilder.placeholderTxtsAndMatches(text1)

        // Test invocation
        val (wordsProcessed1, outputTexts1, outputMatches1) =
          BulletPointBuilder.locateTextsAndMatchesContainingLeadingText(2, List(), 0, List(), 0, texts, matches, 0)

        wordsProcessed1 shouldBe 0

        outputTexts1 shouldBe texts
        outputMatches1 shouldBe matches

        // Test invocation
        val (wordsProcessed2, outputTexts2, outputMatches2) =
          BulletPointBuilder.locateMatchesContainingLeadingText(2, List(), 0, List(), 0, texts, matches, 2)

        wordsProcessed2 shouldBe 2

        outputTexts2 shouldBe texts
        outputMatches2 shouldBe matches
      }

    }
  }

  "Method locateTextsAndMatchesContainingLeadingText" must {

    "Handle so far theoretical case when no text or match components are present" in {

      val text1: String = "Today is a [bold:good day] to enjoy [link:motor racing:http://mydomain/motor-racing] at Silverstone"

      val (texts, matches) = TextBuilder.placeholderTxtsAndMatches(text1)

      // Test invocation
      val (wordsProcessed1, outputTexts1, outputMatches1) =
        BulletPointBuilder.locateTextsAndMatchesContainingLeadingText(2, List(), 0, List(), 0, texts, matches, 0)

      wordsProcessed1 shouldBe 0

      outputTexts1 shouldBe texts
      outputMatches1 shouldBe matches

      // Test invocation
      val (wordsProcessed2, outputTexts2, outputMatches2) =
        BulletPointBuilder.locateMatchesContainingLeadingText(2, List(), 0, List(), 0, texts, matches, 2)

      wordsProcessed2 shouldBe 2

      outputTexts2 shouldBe texts
      outputMatches2 shouldBe matches
    }
  }

  "Bullet point list instruction match testing" must {

    "Not match instructions with no similar text" in {

      val firstInstructionText: String = "Good Morning"
      val secondInstructionText: String = "Buen día"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, true)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe false
    }

    "Not match instructions with two similar leading words" in {

      val firstInstructionText: String = "Today is Wednesday"
      val secondInstructionText: String = "Today is Thursday"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, true)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe false
    }

    "Match instructions with three similar leading words" in {

      val firstInstructionText: String = "I have bought: apples"
      val secondInstructionText: String = "I have bought: oranges"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, true)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe true
    }

    "Match instructions with multiple similar leading words" in {

      val firstInstructionText: String = "The road is long and winding over there"
      val secondInstructionText: String = "The road is long and winding and here"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, true)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe true
    }

    "Not match instructions with multiple similar leading words but different spacing between the second and third words" in {

      val firstInstructionText: String = "The road   is long and winding over there"
      val secondInstructionText: String = "The road is long and winding and here"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, true)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe false
    }

    "Not match instructions with two similar leading words in bold" in {

      val firstInstructionText: String = "[bold:Today is Monday]"
      val secondInstructionText: String = "[bold:Today is Thursday]"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, true)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe false
    }

    "Match instructions with three similar leading words in bold" in {

      val firstInstructionText: String = "[bold:I have bought: apples]"
      val secondInstructionText: String = "[bold:I have bought: oranges]"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, true)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe true
    }

    "Not match instructions with three similar leading words in bold but different spacings between the first and second words" in {

      val firstInstructionText: String = "[bold:I have bought: apples]"
      val secondInstructionText: String = "[bold:I  have bought: oranges]"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, true)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe false
    }

    "Not match instructions with two similar leading words one normal text and one bold" in {

      val firstInstructionText: String = "Today [bold:is Monday]"
      val secondInstructionText: String = "Today [bold:is Thursday]"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, false)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe false
    }

    "Match instructions with three similar leading words one normal text and two bold" in {

      val firstInstructionText: String = "Today [bold:is Monday] 1st"
      val secondInstructionText: String = "Today [bold:is Monday] 2nd"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, false)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe true
    }

    "Match instructions with multiple similar leading words with multiple sets of bold words" in {

      val firstInstructionText: String = "Today is [bold:Monday and] tomorrow will [bold:be Tuesday] 4th"
      val secondInstructionText: String = "Today is [bold:Monday and] tomorrow will [bold:be Tuesday] 7th"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, false)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe true
    }

    "Not match instructions with two similar leading words in links" in {

      val firstInstructionText: String = "[link:Today is Monday:http://mydomain/test]"
      val secondInstructionText: String = "[link:Today is Thursday:http://mydomain/test]"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, false)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe false
    }

    "Match instructions with link in leading text" in {

      val firstInstructionText: String = "[link:The news this: morning:https://mydomain/news/morning] Early riser fails to get up"
      val secondInstructionText: String = "[link:The news this: afternoon:https://mydomain/news/afternoon] Lunch goes missing"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, false)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe true
    }

    "Not match instructions with link in leading text but differing spaces between the second and third words" in {

      val firstInstructionText: String = "[link:The news  this: morning:https://mydomain/news/morning] Early riser fails to get up"
      val secondInstructionText: String = "[link:The news this: afternoon:https://mydomain/news/afternoon] Lunch goes missing"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, false)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe false
    }

    "Match instructions with links in trailing text" in {

      val firstInstructionText: String = "Today I bought some [link:oranges:http://mydomain/fruits/oranges]"
      val secondInstructionText: String = "Today I bought some [link:apples:http://mydomain/fruits/apples]"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, false)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe true
    }

    "Match instructions with complex leading and trailing text" in {

      val firstInstructionText: String =
        "Today [bold: I bought] some [link:fruits:http://mydomain/fruits] at [bold:Stafford Fruit Market] see [link:Staffordshire markets:https://mydomain/markets/staffordshire]"
      val secondInstructionText: String =
        "Today [bold: I bought] some [link:fruits:http://mydomain/fruits] at [bold:Shrewsbury Market] see [link:Shropshire markets:https://mydomain/markets/shropshire]"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, false)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe true
    }

    "Match test instructions from Json prototype 1" in {

      val firstInstructionText: String =
        "The property allowance lets you earn up to \u00a311,000 in rental income, tax free, in each tax year. For example: renting a flat or house"
      val secondInstructionText: String =
        "The property allowance lets you earn up to \u00a311,000 in rental income, tax free, in each tax year. For example: renting out a room in your home"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, false)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe true
    }

    "Match test instructions from Json prototype 2" in {

      val firstInstructionText: String =
        "In some circumstances, you do not have to tell HMRC about extra income you've made. In each tax year you can earn up to £11,000, tax free, if you are: selling goods or services (trading)"
      val secondInstructionText: String =
        "In some circumstances, you do not have to tell HMRC about extra income you've made. In each tax year you can earn up to £11,000, tax free, if you are: renting land or property"

      val firstInstructionPhrase: Phrase = Phrase(firstInstructionText, "")
      val secondInstructionPhrase: Phrase = Phrase(secondInstructionText, "")

      val i1: Instruction = new Instruction(firstInstructionPhrase, Nil, None, false)
      val i2: Instruction = new Instruction(secondInstructionPhrase, Nil, None, false)

      BulletPointBuilder.matchInstructions(i1, i2) shouldBe true
    }
  }

  "Bullet point list instruction stacking" must {

    val phrase1: Phrase = Phrase(Vector("My favourite sweets are wine gums", "Welsh - My favourite sweets are wine gums"))
    val phrase2: Phrase = Phrase(Vector("My favourite sweets are porkie percys", "Welsh - My favourite sweets are porkie percys"))
    val phrase3: Phrase = Phrase(Vector("My favourite sweets are lemon bon bons", "Welsh - My favourite sweets are lemon bon bons"))

    val pageBuilder: PageBuilder = new PageBuilder(new Timescales)

    "Create two separate instructions for two instruction stanzas where both stanzas disable stacking" in {

      val instructionStanza1: InstructionStanza = InstructionStanza(0, Seq("2"), None, stack = false)
      val instructionStanza2: InstructionStanza = InstructionStanza(1, Seq("end"), None, stack = false)

      val flow = Map(
        Process.StartStanzaId -> PageStanza("/list", Seq("1"), stack = false),
        "1" -> instructionStanza1,
        "2" -> instructionStanza2,
        "end" -> EndStanza
      )

      val process: Process =
        Process(metaSection, flow, Vector[Phrase](phrase1, phrase2), Vector[Link]())

      pageBuilder.pagesWithValidation(process) match {

        case Right(pages) =>

          assert(pages.head.stanzas.size == 4)

          val visualStanzas = pages.head.stanzas.collect{case s:VisualStanza => s}
          val stanzas: Seq[VisualStanza] = BulletPointBuilder.groupBulletPointInstructions(Nil)(visualStanzas)

          stanzas(0) shouldBe Instruction(instructionStanza1, phrase1, None, Nil)
          stanzas(1) shouldBe Instruction(instructionStanza2, phrase2, None, Nil)

        case Left(err) => fail(s"Flow error $err")
      }
    }

    "Create two separate instructions for two instruction stanzas where the first stanza enables stacking and the second stanza disables stacking" in {

      val instructionStanza1: InstructionStanza = InstructionStanza(0, Seq("2"), None, stack = true)
      val instructionStanza2: InstructionStanza = InstructionStanza(1, Seq("end"), None, stack = false)

      val flow = Map(
        Process.StartStanzaId -> PageStanza("/list", Seq("1"), stack = false),
        "1" -> instructionStanza1,
        "2" -> instructionStanza2,
        "end" -> EndStanza
      )

      val process: Process =
        Process(metaSection, flow, Vector[Phrase](phrase1, phrase2), Vector[Link]())

      pageBuilder.pagesWithValidation(process) match {

        case Right(pages) =>

          assert(pages.head.stanzas.size == 4)

          val visualStanzas = pages.head.stanzas.collect{case s:VisualStanza => s}
          val stanzas: Seq[VisualStanza] = BulletPointBuilder.groupBulletPointInstructions(Nil)(visualStanzas)

          stanzas(0) shouldBe Instruction(instructionStanza1, phrase1, None, Nil)
          stanzas(1) shouldBe Instruction(instructionStanza2, phrase2, None, Nil)

        case Left(err) => fail(s"Flow error $err")
      }
    }

    "Create instruction group for two instruction stanzas where the first stanza disables stacking and the second stanza enables stacking" in {

      val instructionStanza1: InstructionStanza = InstructionStanza(0, Seq("2"), None, stack = false)
      val instructionStanza2: InstructionStanza = InstructionStanza(1, Seq("end"), None, stack = true)

      val flow = Map(
        Process.StartStanzaId -> PageStanza("/list", Seq("1"), stack = false),
        "1" -> instructionStanza1,
        "2" -> instructionStanza2,
        "end" -> EndStanza
      )

      val process: Process =
        Process(metaSection, flow, Vector[Phrase](phrase1, phrase2), Vector[Link]())

      pageBuilder.pagesWithValidation(process) match {

        case Right(pages) =>

          assert(pages.head.stanzas.size == 4)

          val visualStanzas = pages.head.stanzas.collect{case s:VisualStanza => s}
          val stanzas: Seq[VisualStanza] = BulletPointBuilder.groupBulletPointInstructions(Nil)(visualStanzas)

          val instruction1: Instruction = Instruction(instructionStanza1, phrase1, None, Nil)
          val instruction2: Instruction = Instruction(instructionStanza2, phrase2, None, Nil)

          val expectedInstructionGroup: InstructionGroup = InstructionGroup(Seq(instruction1, instruction2))

          stanzas(0) shouldBe expectedInstructionGroup

        case Left(err) => fail(s"Flow error $err")
      }
    }

    "Create instruction group for two instruction stanzas where both stanzas enable stacking" in {

      val instructionStanza1: InstructionStanza = InstructionStanza(0, Seq("2"), None, stack = true)
      val instructionStanza2: InstructionStanza = InstructionStanza(1, Seq("end"), None, stack = true)

      val flow = Map(
        Process.StartStanzaId -> PageStanza("/list", Seq("1"), stack = false),
        "1" -> instructionStanza1,
        "2" -> instructionStanza2,
        "end" -> EndStanza
      )

      val process: Process =
        Process(metaSection, flow, Vector[Phrase](phrase1, phrase2), Vector[Link]())

      pageBuilder.pagesWithValidation(process) match {

        case Right(pages) =>

          assert(pages.head.stanzas.size == 4)

          val visualStanzas = pages.head.stanzas.collect{case s:VisualStanza => s}
          val stanzas: Seq[VisualStanza] = BulletPointBuilder.groupBulletPointInstructions(Nil)(visualStanzas)

          val instruction1: Instruction = Instruction(instructionStanza1, phrase1, None, Nil)
          val instruction2: Instruction = Instruction(instructionStanza2, phrase2, None, Nil)

          val expectedInstructionGroup: InstructionGroup = InstructionGroup(Seq(instruction1, instruction2))

          stanzas(0) shouldBe expectedInstructionGroup

        case Left(err) => fail(s"Flow error $err")
      }
    }

    "Create three separate instructions for three instruction stanzas with stacking properties : false, false, false" in {

      val instructionStanza1: InstructionStanza = InstructionStanza(0, Seq("2"), None, stack = false)
      val instructionStanza2: InstructionStanza = InstructionStanza(1, Seq("3"), None, stack = false)
      val instructionStanza3: InstructionStanza = InstructionStanza(2, Seq("end"), None, stack = false)

      val flow = Map(
        Process.StartStanzaId -> PageStanza("/list", Seq("1"), stack = false),
        "1" -> instructionStanza1,
        "2" -> instructionStanza2,
        "3" -> instructionStanza3,
        "end" -> EndStanza
      )

      val process: Process =
        Process(metaSection, flow, Vector[Phrase](phrase1, phrase2, phrase3), Vector[Link]())

      pageBuilder.pagesWithValidation(process) match {

        case Right(pages) =>

          assert(pages.head.stanzas.size == 5)

          val visualStanzas = pages.head.stanzas.collect{case s:VisualStanza => s}
          val stanzas: Seq[VisualStanza] = BulletPointBuilder.groupBulletPointInstructions(Nil)(visualStanzas)

          stanzas(0) shouldBe Instruction(instructionStanza1, phrase1, None, Nil)
          stanzas(1) shouldBe Instruction(instructionStanza2, phrase2, None, Nil)
          stanzas(2) shouldBe Instruction(instructionStanza3, phrase3, None, Nil)

        case Left(err) => fail(s"Flow error $err")
      }
    }

    "Create instruction group plus instruction for three instruction stanzas with stacking properties : false, true, false" in {

      val instructionStanza1: InstructionStanza = InstructionStanza(0, Seq("2"), None, stack = false)
      val instructionStanza2: InstructionStanza = InstructionStanza(1, Seq("3"), None, stack = true)
      val instructionStanza3: InstructionStanza = InstructionStanza(2, Seq("end"), None, stack = false)

      val flow = Map(
        Process.StartStanzaId -> PageStanza("/list", Seq("1"), stack = false),
        "1" -> instructionStanza1,
        "2" -> instructionStanza2,
        "3" -> instructionStanza3,
        "end" -> EndStanza
      )

      val process: Process =
        Process(metaSection, flow, Vector[Phrase](phrase1, phrase2, phrase3), Vector[Link]())

      pageBuilder.pagesWithValidation(process) match {

        case Right(pages) =>

          assert(pages.head.stanzas.size == 5)

          val visualStanzas = pages.head.stanzas.collect{case s:VisualStanza => s}
          val stanzas: Seq[VisualStanza] = BulletPointBuilder.groupBulletPointInstructions(Nil)(visualStanzas)

          val instruction1: Instruction = Instruction(instructionStanza1, phrase1, None, Nil)
          val instruction2: Instruction = Instruction(instructionStanza2, phrase2, None, Nil)

          val expectedInstructionGroup: InstructionGroup = InstructionGroup(Seq(instruction1, instruction2))

          stanzas(0) shouldBe expectedInstructionGroup
          stanzas(1) shouldBe Instruction(instructionStanza3, phrase3, None, Nil)

        case Left(err) => fail(s"Flow error $err")
      }
    }

    "Create instruction plus instruction group for three instruction stanzas with stacking properties : false, false, true" in {

      val instructionStanza1: InstructionStanza = InstructionStanza(0, Seq("2"), None, stack = false)
      val instructionStanza2: InstructionStanza = InstructionStanza(1, Seq("3"), None, stack = false)
      val instructionStanza3: InstructionStanza = InstructionStanza(2, Seq("end"), None, stack = true)

      val flow = Map(
        Process.StartStanzaId -> PageStanza("/list", Seq("1"), stack = false),
        "1" -> instructionStanza1,
        "2" -> instructionStanza2,
        "3" -> instructionStanza3,
        "end" -> EndStanza
      )

      val process: Process =
        Process(metaSection, flow, Vector[Phrase](phrase1, phrase2, phrase3), Vector[Link]())

      pageBuilder.pagesWithValidation(process) match {

        case Right(pages) =>

          assert(pages.head.stanzas.size == 5)

          val visualStanzas = pages.head.stanzas.collect{case s:VisualStanza => s}
          val stanzas: Seq[VisualStanza] = BulletPointBuilder.groupBulletPointInstructions(Nil)(visualStanzas)

          stanzas(0) shouldBe Instruction(instructionStanza1, phrase1, None, Nil)

          val instruction2: Instruction = Instruction(instructionStanza2, phrase2, None, Nil)
          val instruction3: Instruction = Instruction(instructionStanza3, phrase3, None, Nil)

          val expectedInstructionGroup: InstructionGroup = InstructionGroup(Seq(instruction2, instruction3))

          stanzas(1) shouldBe expectedInstructionGroup

        case Left(err) => fail(s"Flow error $err")
      }
    }

    "Create instruction group for three instruction stanzas with stacking properties : false, true, true" in {

      val instructionStanza1: InstructionStanza = InstructionStanza(0, Seq("2"), None, stack = false)
      val instructionStanza2: InstructionStanza = InstructionStanza(1, Seq("3"), None, stack = true)
      val instructionStanza3: InstructionStanza = InstructionStanza(2, Seq("end"), None, stack = true)

      val flow = Map(
        Process.StartStanzaId -> PageStanza("/list", Seq("1"), stack = false),
        "1" -> instructionStanza1,
        "2" -> instructionStanza2,
        "3" -> instructionStanza3,
        "end" -> EndStanza
      )

      val process: Process =
        Process(metaSection, flow, Vector[Phrase](phrase1, phrase2, phrase3), Vector[Link]())

      pageBuilder.pagesWithValidation(process) match {

        case Right(pages) =>

          assert(pages.head.stanzas.size == 5)

          val visualStanzas = pages.head.stanzas.collect{case s:VisualStanza => s}
          val stanzas: Seq[VisualStanza] = BulletPointBuilder.groupBulletPointInstructions(Nil)(visualStanzas)

          val instruction1: Instruction = Instruction(instructionStanza1, phrase1, None, Nil)
          val instruction2: Instruction = Instruction(instructionStanza2, phrase2, None, Nil)
          val instruction3: Instruction = Instruction(instructionStanza3, phrase3, None, Nil)

          val expectedInstructionGroup: InstructionGroup = InstructionGroup(Seq(instruction1, instruction2, instruction3))

          stanzas(0) shouldBe expectedInstructionGroup

        case Left(err) => fail(s"Flow error $err")
      }
    }

  }
    "Return ungrouped Instruction stanzas when text contents do not start with similar text" in {

      val instructionStanza1: InstructionStanza = InstructionStanza(0, Seq("2"), None, false)
      val instructionStanza2: InstructionStanza = InstructionStanza(1, Seq("end"), None, false)

      val phrase1: Phrase = Phrase(Vector("Some Text", "Welsh, Some Text"))
      val phrase2: Phrase = Phrase(Vector("Some Text1", "Welsh, Some Text1"))

      val flow = Map(
        Process.StartStanzaId -> PageStanza("/this", Seq("1"), false),
        "1" -> instructionStanza1,
        "2" -> instructionStanza2,
        "end" -> EndStanza
      )
      val process = Process(metaSection, flow, Vector[Phrase](phrase1, phrase2), Vector[Link]())

      pageBuilder.pagesWithValidation(process) match {
        case Right(pages) => {

          assert(pages.head.stanzas.size == 4)

          pages.head.stanzas(1) shouldBe Instruction(instructionStanza1, phrase1, None, Nil)
          pages.head.stanzas(2) shouldBe Instruction(instructionStanza2, phrase2, None, Nil)
        }
        case Left(err) => fail(s"Flow error $err")
      }
    }

    "Return grouped Instruction stanzas when text contents start with similar text" in {

      val instructionStanza1: InstructionStanza = InstructionStanza(0, Seq("2"), None, false)
      val instructionStanza2: InstructionStanza = InstructionStanza(1, Seq("3"), None, true)
      val instructionStanza3: InstructionStanza = InstructionStanza(2, Seq("end"), None, true)

      val phrase1: Phrase = Phrase(Vector("Today I bought some beetroot", "Welsh, Today I bought some beetroot"))
      val phrase2: Phrase = Phrase(Vector("Today I bought some carrots", "Welsh, Today I bought some carrots"))
      val phrase3: Phrase = Phrase(Vector("Today I bought some peppers", "Welsh, Today I bought some peppers"))

      val flow = Map(
        Process.StartStanzaId -> PageStanza("/this", Seq("1"), false),
        "1" -> instructionStanza1,
        "2" -> instructionStanza2,
        "3" -> instructionStanza3,
        "end" -> EndStanza
      )

      val process = Process(metaSection, flow, Vector[Phrase](phrase1, phrase2, phrase3), Vector[Link]())

      pageBuilder.pagesWithValidation(process) match {
        case Right(pages) => {

          assert(pages.head.stanzas.size == 5)

          // Construct expected instruction group stanza
          val instruction1: Instruction = Instruction(instructionStanza1, phrase1, None, Nil)
          val instruction2: Instruction = Instruction(instructionStanza2, phrase2, None, Nil)
          val instruction3: Instruction = Instruction(instructionStanza3, phrase3, None, Nil)

          val expectedInstructionGroup: InstructionGroup = InstructionGroup(Seq(instruction1, instruction2, instruction3))

          val visualStanzas = pages.head.stanzas.collect{case s:VisualStanza => s}
          BulletPointBuilder.groupBulletPointInstructions(Nil)(visualStanzas)(0) shouldBe expectedInstructionGroup
        }
        case Left(err) => fail(s"Flow error $err")
      }
    }

    "Correctly group instructions stanzas in a complex page" in {

      val calloutStanza1: CalloutStanza = CalloutStanza(Title, 0, Seq("2"), false)
      val calloutStanza2: CalloutStanza = CalloutStanza(SubTitle, four, Seq("6"), false)

      val instructionStanza1: InstructionStanza = InstructionStanza(1, Seq("3"), None, true)
      val instructionStanza2: InstructionStanza = InstructionStanza(2, Seq("4"), None, true)
      val instructionStanza3: InstructionStanza = InstructionStanza(3, Seq("5"), None, false)
      val instructionStanza4: InstructionStanza = InstructionStanza(five, Seq("7"), None, false)
      val instructionStanza5: InstructionStanza = InstructionStanza(six, Seq("9"), None, false)
      val instructionStanza6: InstructionStanza = InstructionStanza(seven, Seq("10"), None, false)
      val instructionStanza7: InstructionStanza = InstructionStanza(eight, Seq("end"), None, true)

      // Define phrases
      val phrase1: Phrase = Phrase(Vector("Main title", "Welsh, Main title"))
      val phrase2: Phrase = Phrase(Vector("My favourite sweets are Wine gums", "Welsh, My favourite sweets are Wine gums"))
      val phrase3: Phrase = Phrase(Vector("My favourite sweets are humbugs", "Welsh, My favourite sweets are humbugs"))
      val phrase4: Phrase = Phrase(Vector("Today is Monday", "Welsh, Today is Monday"))
      val phrase5: Phrase = Phrase(Vector("More news", "Welsh, More news"))
      val phrase6: Phrase = Phrase(Vector("Today in the West Midlands", "Welsh, Today in the West Midlands"))
      val phrase7: Phrase = Phrase(Vector("Late night in Brierly hill", "Welsh, Late night in Brierly hill"))
      val phrase8: Phrase = Phrase(Vector("What is happening in Dudley", "Welsh, What is happening in Dudley"))
      val phrase9: Phrase = Phrase(Vector("What is happening in Halesowen", "Welsh, What is happening in Halesowen"))

      val flow = Map(
        Process.StartStanzaId -> PageStanza("/this", Seq("1"), false),
        "1" -> calloutStanza1,
        "2" -> instructionStanza1,
        "3" -> instructionStanza2,
        "4" -> instructionStanza3,
        "5" -> calloutStanza2,
        "6" -> instructionStanza4,
        "7" -> ValueStanza(List(Value(ScalarType, "Region", "West Midlands")), Seq("8"), false),
        "8" -> instructionStanza5,
        "9" -> instructionStanza6,
        "10" -> instructionStanza7,
        "end" -> EndStanza
      )

      val process: Process =
        Process(metaSection, flow, Vector[Phrase](phrase1, phrase2, phrase3, phrase4, phrase5, phrase6, phrase7, phrase8, phrase9), Vector[Link]())

      pageBuilder.pagesWithValidation(process) match {
        case Right(pages) => {

          assert(pages.head.stanzas.size == 12)

          // Test expected instruction group stanzas
          val instruction1: Instruction = Instruction(instructionStanza1, phrase2, None, Nil)
          val instruction2: Instruction = Instruction(instructionStanza2, phrase3, None, Nil)

          val expectedInstructionGroup1: InstructionGroup = InstructionGroup(Seq(instruction1, instruction2))


          val visualStanzas = pages.head.stanzas.collect{case s:VisualStanza => s}
          BulletPointBuilder.groupBulletPointInstructions(Nil)(visualStanzas)(1) shouldBe expectedInstructionGroup1

          val instruction6: Instruction = Instruction(instructionStanza6, phrase8, None, Nil)
          val instruction7: Instruction = Instruction(instructionStanza7, phrase9, None, Nil)

          val expectedInstructionGroup2: InstructionGroup = InstructionGroup(Seq(instruction6, instruction7))

          BulletPointBuilder.groupBulletPointInstructions(Nil)(visualStanzas)(six) shouldBe expectedInstructionGroup2
        }
        case Left(err) => fail(s"Flow error $err")
      }

    }

}
