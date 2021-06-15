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

// val transformations: List[(String, String)] =  List(("'", "’"), (" - ", " – "))
  // val (regexList: List[String], updates: List[String]) = transformations.map(t = (s"(t._1)", t._2))
  // val regex: Regex = regexList.mkString("|").r
  // def toGDSString(s: String): String = regex.replaceAllIn(s, m =>
  //   updates.zipWithIndex.reduce

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

class BPSpec extends BaseSpec with ProcessJson with StanzaHelper {

  def asString(elements: Seq[String]): String = elements.mkString

  // Define instance of class used in testing
  val pageBuilder: PageBuilder = new PageBuilder(new Placeholders(new DefaultTodayProvider))

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

  def createPhraseGroup(text1: String, text2: String): Seq[Phrase] = {

    val phrase1: Phrase = Phrase(text1, s"$welshPrefix $text1")
    val phrase2: Phrase = Phrase(text2, s"$welshPrefix $text2")

    Seq(phrase1, phrase2)
  }

  def createPhraseGroup(texts: Seq[String]): Seq[Phrase] = texts.map(t => Phrase(t,  s"$welshPrefix $t"))

  val welshPrefix: String = "Welsh - "

  "Bullet point builder identification of bullet point list leading text" must {

    "Identify leading text in Trader Triage example with similar ending words you and your" in {

      val texts: Seq[String] = Seq(
        "You have told us that: you move goods into and out of Great Britain (England, Scotland or Wales)",
        "You have told us that: your goods will move to, from or through common transit countries",
        "You have told us that: you process goods inside Great Britain",
        "You have told us that: you do not have premises")

      val phraseGroup: Seq[Phrase] = createPhraseGroup(texts)

      BulletPointBuilder.determineMatchedLeadingText(phraseGroup, _.english) shouldBe
        "You have told us that:"

      BulletPointBuilder.determineMatchedLeadingText(phraseGroup, _.welsh) shouldBe
        s"$welshPrefix You have told us that:"
    }

  }

}
