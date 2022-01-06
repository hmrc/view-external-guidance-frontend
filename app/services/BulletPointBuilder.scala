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

import core.models.ocelot.Phrase
import models.ui.Text
import scala.util.matching.Regex

object BulletPointBuilder {
  val NotSpaceRegex: Regex = """([^ ]+)""".r
  val MatchLimitEnglish: Int = 3
  val MatchLimitWelsh: Int = 1
  val Break: String = s"[break]"
  val BreakPattern: String = s"\\[break\\]"

  def leadingAndBulletText(phrases: Seq[Phrase])(implicit ctx: UIContext): (Text, Seq[Text]) = {
    def bulletPoints(enLength: Int, cyLength: Int, ps: Seq[Phrase]): Seq[Text] =
      ps.map(p => TextBuilder.fromPhrase(Phrase(p.english.drop(enLength).trim, p.welsh.drop(cyLength).trim)))

    def breakSplit(ps: Seq[Phrase]): (Text, Seq[Text]) = {
      val en: String = ps.head.english.take(ps.head.english.indexOf(Break))
      val cy: String = ps.head.welsh.take(ps.head.welsh.indexOf(Break))
      val cleaned: Seq[Phrase] = ps.map(p => Phrase(p.english.replaceFirst(BreakPattern, ""), p.welsh.replaceFirst(BreakPattern, "")))

      (TextBuilder.fromPhrase(Phrase(en, cy)), bulletPoints(en.length, cy.length, cleaned))
    }

    def standardSplit(ps: Seq[Phrase]): (Text, Seq[Text]) = {
      val en: String = BulletPointBuilder.findLeadingText(ps, _.english)
      val cy: String = BulletPointBuilder.findLeadingText(ps, _.welsh)
      (TextBuilder.fromPhrase(Phrase(en, cy)), bulletPoints(en.length, cy.length, ps))
    }

    if (phrases.head.english.contains(Break)) breakSplit(phrases) else standardSplit(phrases)
  }

  private[services] def findLeadingText(phrases: Seq[Phrase], phraseText: Phrase => String): String = {
    val matched: List[List[TextBuilder.Fragment]] = phrases.headOption.fold[List[List[TextBuilder.Fragment]]](Nil){first =>
      phrases.toList.tail.map(p => partialMatchText(phraseText(first), phraseText(p))._2)
    }
    matched.headOption.fold(""){_ =>
      TextBuilder.join(matched.reduce((fs1, fs2) => if (fs1.map(_.size).sum < fs2.map(_.size).sum) fs1 else fs2)).trim
    }
  }

  private[services] def matchPhrases(p1: Phrase, p2: Phrase): Boolean = {
    def breakMatch(text1: String, text2: String): Boolean =
      (text1.indexOf(Break), text2.indexOf(Break)) match {
        case (idx1, idx2) if idx1 <= 0 || idx2 <= 0 => false
        case (idx1, idx2) if idx1 == idx2 => text1.take(idx1) == text2.take(idx2)
        case _ => false
      }

    def standardMatch(text1: String, text2: String, matchLimit: Int): Boolean = partialMatchText(text1, text2)._1.size >= matchLimit

    // If any text component of the two phrases contains the break marker apply break matching
    if (p1.equals(p2)) false
    else if(useBreakMatch(p1, p2)) breakMatch(p1.english, p2.english) && breakMatch(p1.welsh, p2.welsh)
    else standardMatch(p1.english, p2.english, MatchLimitEnglish) && standardMatch(p1.welsh, p2.welsh, MatchLimitWelsh)
  }

  private[services] def useBreakMatch(p1: Phrase, p2: Phrase): Boolean =
    p1.english.contains(Break) || p1.welsh.contains(Break) || p2.english.contains(Break) || p2.welsh.contains(Break)

  private def partialMatchText(text1: String, text2: String): (List[String], List[TextBuilder.Fragment]) =
    TextBuilder.matchFragments(TextBuilder.fragment(text1), TextBuilder.fragment(text2))

  private[services] final def groupMatchingPhrases(phrases: Seq[Phrase]): List[List[Phrase]] =
    phrases.foldRight[List[List[Phrase]]](Nil){ (p, acc) =>
      acc match {
        case Nil => List(List(p))
        case current :: _ if matchPhrases(p, current.head) => (p :: current) :: acc.tail
        case _ => List(List(p)) ::: acc
      }
    }
}
