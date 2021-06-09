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

import core.models.ocelot.Phrase
import scala.util.matching.Regex

object BulletPointBuilder {
  val NotSpaceRegex: Regex = """([^ ]+)""".r
  val MatchLimitEnglish: Int = 3
  val MatchLimitWelsh: Int = 1
  val Break: String = "break"
  val ExplicitBreak: String = s"[$Break]"
  val BreakMatchPattern: String = s"\\[$Break\\]"

  def determineMatchedLeadingText(phrases: Seq[Phrase], phraseText: Phrase => String): String = {
    val matched: List[List[TextBuilder.Fragment]] = phrases.headOption.fold[List[List[TextBuilder.Fragment]]](Nil){first =>
      phrases.toList.tail.map(p => partialMatchText(phraseText(first), phraseText(p))._2)
    }
    matched.headOption.fold("")(_ => TextBuilder.join(matched.reduce((x, y) => if (x.length < y.length) x else y)).trim)
  }

  private[services] def matchPhrases(p1: Phrase, p2: Phrase): Boolean = {
    def explicitMatch(text1: String, text2: String): Boolean =
      if (text1 == text2) false
      else (text1.indexOf(ExplicitBreak), text2.indexOf(ExplicitBreak)) match {
        case (idx1, idx2) if idx1 <= 0 || idx2 <= 0 => false
        case (idx1, idx2) if idx1 == idx2 => text1.take(idx1) == text2.take(idx2)
        case _ => false
      }

    def implicitMatch(text1: String, text2: String, matchLimit: Int): Boolean = partialMatchText(text1, text2)._1.size >= matchLimit

    // If any text component of the two phrases contains the explicit break marker apply explicit matching
    if(useExplicitMatch(p1, p2)) explicitMatch(p1.english, p2.english) && explicitMatch(p1.welsh, p2.welsh)
    else implicitMatch(p1.english, p2.english, MatchLimitEnglish) && implicitMatch(p1.welsh, p2.welsh, MatchLimitWelsh)
  }

  private[services] def useExplicitMatch(p1: Phrase, p2: Phrase): Boolean =
    p1.english.contains(ExplicitBreak) || p1.welsh.contains(ExplicitBreak) || p2.english.contains(ExplicitBreak) || p2.welsh.contains(ExplicitBreak)

  private def partialMatchText(text1: String, text2: String): (Seq[String], List[TextBuilder.Fragment]) = {
    // Break text into fragments, then match
    val fragments1: List[TextBuilder.Fragment] = TextBuilder.fragment(text1)
    val fragments2: List[TextBuilder.Fragment] = TextBuilder.fragment(text2)
    val (matchedItems: List[String], matchedFragments: List[TextBuilder.Fragment]) = TextBuilder.matchFragments(fragments1, fragments2)
    val text1tokens: List[String] = TextBuilder.flattenFragments(fragments1)
    val text2tokens: List[String] = TextBuilder.flattenFragments(fragments2)
    // If the matched token length is less than both of the token lengths of the original texts return the match, nothing otherwise
    if (matchedItems.size >= Math.min(text1tokens.size, text2tokens.size)) (Nil, Nil) else (matchedItems, matchedFragments)
  }
}
