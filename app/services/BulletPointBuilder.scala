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
import core.models.ocelot.stanzas.NoteCallout
import scala.util.matching.Regex
import Regex._
import scala.annotation.tailrec

object BulletPointBuilder {

  val NotSpaceRegex: Regex = """([^ ]+)""".r
  val MatchLimitEnglish: Int = 3
  val MatchLimitWelsh: Int = 1
  val Break: String = "break"
  val ExplicitBreak: String = s"[$Break]"
  val BreakMatchPattern: String = s"\\[$Break\\]"

  @tailrec
  def groupBulletPointNoteCalloutPhrases(acc: Seq[Seq[Phrase]])(inputSeq: Seq[NoteCallout]): Seq[Seq[Phrase]] = {
    @tailrec
    def groupMatchingNoteCallouts(inputSeq: Seq[NoteCallout], calloutAcc: Seq[NoteCallout]): Seq[NoteCallout] =
      inputSeq match {
        case Nil => calloutAcc
        case x :: xs if matchPhrases(calloutAcc.last.text, x.text) => groupMatchingNoteCallouts(xs, calloutAcc :+ x)
        case _ => calloutAcc
      }

    inputSeq match {
      case Nil => acc
      case x :: xs =>
        val matchedCallouts: Seq[NoteCallout] = groupMatchingNoteCallouts(xs, Seq(x))
        if(matchedCallouts.size > 1) {
          groupBulletPointNoteCalloutPhrases(acc :+ matchedCallouts.map(_.text))(xs.drop(matchedCallouts.size - 1))
        } else {
          groupBulletPointNoteCalloutPhrases(acc :+ matchedCallouts.map(_.text))(xs)
        }
    }
  }

  def determineMatchedLeadingText(phrases: Seq[Phrase], phraseText: Phrase => String): String = {
    val matchedText: Seq[Seq[String]] = phrases.headOption.fold[Seq[Seq[String]]](Nil){first =>
      phrases.tail.map(p => partialMatchInstructionText(phraseText(first), phraseText(p)))
    }
    val noOfMatchedLeadingWords: Int = matchedText.map(_.length).min
    val (texts, matches) = TextBuilder.placeholderTxtsAndMatches(phraseText(phrases.head))
    val (wordsProcessed, outputTexts, outputMatches) = leadingTextsAndMatches(noOfMatchedLeadingWords, texts, matches)

    constructLeadingText(noOfMatchedLeadingWords, outputTexts, outputMatches).mkString
  }

  /**
    * Method identifies the text and match components containing a specified number of words. These words
    * usually match those identifying the leading text of a bullet point list
    *
    * @param noOfWordsToMatch - The number of words to be located
    * @param inputTexts - The text components for the whole string being searched
    * @param textsProcessed - The number of text elements processed so far
    * @param inputMatches - The match components for the whole string being searched
    * @param matchesProcessed - The number of match elements processed so far
    * @param outputTexts - The texts components containing the matched words
    * @param outputMatches - The match components containing the matched words
    * @param wordsProcessed - The number of matched words identified by previous recursive calls to this method
    *
    * @return The method returns the text and match components that contain the matched words
    */
  private[services] def leadingTextsAndMatches(
      noOfWordsToMatch: Int,
      inputTexts: List[String],
      inputMatches: List[Match],
      textsProcessed: Int = 0,
      matchesProcessed: Int = 0,
      outputTexts: List[String] = Nil,
      outputMatches: List[Match] = Nil,
      wordsProcessed: Int = 0
  ): (Int, List[String], List[Match]) =
    if (textsProcessed == inputTexts.size) {
      if (matchesProcessed == inputMatches.size) {
        (wordsProcessed, outputTexts, outputMatches)
      } else {
        leadingMatches(
          noOfWordsToMatch,
          inputTexts,
          inputMatches,
          textsProcessed,
          matchesProcessed,
          outputTexts,
          outputMatches,
          wordsProcessed
        )
      }
    } else {
      val text: String = inputTexts(textsProcessed)
      val noOfWords: Int = wordCount(text, textsProcessed, inputMatches, matchesProcessed)
      if (processNextMatch(noOfWordsToMatch, wordsProcessed+noOfWords, inputTexts, textsProcessed, inputMatches, matchesProcessed)) {
        leadingMatches(
          noOfWordsToMatch,
          inputTexts,
          inputMatches,
          textsProcessed + 1,
          matchesProcessed,
          outputTexts :+ text,
          outputMatches,
          wordsProcessed + noOfWords
        )
      } else {
        (wordsProcessed + noOfWords, (outputTexts :+ text), outputMatches)
      }
    }

  private[services] def leadingMatches(
      noOfWordsToMatch: Int,
      inputTexts: List[String],
      inputMatches: List[Match],
      textsProcessed: Int,
      matchesProcessed: Int,
      outputTexts: List[String],
      outputMatches: List[Match],
      wordsProcessed: Int
  ): (Int, List[String], List[Match]) = {
    if (matchesProcessed == inputMatches.size) {
      (wordsProcessed, outputTexts, outputMatches)
    } else {
      val text: String = TextBuilder.placeholderMatchText(inputMatches(matchesProcessed))
      val noOfWords: Int = wordCountInMatchText(text, inputTexts, textsProcessed)
      if (processNextText(noOfWordsToMatch, wordsProcessed+noOfWords, inputTexts, textsProcessed, text)) {
        leadingTextsAndMatches(
          noOfWordsToMatch,
          inputTexts,
          inputMatches,
          textsProcessed,
          matchesProcessed + 1,
          outputTexts,
          outputMatches :+ inputMatches(matchesProcessed),
          wordsProcessed + noOfWords
        )
      } else {
        (wordsProcessed + noOfWords, outputTexts, (outputMatches :+ inputMatches(matchesProcessed)))
      }
    }
  }

  /*
    * Method gathers the leading text from the text and match components containing the match words
    *
    * @param wordLimit - The number of words in the text to be reconstructed
    * @param texts - The text components containing some of the matched words
    * @param textsProcessed - Count of number of text elements previously processed
    * @param matches - The match components containing some of the matched words
    * @param matchesProcessed - Count of number of match elements previously processed
    * @param items - The list of text items containing the matched words
    * @param wordsProcessed - The number of words gathered by previous recursive calls to the method
    *
    * @return - Returns a list of strings containing the leading text
    */
  private def constructLeadingText(
      wordLimit: Int,
      texts: List[String],
      matches: List[Match],
      textsProcessed: Int = 0,
      matchesProcessed: Int = 0,
      items: List[String] = Nil,
      wordsProcessed: Int = 0
  ): List[String] =
    if ((textsProcessed == texts.size) && (matchesProcessed < matches.size)) {
      constructLeadingTextFromMatches(wordLimit, texts, matches, textsProcessed, matchesProcessed, items, wordsProcessed)
    } else if ((texts.size - textsProcessed == 1) && (matches.size == matchesProcessed)) {
      val noOfWordsToAdd: Int = wordLimit - wordsProcessed
      val leadingText: String = extractLeadingMatchedWords(noOfWordsToAdd, texts, textsProcessed, matches, matchesProcessed)
      items :+ leadingText
    } else {
      val text: String = texts(textsProcessed)
      val noOfWords: Int = wordCount(text, textsProcessed, matches, matchesProcessed)
      constructLeadingTextFromMatches(wordLimit, texts, matches, textsProcessed + 1, matchesProcessed, items :+ text, wordsProcessed + noOfWords)
    }

  private def constructLeadingTextFromMatches(
      wordLimit: Int,
      texts: List[String],
      matches: List[Match],
      textsProcessed: Int,
      matchesProcessed: Int,
      items: List[String],
      wordsProcessed: Int
  ): List[String] =
    if ((matches.size - matchesProcessed == 1) && (texts.size == textsProcessed)) {
      items :+ matches(matchesProcessed).toString
    } else {
      val text: String = TextBuilder.placeholderMatchText(matches(matchesProcessed))
      val noOfWords: Int = wordCountInMatchText(text, texts, textsProcessed)
      constructLeadingText(
        wordLimit,
        texts,
        matches,
        textsProcessed,
        matchesProcessed + 1,
        items :+ matches(matchesProcessed).toString(),
        wordsProcessed + noOfWords
      )
    }

  /**
    * Method returns a substring of the input text comprising "noOfMatchedWords" words separated by white space
    *
    * @param noOfMatchedWords - The number of words to be included in the string returned by the method
    * @param texts - The text components containing some of the matched words
    * @param textsProcessed - Count of number of text elements previously processed
    * @param matches - The match components containing some of the matched words
    * @param matchesProcessed - Count of number of match elements previously processed
    *
    * @return - Returns a sub-sample of the final input text
    */
  private def extractLeadingMatchedWords(noOfMatchedWords: Int, texts: List[String], textsProcessed: Int, matches: List[Match], matchesProcessed: Int): String = {
    val textElements: List[Match] = NotSpaceRegex.findAllMatchIn(texts(textsProcessed)).toList

    matches match {
      case Nil if noOfMatchedWords <= textElements.size => texts(textsProcessed).substring(0, textElements(noOfMatchedWords - 1).end)
      case Nil => texts(textsProcessed)
      // A value of zero for the number of matched words indicates single word in text immediately
      // following a bold or link annotation
      case _ if noOfMatchedWords == 0 => texts(textsProcessed).substring(0, textElements.head.end)
      case _ => getUpdatedLeadingMatchedWords(noOfMatchedWords, textElements, texts, textsProcessed, matches, matchesProcessed)
    }
  }

  private def getUpdatedLeadingMatchedWords(
      noOfMatchedWords: Int,
      textElements: List[Match],
      texts: List[String],
      textsProcessed: Int,
      matches: List[Match],
      matchesProcessed: Int
  ): String = {

    val trailingText: Boolean = textTrailingMatchText(textsProcessed, texts, TextBuilder.placeholderMatchText(matches(matchesProcessed - 1)))

    val updatedNoOfWords: Int = updateNoOfMatchedWords(trailingText, noOfMatchedWords)

    updatedNoOfWords match {
      case x if x <= textElements.size => texts(textsProcessed).substring(0, textElements(updatedNoOfWords - 1).end)
      case _ => texts(textsProcessed)
    }
  }

  private[services] def matchPhrases(p1: Phrase, p2: Phrase): Boolean = {

    def explicitMatch(text1: String, text2: String): Boolean =
      (text1, text2) match {
        case (txt1, txt2) if txt1 == txt2 => false
        case (txt1, txt2) =>
          (txt1.indexOf(ExplicitBreak), txt2.indexOf(ExplicitBreak)) match {
            case (index1, index2) if (index1 == index2) && index1 > 0 && index2 > 0 =>
              txt1.substring(0, index1) == txt2.substring(0, index2)
            case _ => false
          }
      }

    def implicitMatch(text1: String, text2: String, matchLimit: Int): Boolean = partialMatchInstructionText(text1, text2).size >= matchLimit

    // If any text component of the two phrases contains the explicit break marker apply explicit matching
    if(useExplicitMatch(p1, p2)) explicitMatch(p1.english, p2.english) && explicitMatch(p1.welsh, p2.welsh)
    else implicitMatch(p1.english, p2.english, MatchLimitEnglish) && implicitMatch(p1.welsh, p2.welsh, MatchLimitWelsh)
  }

  private[services] def useExplicitMatch(p1: Phrase, p2: Phrase): Boolean =
    p1.english.contains(ExplicitBreak) || p1.welsh.contains(ExplicitBreak) || p2.english.contains(ExplicitBreak) || p2.welsh.contains(ExplicitBreak)

  private def partialMatchInstructionText(text1: String, text2: String): Seq[String] = {
    // Break instruction text into fragments, then tokens, retaining non space joins
    // between placeholder tokens and string based tokens
    val fragments1: List[TextBuilder.Fragment] = TextBuilder.placeholderFragments(text1)
    val fragments2: List[TextBuilder.Fragment] = TextBuilder.placeholderFragments(text2)
    val matchedItems: List[String] = TextBuilder.matchFragments(fragments1, fragments2)
    val text1tokens: List[String] = TextBuilder.flattenFragments(fragments1)
    val text2tokens: List[String] = TextBuilder.flattenFragments(fragments2)
    // Return "No Match" if total match found (identical phrases)
    // Matching instructions must have matching leading text followed dissimilar trailing text
    if (matchedItems.size >= Math.min(text1tokens.size, text2tokens.size)) Nil else matchedItems
  }

  private def wordCount(text: String): Int = NotSpaceRegex.findAllMatchIn(text).toList.size

  private def wordCount(text: String, textsProcessed: Int, matches: List[Match], matchesProcessed: Int): Int =
    wordCount(text) match {
      case count if textsProcessed == 0 => count
      // No gap between placeholder and following text, reduce word count
      case count if !intraTextGapExists(TextBuilder.placeholderMatchText(matches(matchesProcessed - 1)), text) => count - 1
      case count => count
    }

  private def wordCountInMatchText(matchText: String, texts: List[String], textsProcessed: Int): Int =
    wordCount(matchText) match {
      case count if textsProcessed == 0 => count
      case count if texts(textsProcessed - 1).isEmpty => count
      // No gap between placeholder and following text, reduce word count
      case count if !intraTextGapExists(texts(textsProcessed - 1), matchText) => count - 1
      case count => count
    }

  private def processNextMatch(
      noOfWordsToMatch: Int,
      processed: Int,
      texts: List[String],
      textsProcessed: Int,
      matches: List[Match],
      matchesProcessed: Int
  ): Boolean = {
    val textLeadsNextMatch: Boolean = (processed == noOfWordsToMatch) &&
      (matchesProcessed < matches.size) &&
      textLeadingMatchText(textsProcessed, texts, matches(matchesProcessed))

    processed < noOfWordsToMatch || textLeadsNextMatch
  }

  private def processNextText(
      noOfWordsToMatch: Int,
      processed: Int,
      texts: List[String],
      textsProcessed: Int,
      matchText: String
  ): Boolean = {
    val textFollowingPreviousMatch: Boolean = (processed == noOfWordsToMatch) &&
      textTrailingMatchText(textsProcessed, texts, matchText)

    processed < noOfWordsToMatch || textFollowingPreviousMatch
  }

  private def textLeadingMatchText(textsProcessed: Int, texts: List[String], m: Match): Boolean =
    !intraTextGapExists(texts(textsProcessed), TextBuilder.placeholderMatchText(m))

  private def textTrailingMatchText(textsProcessed: Int, texts: List[String], matchText: String): Boolean =
    if (texts.size - 1 >= textsProcessed) !intraTextGapExists(matchText, texts(textsProcessed)) else false

  private def updateNoOfMatchedWords(trailingText: Boolean, noOfMatchedWords: Int): Int = if (trailingText) noOfMatchedWords + 1 else noOfMatchedWords
  private def intraTextGapExists(text1: String, text2: String): Boolean = text1.endsWith(" ") || text2.startsWith(" ")
}
