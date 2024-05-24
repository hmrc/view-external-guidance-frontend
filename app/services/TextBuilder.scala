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

import models._
import core.models.ocelot.{Link, Phrase, Labels, stringWithOptionalHint, fromPattern}
import core.models.ocelot.{UiExpansionRegex, LabelOutputFormatGroup, matchGroup, scalarMatch, boldPattern, linkPattern}
import models.ui._
import scala.util.matching.Regex
import Regex._
import play.api.i18n.{Messages, Lang}
import scala.annotation.tailrec
import uk.gov.hmrc.hmrcfrontend.views.viewmodels.language.{En, Cy}

object StringTransform {
  val OriginalCaptureIdx: Int = 0
  val Apostrophe: String = "'"
  val CurlyApostrophe: String = "’"
  val ApostropheCaptureIdx: Int = 1

  val StandardDash: String = " - "
  val LongDash: String = " – "
  val DashCaptureIdx: Int = 2
  val matchRegex: Regex = s"($Apostrophe)|($StandardDash)".r
  def transform(phrase: Phrase)(implicit ctx: UIContext): String = transform(phrase.value(ctx.messages.lang))
  def transform(s: String): String = matchRegex.replaceAllIn(s, m =>
    Option(m.group(ApostropheCaptureIdx)).fold{
      Option(m.group(DashCaptureIdx)).fold(m.group(OriginalCaptureIdx))(_ => LongDash)
    }(_ => CurlyApostrophe)
  )
}

object TextBuilder {
  val NonWhitespaceRegex: Regex = "[^\\s]+".r
  val English: Lang = Lang(En.code)
  val Welsh: Lang = Lang(Cy.code)
  val languageMap = Map(En.code -> services.TextBuilder.English, Cy.code -> services.TextBuilder.Welsh)
  def language(languageCode: String): Lang = languageMap.get(languageCode).getOrElse(English)

  private object TextPlaceholders {
    // Indexes into the Placeholder regex match groups
    val BoldTextIdx: Int = 1
    val ButtonOrLinkIdx: Int = 2
    val LinkTypeIdx: Int = 3
    val LinkTextIdx: Int = 4
    val LinkDestIdx: Int = 5
    val placeholderPattern: String = s"$boldPattern|$linkPattern"
    val plregex: Regex = placeholderPattern.r
    def from(text: String):(List[String], List[Match]) = fromPattern(plregex, text)

    def placeholdersToItems(matches: List[Match])(implicit ctx: UIContext): List[TextItem] =
      matches.map { m =>
        val capture: Int => Option[String] = matchGroup(m)
        capture(BoldTextIdx).fold[TextItem]({
            val linkDestination = m.group(LinkDestIdx)
            val window: Boolean = capture(LinkTypeIdx).fold(false)(modifier => modifier == "-tab")
            val dest: String = if (Link.isLinkableStanzaId(linkDestination)) ctx.pageMapById(linkDestination).url else linkDestination
            val asButton: Boolean = capture(ButtonOrLinkIdx).fold(false)(_ == "button")
            val (lnkText, lnkHint) = stringWithOptionalHint(m.group(LinkTextIdx))
            ui.Link(dest, lnkText, window, asButton, lnkHint)
          })
        {txt => Words(txt, true)}
      }
  }

  import TextPlaceholders._
  import StringTransform._

  def expandLabels(labels: Labels)(p: Phrase)(implicit messages: Messages): Phrase = expandLabels(p, labels)
  def expandLabels(p: Phrase, labels: Labels)(implicit messages: Messages): Phrase =
    messages.lang match {
      case English => Phrase(expandLabels(p.english, labels), p.welsh)
      case Welsh => Phrase(p.english, expandLabels(p.welsh, labels))
      case _ => Phrase(expandLabels(p.english, labels), p.welsh)
    }

  private[services] def expandLabels(text: String, labels: Labels)(implicit messages: Messages): String = {
    def labelValue(name: String): Option[String] = labels.displayValue(name)(messages.lang).map(Regex.quoteReplacement)
    def expand(s: String): String = UiExpansionRegex.replaceAllIn(s, {m =>
                                      OutputFormat(Option(m.group(LabelOutputFormatGroup))).asString(scalarMatch(matchGroup(m), labelValue)(labels), messages)
                                    })
    expand(expand(text)) // Double expansion to allow for labels as arguments to lists and functions
  }

  def fromPhrase(txt: Phrase)(implicit ctx: UIContext): Text = {
    val isEmpty: TextItem => Boolean = _.isEmpty
    val (texts, matches) = from(transform(txt.value(ctx.messages.lang)))
    Text(merge(texts.map(Words(_)), placeholdersToItems(matches), Nil, isEmpty))
  }

  // Parses current lang value of Phrase potentially containing a hint pattern[hint:<Text Hint>]
  // The string before the first hint will be converted to a Text object
  // and returned along with optional hint
  // All characters after the optional hint pattern are discarded
  def fromPhraseWithOptionalHint(txt: Phrase)(implicit ctx: UIContext): (Text, Option[Text]) = {
    val isEmpty: TextItem => Boolean = _.isEmpty
    val (str, hint) = stringWithOptionalHint(txt.value(ctx.messages.lang))
    val (texts, matches) = from(transform(str))
    (Text(merge(texts.map(Words(_)), placeholdersToItems(matches), Nil, isEmpty)), hint.map(Text(_)))
  }

  @tailrec
  private def merge[A, B](txts: List[A], links: List[A], acc: List[A], isEmpty: A => Boolean): List[A] =
    (txts, links) match {
      case (Nil, Nil) => acc
      case (t :: txs, l :: lxs) if isEmpty(t) => merge(txs, lxs, acc :+ l, isEmpty)
      case (t :: txs, l :: lxs) => merge(txs, lxs, (acc :+ t) :+ l, isEmpty)
      case (t, Nil) => acc ++ t
      case (Nil, l) => acc ++ l
    }

  //
  // Following used by BulletPointBuilder
  //
  sealed trait Fragment {
    def tokenise(s: String): List[String] = {
      val (txts, matches) = fromPattern(NonWhitespaceRegex, s)
      merge[String, String](txts, matches.map(_.toString), Nil, _.isEmpty)
    }
    val original: String
    val tokens: List[String]
    lazy val size:Int = tokens.length
  }

  case class TotalMatch(original: String, txt: String) extends Fragment {val tokens: List[String] = tokenise(txt)}
  case class PartialMatch(original: String) extends Fragment {val tokens: List[String] = tokenise(original)}

  def fragment(text: String): List[Fragment] = {
    val (txts, matches) = fromPattern(plregex, text)
    merge[Fragment, Fragment](txts.map(PartialMatch), matches.map(m => TotalMatch(m.group(0), placeholderText(m))), Nil, _ => false)
  }

  def flattenFragments(f: List[Fragment]): List[String] = f.flatMap(_.tokens).mkString.split("\\s+").toList

  def matchFragments(f1: List[Fragment], f2: List[Fragment]): (List[String], List[Fragment]) = {
    @tailrec
    def matchFragments(f1: List[Fragment], f2: List[Fragment], acc: List[String], facc: List[Fragment]): (List[String], List[Fragment]) =
      (f1, f2) match {
        case (Nil, _) | (_, Nil) => (acc, facc)
        case ((f1: Fragment) :: xs1, (f2: Fragment) :: xs2) if f1.equals(f2) => matchFragments(xs1, xs2, acc ++ f1.tokens, facc :+ f1)
        case ((f1: PartialMatch) :: _, (f2: PartialMatch) :: _) =>
          val matching: List[String] = (f1.tokens zip f2.tokens).takeWhile(t => t._1 == t._2 ).map(_._1)
          (acc ++ matching, facc :+ PartialMatch(matching.mkString))
        case _ => (acc, facc)
      }

    matchFragments(f1, f2, Nil, Nil) match {
      case (Nil, _) => (Nil, Nil)
      case (stringList, fragments) => (stringList.mkString.split("\\s+").toList, fragments)
    }
  }

  def join(fragments: List[Fragment]): String = join(fragments, Nil)

  @tailrec
  private def join(fragments: List[Fragment], acc: List[String]): String = fragments match {
    case Nil => acc.reverse.mkString
    case f :: xs => join(xs, f.original :: acc )
  }

  private def placeholderText(m: Match): String = matchGroup(m)(BoldTextIdx).getOrElse(matchGroup(m)(LinkTextIdx).getOrElse(""))
}
