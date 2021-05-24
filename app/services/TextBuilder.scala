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

import models._
import core.models.ocelot.{Link, Phrase, hintRegex, labelPattern, listPattern, listLength}
import core.models.ocelot.{labelAndListRegex, labelScalarMatch, boldPattern, linkPattern}
import models.ui._
import scala.util.matching.Regex
import Regex._
import play.api.i18n.{Messages, Lang}
import scala.annotation.tailrec

object TextBuilder {
  val English: Lang = Lang("en")
  val Welsh: Lang = Lang("cy")

  object Placeholders {
    // Indexes into the Placeholder regex match groups
    val LabelNameIdx: Int = 1
    val LabelFormatIdx: Int = 3
    val BoldTextIdx: Int = 4
    val BoldLabelNameIdx: Int = 5
    val BoldLabelFormatIdx: Int = 7
    val ButtonOrLinkIdx: Int = 8
    val LinkTypeIdx: Int = 9
    val LinkTextIdx: Int = 10
    val LinkDestIdx: Int = 11
    val ListNameIdx: Int = 12
    val plregex: Regex = s"$labelPattern|$boldPattern|$linkPattern|$listPattern".r
    def labelNameOpt(m: Match): Option[String] = Option(m.group(LabelNameIdx))
    def labelFormatOpt(m: Match): Option[String] = Option(m.group(LabelFormatIdx))
    def boldTextOpt(m: Match): Option[String] = Option(m.group(BoldTextIdx))
    def boldLabelNameOpt(m: Match): Option[String] = Option(m.group(BoldLabelNameIdx))
    def boldLabelFormatOpt(m: Match): Option[String] = Option(m.group(BoldLabelFormatIdx))
    def buttonOrLink(m: Match): Option[String] = Option(m.group(ButtonOrLinkIdx))
    def linkTypeOpt(m: Match): Option[String] = Option(m.group(LinkTypeIdx))
    def linkText(m: Match): String = m.group(LinkTextIdx)
    def linkTextOpt(m: Match): Option[String] = Option(linkText(m))
    def linkDest(m: Match): String = m.group(LinkDestIdx)
    def listNameOpt(m: Match): Option[String] = Option(m.group(ListNameIdx))
  }
  import Placeholders._

  private def fromPattern(pattern: Regex, text: String): (List[String], List[Match]) =
    (pattern.split(text).toList, pattern.findAllMatchIn(text).toList)

  private def placeholdersToItems(matches: List[Match])(implicit ctx: UIContext): List[TextItem] =
    matches.map { m =>
      labelNameOpt(m).fold[TextItem]({
        boldTextOpt(m).fold[TextItem]({
          listNameOpt(m).fold[TextItem]({
            val window: Boolean = linkTypeOpt(m).fold(false)(modifier => modifier == "-tab")
            val dest: String = if (Link.isLinkableStanzaId(linkDest(m))) ctx.pageMapById(linkDest(m)).url else linkDest(m)
            val asButton: Boolean = buttonOrLink(m).fold(false)(_ == "button")
            val (lnkText, lnkHint) = singleStringWithOptionalHint(linkText(m))
            ui.Link(dest, lnkText, window, asButton, lnkHint)
          }){listName => Words(listLength(listName, ctx.labels).getOrElse("0"))}
        }){txt =>
          boldLabelNameOpt(m).fold[TextItem](Words(txt, true)){labelName =>
            LabelRef(labelName, OutputFormat(boldLabelFormatOpt(m)), true)
          }
        }
      })(labelName => LabelRef(labelName, OutputFormat(labelFormatOpt(m))))
    }

  def expandLabels(p: Phrase)(implicit ctx: UIContext): Phrase = Phrase(expandLabels(p.english, English), expandLabels(p.welsh, Welsh))

  private def expandLabels(s: String, lang: Lang)(implicit ctx: UIContext): String = {
    val messages: Messages = ctx.messagesApi.preferred(Seq(lang))
    def labelValue(name: String): Option[String] = ctx.labels.displayValue(name)(lang)
    labelAndListRegex.replaceAllIn(s, { m => OutputFormat(labelFormatOpt(m)).asString(labelScalarMatch(m, ctx.labels, labelValue _), messages)})
  }

  def fromPhrase(txt: Phrase)(implicit ctx: UIContext): Text = {
    val isEmpty: TextItem => Boolean = _.isEmpty
    val (texts, matches) = fromPattern(plregex, expandLabels(txt.value(ctx.lang), ctx.lang))
    Text(merge(texts.map(Words(_)), placeholdersToItems(matches), Nil, isEmpty))
  }

  private def singleStringWithOptionalHint(str: String): (String, Option[String]) = {
    val (txts, matches) = fromPattern(hintRegex, str)
    val hint = matches.headOption.map(m => m.group(1))
    (txts.head.trim, hint)
  }

  // Parses current lang value of Phrase potentially containing a hint pattern[hint:<Text Hint>]
  // The string before the first hint will be converted to a Text object
  // and returned along with optional hint
  // All characters after the optional hint pattern are discarded
  def fromPhraseWithOptionalHint(txt: Phrase)(implicit ctx: UIContext): (Text, Option[Text]) = {
    val isEmpty: TextItem => Boolean = _.isEmpty
    val (str, hint) = singleStringWithOptionalHint(txt.value(ctx.lang))
    val (texts, matches) = fromPattern(plregex, expandLabels(str, ctx.lang))
    (Text(merge(texts.map(Words(_)), placeholdersToItems(matches), Nil, isEmpty)), hint.map(Text(_)))
  }

  @tailrec
  private def merge[A, B](txts: List[A], links: List[A], acc: Seq[A], isEmpty: A => Boolean): Seq[A] =
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
  def placeholderMatchText(m: Match): String = boldTextOpt(m).getOrElse(linkTextOpt(m).getOrElse(""))
  def placeholderTxtsAndMatches(text: String): (List[String], List[Match]) = fromPattern(plregex, text)
  def flattenPlaceholders(text: String): Seq[String] = {
    val (txts, matches) = fromPattern(plregex, text)
    merge[String, String](txts, matches.map(m => boldTextOpt(m).fold(linkTextOpt(m).getOrElse(""))(v => v)), Nil, _.isEmpty).filterNot(_.isEmpty)
  }
}
