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

package core.models

import java.time.LocalDate
import java.time.format.{DateTimeFormatter, ResolverStyle}
import scala.util.Try
import scala.util.matching.Regex
import Regex._

package object ocelot {
  val TimescaleIdPattern: String = "[A-Za-z][a-zA-Z0-9_-]+"
  val DatePattern: String = "\\d{1,2}\\/\\d{1,2}\\/\\d{4}"
  val HttpUriPattern: String = "https?:[a-zA-Z0-9\\/\\.\\-\\?_\\.=&#]+"
  val StanzaIdPattern: String = s"\\d+|${Process.StartStanzaId}"
  val TenDigitIntPattern: String = "\\d{1,10}"
  val LabelNamePattern: String = "[A-Za-z0-9\\s\\-_]+"

  val LabelPattern: String = s"\\[label:($LabelNamePattern)(?::(currency|currencyPoundsOnly|date|number))?\\]"
  val boldPattern: String = s"\\[bold:($LabelPattern|[^\\]]+)\\]"
  val SimpleTimescalePattern: String = s"\\[timescale:(?:(?:($TimescaleIdPattern):days))\\]"
  val DateAddPattern: String = s"\\[date_add:(?:($LabelNamePattern)|($DatePattern)):($TimescaleIdPattern)\\]"
  val TimescaleIdUsagePattern: String = s"(?:$DateAddPattern)|(?:$SimpleTimescalePattern)"
  val linkToPageOnlyPattern: String = s"\\[link:(.+?):($StanzaIdPattern)\\]"
  val pageLinkPattern: String = s"\\[(button|link)(-same|-tab)?:(.+?):($StanzaIdPattern)\\]"
  val buttonLinkPattern: String = s"\\[(button)(-same|-tab)?:(.+?):($StanzaIdPattern)\\]"
  val linkPattern: String = s"\\[(button|link)(-same|-tab)?:(.+?):($StanzaIdPattern|$HttpUriPattern)\\]"
  val timeConstantPattern: String = s"^($TenDigitIntPattern)\\s*(days?|weeks?|months?|years?)$$"
  val PositiveIntListPattern: String = s"^$TenDigitIntPattern(?:,$TenDigitIntPattern)*$$"
  val listPattern: String = s"\\[list:($LabelNamePattern):length\\]"
  val operandPattern: String = s"^$LabelPattern|$listPattern|$DateAddPattern$$"
  val operandRegex: Regex = operandPattern.r
  val labelsListDateAddPattern: String = s"$LabelPattern|$listPattern|$DateAddPattern"
  val labelsListDateAddRegex: Regex = labelsListDateAddPattern.r
  val hintRegex: Regex = "\\[hint:([^\\]]+)\\]".r
  val pageLinkRegex: Regex = pageLinkPattern.r
  val buttonLinkRegex: Regex = buttonLinkPattern.r
  val labelRefRegex: Regex = LabelPattern.r
  val inputCurrencyRegex: Regex = "^-?£?(\\d{1,3}(,\\d{3})*|\\d+)(\\.(\\d{1,2})?)?$".r
  val inputCurrencyPoundsRegex: Regex = "^-?£?(\\d{1,3}(,\\d{3})*|\\d+)$".r
  val positiveIntRegex: Regex = s"^$TenDigitIntPattern$$".r                                 // Limited to 10 decimal digits
  val listOfPositiveIntRegex: Regex = PositiveIntListPattern.r
  val anyIntegerRegex: Regex = s"^-?(\\d{1,3}(,\\d{3}){0,3}|$TenDigitIntPattern)$$".r       // Limited to 10 decimal digits or 12 comma separated
  val EmbeddedParameterRegex: Regex = """\{(\d)\}""".r
  val ExclusivePlaceholder: String = "[exclusive]"
  val timeConstantRegex: Regex = timeConstantPattern.r
  val TimescaleIdUsageRegex: Regex = TimescaleIdUsagePattern.r

  val DateOutputFormat = "d MMMM uuuu"
  val ignoredCurrencyChars: Seq[Char] = Seq(' ','£', ',')
  val dateFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("d/M/uuuu", java.util.Locale.UK).withResolverStyle(ResolverStyle.STRICT)
  val pageLinkOnlyPattern: String = s"^${linkToPageOnlyPattern}$$"
  val boldOnlyPattern: String = s"^${boldPattern}$$"

  def matchGroup(m: Match)(grp: Int): Option[String] = Option(m.group(grp))
  def operandValue(str: String)(implicit labels: Labels): Option[String] =
    operandRegex.findFirstMatchIn(str).fold[Option[String]](Some(str)){m => scalarMatch(matchGroup(m), labels, labels.value)}
  val LabelNameGroup: Int = 1
  val LabelOutputFormatGroup: Int = 2
  val ListLengthLabelNameGroup: Int = 3
  val DateAddLabelNameGroup: Int = 4
  val DateAddLiteralGroup: Int = 5
  val DateAddTimescaleIdGroup: Int = 6
  def scalarMatch(capture: Int => Option[String], labels: Labels, lbl: String => Option[String]): Option[String] =
    capture(LabelNameGroup).fold{
      capture(ListLengthLabelNameGroup).fold{
        capture(DateAddTimescaleIdGroup).fold[Option[String]](None){tsId =>
          capture(DateAddLabelNameGroup).fold(dateAdd(capture(DateAddLiteralGroup), tsId, labels)){daLabel =>
            dateAdd(lbl(daLabel), tsId, labels)
          }
        }
      }{list => listLength(list, labels)}
    }{label => lbl(label)}

  def buttonLinkIds(str: String): List[String] = plSingleGroupCaptures(buttonLinkRegex, str, 4)
  def buttonLinkIds(phrases: Seq[Phrase]): List[String] = phrases.flatMap(phrase => buttonLinkIds(phrase.english)).toList
  def pageLinkIds(str: String): List[String] = plSingleGroupCaptures(pageLinkRegex, str, 4)
  def pageLinkIds(phrases: Seq[Phrase]): List[String] = phrases.flatMap(phrase => pageLinkIds(phrase.english)).toList
  def labelReferences(str: String): List[String] = plSingleGroupCaptures(labelRefRegex, str)
  def labelReference(str: String): Option[String] = plSingleGroupCaptures(labelRefRegex, str).headOption
  def listLength(listName: String, labels: Labels): Option[String] = labels.valueAsList(listName).fold[Option[String]](None){l => Some(l.length.toString)}
  def stringFromDate(when: LocalDate): String = when.format(dateFormatter)

  def stripHintPlaceholder(p: Phrase): Phrase = Phrase(hintRegex.replaceAllIn(p.english, ""), hintRegex.replaceAllIn(p.welsh, ""))
  def fromPattern(pattern: Regex, text: String): (List[String], List[Match]) = (pattern.split(text).toList, pattern.findAllMatchIn(text).toList)
  def isLinkOnlyPhrase(phrase: Phrase): Boolean =phrase.english.matches(pageLinkOnlyPattern)
  def isBoldOnlyPhrase(phrase: Phrase): Boolean =phrase.english.matches(boldOnlyPattern)
  def stringWithOptionalHint(str: String): (String, Option[String]) = {
    val (txts, matches) = fromPattern(hintRegex, str)
    val hint = matches.headOption.map(m => m.group(1))
    (txts.mkString.trim, hint)
  }

  def asTextString(value: String): Option[String] = value.trim.headOption.fold[Option[String]](None)(_ => Some(value.trim))
  def asDecimal(value: String): Option[BigDecimal] =
    inputCurrencyRegex.findFirstIn(value.filterNot(c => c==' ')).map(s => BigDecimal(s.filterNot(ignoredCurrencyChars.contains(_))))
  def asCurrencyPounds(value: String): Option[BigDecimal] =
    inputCurrencyPoundsRegex.findFirstIn(value.filterNot(c => c==' ')).map(s => BigDecimal(s.filterNot(ignoredCurrencyChars.contains(_))))
  def asDate(value: String): Option[LocalDate] = Try(LocalDate.parse(value.filterNot(_.equals(' ')), dateFormatter)).map(d => d).toOption
  def asPositiveInt(value: String): Option[Int] = matchedInt(value, positiveIntRegex)
  def asAnyInt(value: String): Option[Int] = matchedInt(value, anyIntegerRegex)
  def asListOfPositiveInt(value: String): Option[List[Int]] = listOfPositiveIntRegex.findFirstIn(value.filterNot(_.equals(' ')))
                                                                                    .flatMap(s => lOfOtoOofL(s.split(",").toList.map(asPositiveInt)))
  def asTimePeriod(value: String): Option[TimePeriod] =
    timeConstantRegex.findFirstMatchIn(value.trim).flatMap{m =>
    Option(m.group(1)).fold[Option[TimePeriod]](None)(n =>
      Option(m.group(2)).fold[Option[TimePeriod]](None){
        case "day" | "days" => Some(TimePeriod(n.toInt, Day))
        case "week" | "weeks" => Some(TimePeriod(n.toInt, Week))
        case "month" | "months" => Some(TimePeriod(n.toInt, Month))
        case "year" | "years" => Some(TimePeriod(n.toInt, Year))
      }
    )
  }

  def dateAdd(date: Option[String], tsId: String, labels: Labels): Option[String] =
    labels.timescaleDays(tsId).flatMap(days => date.flatMap(asDate).map(dt => stringFromDate(dt.plusDays(days.toLong))))

  private def plSingleGroupCaptures(regex: Regex, str: String, index: Int = 1): List[String] = regex.findAllMatchIn(str).map(_.group(index)).toList
  private def matchedInt(value: String, regex: Regex): Option[Int] = regex.findFirstIn(value.filterNot(_.equals(' '))).flatMap(asInt)
  private def asInt(value: String): Option[Int] = {
    val longValue: Long = value.filterNot(_ == ',').toLong
    if (longValue < Int.MinValue || longValue > Int.MaxValue) None else Some(longValue.toInt)
  }
}
