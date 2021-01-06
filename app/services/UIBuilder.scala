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

import javax.inject.Singleton
import models._
import models.ocelot.stanzas.{CurrencyInput, CurrencyPoundsOnlyInput, DateInput, Input, Question, _}
import models.ocelot.{Link, Phrase}
import models.ui.{Answer, BulletPointList, ConfirmationPanel, CyaSummaryList, Details, ErrorMsg, H1, H2, H3, H4, InsetText}
import models.ui.{NameValueSummaryList, Page, Paragraph, RequiredErrorMsg, Table, Text, TypeErrorMsg, UIComponent, ValueErrorMsg, stackStanzas}
import play.api.Logger

import scala.annotation.tailrec

sealed trait ErrorStrategy {
  def default(stanzas: Seq[VisualStanza]): ErrorStrategy = this
}
case object NoError extends ErrorStrategy
case object ValueMissingError extends ErrorStrategy
case object ValueTypeError extends ErrorStrategy {
  override def default(stanzas: Seq[VisualStanza]): ErrorStrategy =
    stanzas.collect{case s:TypeErrorCallout => s}
           .headOption
           .fold[ErrorStrategy](ValueMissingError)(_ => this)
}

@Singleton
class UIBuilder {
  val logger: Logger = Logger(getClass)
  val stanzaTransformPipeline: Seq[Seq[VisualStanza] => Seq[VisualStanza]] =
    Seq(BulletPointBuilder.groupBulletPointInstructions(Nil), Aggregator.aggregateStanzas(Nil), stackStanzas(Nil))

  def buildPage(url: String, stanzas: Seq[VisualStanza], errStrategy: ErrorStrategy = NoError)
               (implicit stanzaIdToUrlMap: Map[String, String]): Page =
    Page(url,
         fromStanzas(stanzaTransformPipeline.foldLeft(stanzas){case (s, t) => t(s)},
                     Nil,
                     errStrategy.default(stanzas)))

  @tailrec
  private def fromStanzas(stanzas: Seq[VisualStanza], acc: Seq[UIComponent], errStrategy: ErrorStrategy)
                 (implicit stanzaIdToUrlMap: Map[String, String]): Seq[UIComponent] =
    stanzas match {
      case Nil => acc
      case (sg: StackedGroup) :: xs => fromStanzas(xs, acc ++ fromStackedGroup(sg, errStrategy), errStrategy)
      case (i: Instruction) :: xs => fromStanzas(xs, acc ++ Seq(fromInstruction(i)), errStrategy)
      case (ig: InstructionGroup) :: xs => fromStanzas(xs, acc ++ Seq(fromInstructionGroup(ig)), errStrategy)
      case (rg: RowGroup) :: xs if rg.isCYASummaryList => fromStanzas(xs, acc ++ Seq(fromCYASummaryListRowGroup(rg)), errStrategy)
      case (rg: RowGroup) :: xs if rg.isNameValueSummaryList => fromStanzas(xs, acc ++ Seq(fromNameValueSummaryListRowGroup(rg)), errStrategy)
      case (nl: NumberedList) :: xs => fromStanzas(xs, acc ++ Seq(fromNumberedList(nl)), errStrategy)
      case (nl: NumberedCircleList) :: xs => fromStanzas(xs, acc ++ Seq(fromNumberedCircleList(nl)), errStrategy)
      case (c: Callout) :: xs => fromStanzas(xs, acc ++ fromCallout(c, errStrategy), errStrategy)
      case (in: Input) :: xs => fromStanzas(Nil, Seq(fromInput(in, acc)), errStrategy)
      case (q: Question) :: xs => fromStanzas(Nil, Seq(fromQuestion(q, acc)), errStrategy)
      case (ng: NoteGroup) :: xs => fromStanzas(xs, acc ++ Seq(fromNoteGroup(ng)), errStrategy)
      case (ycg: YourCallGroup) :: xs => fromStanzas(xs, acc ++ Seq(fromYourCallGroup(ycg)), errStrategy)
      case x :: xs =>
        logger.error(s"Encountered and ignored VisualStanza invalid due to accessibility rules, $x")
        fromStanzas(xs, acc, errStrategy)
    }

  private def fromStackedGroup(sg: StackedGroup, errStrategy: ErrorStrategy)(implicit stanzaIdToUrlMap: Map[String, String]): Seq[UIComponent] =
    sg.group match {
      case (c: SubSectionCallout) :: (rg: RowGroup) :: xs if rg.isTableCandidate =>
        fromStanzas(stackStanzas(Nil)(xs), Seq(fromTableRowGroup(TextBuilder.fromPhrase(c.text), rg)), errStrategy)
      case (c: SubSectionCallout) :: (ng: NoteGroup) :: xs  =>
        fromStanzas(stackStanzas(Nil)(xs), Seq(fromSectionAndNoteGroup(TextBuilder.fromPhrase(c.text), ng)), errStrategy)
      case (c: SubSectionCallout) :: (nc: NoteCallout) :: xs  =>
        fromStanzas(stackStanzas(Nil)(xs), Seq(fromSectionAndNoteCallout(TextBuilder.fromPhrase(c.text), nc)), errStrategy)
      case x :: xs => // No recognised stacked pattern
        fromStanzas(x +: stackStanzas(Nil)(xs), Nil, errStrategy)
    }

  private def fromCYASummaryListRowGroup(rg: RowGroup)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    CyaSummaryList(rg.paddedRows.map(row => row.map(phrase => TextBuilder.fromPhrase(phrase))))

  private def fromNameValueSummaryListRowGroup(rg: RowGroup)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    NameValueSummaryList(rg.paddedRows.map(row => row.map(phrase => TextBuilder.fromPhrase(phrase))))

  private def fromTableRowGroup(caption: Text, rg: RowGroup)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent = {
    val tableRows: Seq[Seq[Text]] = rg.paddedRows.map(r => r.map(phrase => TextBuilder.fromPhrase(phrase)))
    Table(caption, tableRows.head, tableRows.tail)
  }

  private def fromNumberedList(nl: NumberedList)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    ui.NumberedList(nl.group.map(co => TextBuilder.fromPhrase(co.text)))

  private def fromNumberedCircleList(nl: NumberedCircleList)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    ui.NumberedCircleList(nl.group.map(co => TextBuilder.fromPhrase(co.text)))

  private def fromInstruction( i:Instruction)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    i match {
      case Instruction(txt, _, Some(Link(id, dest, _, window)), _, _) if Link.isLinkableStanzaId(dest) =>
        Paragraph(Text.link(stanzaIdToUrlMap(dest), txt.langs, window))
      case Instruction(txt, _, Some(Link(id, dest, _, window)), _, _) => Paragraph(Text.link(dest, txt.langs, window))
      case Instruction(txt, _, _, _, _) => Paragraph(TextBuilder.fromPhrase(txt))
    }

  private def fromQuestion(q: Question, components: Seq[UIComponent]): UIComponent = {
    val answers = q.answers.map { ans =>
      val (answer, hint) = TextBuilder.singleTextWithOptionalHint(ans)
      Answer(answer, hint)
    }
    // Split out an Error callouts from body components
    val (errorMsgs, uiElements) = partitionComponents(components, Seq.empty, Seq.empty)
    val (question, hint) = TextBuilder.singleTextWithOptionalHint(q.text)
    ui.Question(question, hint, uiElements, answers, errorMsgs)
  }

  private def fromCallout(co: Callout, errStrategy: ErrorStrategy)(implicit stanzaIdToUrlMap: Map[String, String]): Seq[UIComponent] =
    co match {
      case c: TitleCallout => Seq(H1(TextBuilder.fromPhrase(c.text)))
      case c: SubTitleCallout => Seq(H2(TextBuilder.fromPhrase(c.text)))
      case c: SectionCallout => Seq(H3(TextBuilder.fromPhrase(c.text)))
      case c: SubSectionCallout => Seq(H4(TextBuilder.fromPhrase(c.text)))
      case c: LedeCallout => Seq(Paragraph(TextBuilder.fromPhrase(c.text), lede = true))
      case c: YourCallCallout => Seq(ConfirmationPanel(TextBuilder.fromPhrase(c.text)))
      case c: NoteCallout => Seq(InsetText(Seq(TextBuilder.fromPhrase(c.text))))
      case c: TypeErrorCallout if errStrategy == ValueTypeError => Seq(TypeErrorMsg(TextBuilder.fromPhrase(c.text)))
      case c: ValueErrorCallout => Seq(ValueErrorMsg(TextBuilder.fromPhrase(c.text)))
      case c: ErrorCallout if errStrategy == ValueMissingError => Seq(RequiredErrorMsg(TextBuilder.fromPhrase(c.text)))
      case _: ImportantCallout => Seq.empty               // Reserved for future use
      case _: NumberedListItemCallout => Seq.empty        // Unused
      case _: NumberedCircleListItemCallout => Seq.empty  // Unused
      case _ => Seq.empty                                 // Consume unmatched Error callouts
    }

  private def fromInstructionGroup(insGroup: InstructionGroup)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent = {
    def createBulletPointItems(leadingEn: String, leadingCy: String, remainder: Seq[Instruction])
                              (implicit stanzaIdToUrlMap: Map[String, String]): Seq[Text] =
      remainder.map { instruction =>
        val bulletPointEnglish: String = instruction.text
          .langs(0)
          .substring(leadingEn.length, instruction.text.langs(0).length)
          .trim
        val bulletPointWelsh: String = instruction.text
          .langs(1)
          .substring(leadingCy.length, instruction.text.langs(1).length)
          .trim

        TextBuilder.fromPhrase(Phrase(bulletPointEnglish, bulletPointWelsh))
      }

    val leadingEn: String = BulletPointBuilder.determineMatchedLeadingText(insGroup, 0)
    val leadingCy: String = BulletPointBuilder.determineMatchedLeadingText(insGroup, 1)
    // Process bullet points
    val bulletPointListItems: Seq[Text] = createBulletPointItems(leadingEn, leadingCy, insGroup.group)

    BulletPointList(TextBuilder.fromPhrase(Phrase(leadingEn, leadingCy)), bulletPointListItems)
  }

  private def fromInput(input: Input, components: Seq[UIComponent])
                       (implicit stanzaIdToUrlMap: Map[String, String]): UIComponent = {
    // Split out an Error callouts from body components
    val (errorMsgs, uiElements) = partitionComponents(components, Seq.empty, Seq.empty)
    val name = TextBuilder.fromPhrase(input.name)
    val hint = input.help.map(phrase => TextBuilder.fromPhrase(phrase))
    // Placeholder not used
    input match {
      case _: CurrencyInput => ui.CurrencyInput(name, hint, uiElements, errorMsgs)
      case _: CurrencyPoundsOnlyInput => ui.CurrencyPoundsOnlyInput(name, hint, uiElements, errorMsgs)
      case _: DateInput => ui.DateInput(name, hint, uiElements, errorMsgs)
    }
  }

  @tailrec
  private def partitionComponents(components: Seq[UIComponent], errors: Seq[ErrorMsg], others: Seq[UIComponent]): (Seq[ErrorMsg], Seq[UIComponent]) =
    components match {
      case Nil => (errors.reverse, others.reverse)
      case (e: ErrorMsg) :: xs => partitionComponents(xs, e +: errors, others)
      case x :: xs => partitionComponents(xs, errors, x +: others)
    }

  private def fromNoteGroup(ng: NoteGroup)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    InsetText(ng.group.map(co => TextBuilder.fromPhrase(co.text)))

  private def fromYourCallGroup(ycg: YourCallGroup)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent = {
    val texts: Seq[Text] = ycg.group.map(c => TextBuilder.fromPhrase(c.text))
    ConfirmationPanel(texts.head, texts.tail)
  }

  private def fromSectionAndNoteGroup(caption: Text, ng: NoteGroup)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    Details(caption, ng.group.map(co => TextBuilder.fromPhrase(co.text)))

  private def fromSectionAndNoteCallout(caption: Text, nc: NoteCallout)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    Details(caption, Seq(TextBuilder.fromPhrase(nc.text)))

}
