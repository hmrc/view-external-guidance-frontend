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

import javax.inject.Singleton
import scala.util.matching.Regex._
import models._
import models.ocelot.stanzas._
import core.models.errors.Error
import core.models.RequestOutcome
import core.models.ocelot.stanzas.{CurrencyInput, CurrencyPoundsOnlyInput, DateInput, Input, Question}
import core.models.ocelot.stanzas._
import core.models.ocelot.{EmbeddedParameterGroup, EmbeddedParameterRegex, Labels, Link, PageReview}
import core.models.ocelot.errors.UnsupportedUiPatternError
import models.ui.{Answer, BulletPointList, ConfirmationPanel, CyaSummaryList, Details, ErrorMsg, H1, H2, H3, H4, InsetText, SequenceAnswer, WarningText}
import models.ui.{NameValueSummaryList, Page, Paragraph, RequiredErrorMsg, Table, Text, TypeErrorMsg, UIComponent, ValueErrorMsg, stackStanzas}
import play.api.Logger
import play.api.i18n.Messages

import scala.annotation.tailrec

case class UIContext(labels: Labels, pageMapById: Map[String, PageDesc], messages: Messages)

sealed trait ErrorStrategy {
  def default(stanzas: Seq[VisualStanza]): ErrorStrategy = this
  val missingFieldNames: List[String] = Nil
  val missingFieldIds: List[String] = Nil
}
case object NoError extends ErrorStrategy

case object ValueMissingError extends ErrorStrategy
case class ValueMissingGroupError(override val missingFieldNames: List[String]) extends ErrorStrategy

case object ValueTypeError extends ErrorStrategy {
  override def default(stanzas: Seq[VisualStanza]): ErrorStrategy =
    stanzas.collectFirst{case s:TypeErrorCallout => s}.fold[ErrorStrategy](ValueMissingError)(_ => this)
}
case class ValueTypeGroupError(override val missingFieldNames: List[String], override val missingFieldIds: List[String]) extends ErrorStrategy {
  override def default(stanzas: Seq[VisualStanza]): ErrorStrategy =
    if (stanzas.collect{case s:TypeErrorCallout => s}.size == 1) ValueTypeError else this
}

@Singleton
class UIBuilder {
  val logger: Logger = Logger(getClass)

  def buildPage(url: String, stanzas: List[VisualStanza], errStrategy: ErrorStrategy = NoError)(implicit ctx: UIContext): RequestOutcome[Page] = {
    val transformPipeline: List[List[VisualStanza] => List[VisualStanza]] = List(Aggregator.aggregateStanzas(Nil), stackStanzas(Nil))
    fromStanzas(transformPipeline.foldLeft(stanzas){case (s, t) => t(s)}, Nil, errStrategy.default(stanzas)) match {
      case Right(stanzas) => Right(Page(url, stanzas))
      case Left(err) => Left(err)
    }
  }

  @tailrec
  private def fromStanzas(stanzas: List[VisualStanza], acc: List[UIComponent], errStrategy: ErrorStrategy)
                         (implicit ctx: UIContext): RequestOutcome[List[UIComponent]] =
    stanzas match {
      case Nil => Right(acc)
      case (sg: StackedGroup) :: xs => sg.group.toList match {
        case (c: SubSectionCallout) :: (rg: RowGroup) :: xsgroup if rg.isTableCandidate =>
          fromStanzas(stackStanzas(Nil)(xsgroup) ++ xs, acc ++ List(fromTableRowGroup(TextBuilder.fromPhrase(c.text), rg)), errStrategy)
        case (c: SubSectionCallout) :: (ng: NoteGroup) :: xsgroup =>
          fromStanzas(stackStanzas(Nil)(xsgroup) ++ xs, acc ++ List(fromSectionAndNoteGroup(TextBuilder.fromPhrase(c.text), ng)), errStrategy)
        case (c: SubSectionCallout) :: (nc: NoteCallout) :: xsgroup =>
          fromStanzas(stackStanzas(Nil)(xsgroup) ++ xs, acc ++ List(fromSectionAndNoteCallout(TextBuilder.fromPhrase(c.text), nc)), errStrategy)
        case x :: xsgroup => fromStanzas(x :: stackStanzas(Nil)(xsgroup) ++ xs, acc, errStrategy)
        case Nil => fromStanzas(xs, acc, errStrategy)
      }
      case (eg: RequiredErrorGroup) :: xs => fromStanzas(xs, acc ++ fromRequiredErrorGroup(eg, errStrategy), errStrategy)
      case (tg: TypeErrorGroup) :: xs => fromStanzas(xs, acc ++ fromTypeErrorGroup(tg, errStrategy), errStrategy)
      case (i: Instruction) :: xs => fromStanzas(xs, acc ++ List(fromInstruction(i)), errStrategy)
      case (ig: InstructionGroup) :: xs => fromStanzas(xs, acc ++ List(fromInstructionGroup(ig)), errStrategy)
      case (rg: RowGroup) :: xs if rg.isCYASummaryList => fromStanzas(xs, acc ++ List(fromCYASummaryListRowGroup(rg)), errStrategy)
      case (rg: RowGroup) :: xs if rg.isNameValueSummaryList => fromStanzas(xs, acc ++ List(fromNameValueSummaryListRowGroup(rg)), errStrategy)
      case (nl: NumberedList) :: xs => fromStanzas(xs, acc ++ List(fromNumberedList(nl)), errStrategy)
      case (nl: NumberedCircleList) :: xs => fromStanzas(xs, acc ++ List(fromNumberedCircleList(nl)), errStrategy)
      case (c: Callout) :: xs => fromStanzas(xs, acc ++ fromCallout(c, errStrategy), errStrategy)
      case (in: Input) :: _ => fromStanzas(Nil, List(fromInput(in, acc)), errStrategy)
      case (q: Question) :: _ => fromStanzas(Nil, List(fromQuestion(q, acc)), errStrategy)
      case (s: Sequence) :: _ => fromStanzas(Nil, List(fromSequence(s, acc)), errStrategy)
      case (ng: NoteGroup) :: xs => fromStanzas(xs, acc ++ List(fromNoteGroup(ng)), errStrategy)
      case (wt: ImportantGroup) :: xs => fromStanzas(xs, acc ++ List(fromImportantGroup(wt)), errStrategy)
      case (ycg: YourCallGroup) :: xs => fromStanzas(xs, acc ++ List(fromYourCallGroup(ycg)), errStrategy)
      case x :: xs if ctx.labels.runMode == PageReview =>
        logger.warn(s"Encountered and ignored (PageReview usage) invalid VisualStanza due to accessibility rules, $x")
        fromStanzas(xs, acc, errStrategy)
      case x :: _ =>
        logger.warn(s"Encountered invalid VisualStanza due to accessibility rules, $x")
        Left(Error(UnsupportedUiPatternError, ctx.labels.runMode, x.next.headOption))
    }

  private def fromCYASummaryListRowGroup(rg: RowGroup)(implicit ctx: UIContext): UIComponent =
    CyaSummaryList(rg.paddedRows.map{row =>
      row.map(phrase => TextBuilder.fromPhrase(phrase)) match {
        // If hint is missing, use first column text as hint
        case Seq(label, value, Text(Seq(l: models.ui.Link))) if l.hint.isEmpty =>
          Seq(label, value, Text(Seq(l.copy(hint = Some(label.asString)))))
        case rowAsText => rowAsText
      }
    })

  private def fromNameValueSummaryListRowGroup(rg: RowGroup)(implicit ctx: UIContext): UIComponent =
    NameValueSummaryList(rg.paddedRows.map(row => row.map(phrase => TextBuilder.fromPhrase(phrase))))

  private def fromTableRowGroup(caption: Text, rg: RowGroup)(implicit ctx: UIContext): UIComponent = {
    val tableRows: Seq[Seq[Text]] = rg.paddedRows.map(r => r.map(phrase => TextBuilder.fromPhrase(phrase)))
    Table(caption, tableRows.head, tableRows.tail)
  }

  private def fromNumberedList(nl: NumberedList)(implicit ctx: UIContext): UIComponent =
    ui.NumberedList(nl.group.map(co => TextBuilder.fromPhrase(co.text)))

  private def fromNumberedCircleList(nl: NumberedCircleList)(implicit ctx: UIContext): UIComponent =
    ui.NumberedCircleList(nl.group.map(co => TextBuilder.fromPhrase(co.text)))

  private def fromInstruction( i:Instruction)(implicit ctx: UIContext): UIComponent =
    i match {
      case Instruction(txt, _, Some(Link(_, dest, _, window)), _, _, _) if Link.isLinkableStanzaId(dest) =>
        Paragraph(Text.link(ctx.pageMapById(dest).url, StringTransform.transform(txt.value(ctx.messages.lang)), window))
      case Instruction(txt, _, Some(Link(_, dest, _, window)), _, _, _) =>
        Paragraph(Text.link(dest, StringTransform.transform(txt.value(ctx.messages.lang)), window))
      case Instruction(txt, _, _, _, _, _) => Paragraph(TextBuilder.fromPhrase(txt))
    }

  private def fromInstructionGroup(grp: InstructionGroup)(implicit ctx: UIContext): UIComponent = {
    val (leading: Text, bullets: Seq[Text]) = BulletPointBuilder.leadingAndBulletText(grp.group.map(_.text))
    BulletPointList(leading, bullets)
  }

  private def fromQuestion(q: Question, components: Seq[UIComponent])(implicit ctx: UIContext): UIComponent = {
    val answers = q.answers.map { ans =>
      val (answer, hint) = TextBuilder.fromPhraseWithOptionalHint(ans)
      Answer(answer, hint)
    }
    // Split out an Error callouts from body components
    val (errorMsgs, uiElements) = partitionComponents(components.toList, List.empty, List.empty)
    val (question, hint) = TextBuilder.fromPhraseWithOptionalHint(q.text)
    ui.Question(question, hint, uiElements, answers, errorMsgs)
  }

  private def fromCallout(co: Callout, errStrategy: ErrorStrategy)(implicit ctx: UIContext): Seq[UIComponent] =
    co match {
      case c: TitleCallout => Seq(H1(TextBuilder.fromPhrase(c.text)))
      case c: SubTitleCallout => Seq(H2(TextBuilder.fromPhrase(c.text)))
      case c: SectionCallout => Seq(H3(TextBuilder.fromPhrase(c.text)))
      case c: SubSectionCallout => Seq(H4(TextBuilder.fromPhrase(c.text)))
      case c: LedeCallout => Seq(Paragraph(TextBuilder.fromPhrase(c.text), lede = true))
      case c: YourCallCallout => Seq(ConfirmationPanel(TextBuilder.fromPhrase(c.text)))
      case c: NoteCallout => Seq(InsetText(Seq(TextBuilder.fromPhrase(c.text))))
      case c: TypeErrorCallout if errStrategy == ValueTypeError => Seq(TypeErrorMsg(TextBuilder.fromPhrase(c.text)))
      case c: ErrorCallout if errStrategy == ValueMissingError => Seq(RequiredErrorMsg(TextBuilder.fromPhrase(c.text)))
      case c: ValueErrorCallout => Seq(ValueErrorMsg(TextBuilder.fromPhrase(c.text)))
      case _: TypeErrorCallout | _: ErrorCallout => Seq.empty   // Consume unmatched Error callouts
      case c: ImportantCallout =>  Seq(WarningText(Seq(TextBuilder.fromPhrase(c.text))))
      case _: NumberedListItemCallout => Seq.empty              // Unused
      case _: NumberedCircleListItemCallout => Seq.empty        // Unused
    }

  private def fromInput(input: Input, components: Seq[UIComponent])(implicit ctx: UIContext): UIComponent = {
    // Split out an Error callouts from body components
    val (errorMsgs, uiElements) = partitionComponents(components.toList, List.empty, List.empty)
    val name = TextBuilder.fromPhrase(input.name)
    val hint = input.help.map(phrase => TextBuilder.fromPhrase(phrase))
    input match {
      case _: TextInput => ui.TextInput(name, hint, uiElements, errorMsgs, input.dontRepeatName, input.width)
      case _: NumberInput => ui.NumberInput(name, hint, uiElements, errorMsgs, input.dontRepeatName, input.width)
      case _: CurrencyInput => ui.CurrencyInput(name, hint, uiElements, errorMsgs, input.dontRepeatName, input.width)
      case _: CurrencyPoundsOnlyInput => ui.CurrencyPoundsOnlyInput(name, hint, uiElements, errorMsgs, input.dontRepeatName, input.width)
      case _: DateInput => ui.DateInput(name, hint, uiElements, errorMsgs, input.dontRepeatName, input.width)
    }
  }

  @tailrec
  private def partitionComponents(components: List[UIComponent], errors: List[ErrorMsg], others: List[UIComponent]): (List[ErrorMsg], List[UIComponent]) =
    components match {
      case Nil => (errors.reverse, others.reverse)
      case (e: ErrorMsg) :: xs => partitionComponents(xs, e +: errors, others)
      case x :: xs => partitionComponents(xs, errors, x +: others)
    }

  private def fromRequiredErrorGroup(eg: RequiredErrorGroup, errStrategy: ErrorStrategy)(implicit ctx: UIContext): Seq[UIComponent] =
    errStrategy match {
      case _: ValueMissingGroupError => errorGroupMsg(eg.group, errStrategy).map(RequiredErrorMsg)
      case _ => Seq()
    }

  private def fromTypeErrorGroup(tg: TypeErrorGroup, errStrategy: ErrorStrategy)(implicit ctx: UIContext): Seq[UIComponent] =
    errStrategy match {
      case _: ValueTypeGroupError => errorGroupMsg(tg.group, errStrategy).map(TypeErrorMsg(_, errStrategy.missingFieldIds))
      case _ => Seq()
    }

  private[services] def errorGroupMsg(callouts: Seq[Callout], errStrategy: ErrorStrategy)(implicit ctx: UIContext): Seq[Text] = {
    // Substitute positional params with the supplied field names
    val mapToFieldName: Match => Option[String] =
      m => Option(m.group(EmbeddedParameterGroup)).map(_.toInt).fold[Option[String]](None)(idx => errStrategy.missingFieldNames.lift(idx))

    callouts.find(co => EmbeddedParameterRegex.findAllMatchIn(co.text.value(ctx.messages.lang)).length == errStrategy.missingFieldNames.length)
            .fold[Seq[Text]](Nil){eco =>
              Seq(Text(StringTransform.transform(EmbeddedParameterRegex.replaceSomeIn(eco.text.value(ctx.messages.lang), mapToFieldName))))
            }
  }

  private def fromNoteGroup(ng: NoteGroup)(implicit ctx: UIContext): UIComponent =
    InsetText(ng.group.map(co => TextBuilder.fromPhrase(co.text)))

  private def fromImportantGroup(wt: ImportantGroup)(implicit ctx: UIContext): UIComponent =
    WarningText(wt.group.map(wc => TextBuilder.fromPhrase(wc.text)))

  private def fromYourCallGroup(ycg: YourCallGroup)(implicit ctx: UIContext): UIComponent = {
    val texts: Seq[Text] = ycg.group.map(c => TextBuilder.fromPhrase(c.text))
    ConfirmationPanel(texts.head, texts.tail)
  }

  private def fromSectionAndNoteGroup(caption: Text, ng: NoteGroup)(implicit ctx: UIContext): UIComponent = {
    val disclosureComponents: Seq[UIComponent] = BulletPointBuilder.groupMatchingPhrases(ng.group.map(_.text)).map{
      case phraseList if phraseList.length == 1 => Paragraph(TextBuilder.fromPhrase(phraseList.head))
      case phraseList =>
        val (leading: Text, bullets: Seq[Text]) = BulletPointBuilder.leadingAndBulletText(phraseList)
        BulletPointList(leading, bullets)
    }

    Details(caption, disclosureComponents)
  }

  private def fromSectionAndNoteCallout(caption: Text, nc: NoteCallout)(implicit ctx: UIContext): UIComponent =
    Details(caption, Seq(Paragraph(TextBuilder.fromPhrase(nc.text))))

  private def fromSequence(s: Sequence, body: Seq[UIComponent])(implicit ctx: UIContext): UIComponent = {
    val exclusiveAnswer = s.exclusive.map(p => SequenceAnswer.tupled(TextBuilder.fromPhraseWithOptionalHint(p)))
    val standardAnswers = s.options.map(p=> SequenceAnswer.tupled(TextBuilder.fromPhraseWithOptionalHint(p)))

    // Split out an Error callouts from body
    val (errorMsgs, uiElements) = partitionComponents(body.toList, List.empty, List.empty)
    val (question, hint) = TextBuilder.fromPhraseWithOptionalHint(s.text)

    ui.Sequence(question, hint, standardAnswers, exclusiveAnswer, uiElements, errorMsgs)
  }
}
