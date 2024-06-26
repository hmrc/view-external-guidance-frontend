/*
 * Copyright 2024 HM Revenue & Customs
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

package core.models.ocelot.errors

import play.api.libs.json._

sealed trait EGError
sealed trait GuidanceError extends EGError
sealed trait MetaError extends GuidanceError
sealed trait FlowError extends GuidanceError
sealed trait PhrasesError extends GuidanceError
sealed trait LinksError extends GuidanceError
sealed trait TimescalesError extends GuidanceError
sealed trait RatesError extends GuidanceError
sealed trait RuntimeError extends EGError

case class UnsupportedOperationError(op: String, lvalue: Option[String], rvalue: Option[String], left: String, right: String) extends RuntimeError
case object NonTerminatingPageError extends RuntimeError
case object UnsupportedUiPatternError extends RuntimeError
case class ProgrammingError(msg: String) extends RuntimeError
case class DivideByZeroError(left: String, right: String) extends RuntimeError

// General and section parse errors
case class ParseError(jsPath: JsPath, errs: Seq[JsonValidationError]) extends GuidanceError
case class FlowParseError(id: String, msg: String, arg: String) extends FlowError
case class MetaParseError(id: String, msg: String, arg: String) extends MetaError
case class PhrasesParseError(id: String, msg: String, arg: String) extends PhrasesError
case class LinksParseError(id: String, msg: String, arg: String) extends LinksError
case class TimescalesParseError(id: String, msg: String, arg: String) extends TimescalesError
case class RatesParseError(id: String, msg: String, arg: String) extends RatesError
// Guidance Flow errors
case class UnknownStanza(id: String, typeName: String) extends FlowError
case class UnknownCalloutType(id: String, typeName: String) extends FlowError
case class UnknownValueType(id: String, typeName: String) extends FlowError
case class UnknownCalcOperationType(id: String, typeName: String) extends FlowError
case class UnknownTestType(id: String, typeName: String) extends FlowError
case class UnknownInputType(id: String, typeName: String) extends FlowError
case class StanzaNotFound(id: String) extends FlowError
case class PageStanzaMissing(id: String) extends FlowError
case class PageUrlEmptyOrInvalid(id: String) extends FlowError
case class PhraseNotFound(id: String, index: Int) extends FlowError
case class LinkNotFound(id: String, index: Int) extends FlowError
case class DuplicatePageUrl(id: String, url: String) extends FlowError
case class InconsistentQuestion(id: String) extends FlowError
case class MissingWelshText(id: String, index: String, english: String) extends FlowError
case class VisualStanzasAfterDataInput(id: String) extends FlowError
case class IncompleteDateInputPage(id: String) extends FlowError
case class IncompleteExclusiveSequencePage(id: String) extends FlowError
case class PageRedirectNotSupported(id: String) extends FlowError
case class UseOfReservedUrl(id: String) extends FlowError
case class PageOccursInMultiplSequenceFlows(id: String) extends FlowError
case class MultipleExclusiveOptions(id: String) extends FlowError
case class ErrorRedirectToFirstNonPageStanzaOnly(id: String) extends FlowError
case class InvalidLabelName(id: String) extends FlowError
case class InvalidFieldWidth(id: String) extends FlowError
case class MissingTimescaleDefinition(timescaleId: String) extends TimescalesError
case class MissingRateDefinition(rateId: String) extends RatesError
case class IncompleteInputPage(id: String) extends FlowError
case class MissingTitle(id: String) extends FlowError
case class AllFlowsMustContainMultiplePages(id: String) extends FlowError
case class LanguageLinkIdsDiffer(id: String) extends FlowError

object GuidanceError {

  // Some flow errors add a hint the to JsonValidationError message to indicate that an
  // unsupported type/stanza or option has been found. Other validation errors are
  // converted to a general parse error for the containing section
  def fromJsonValidationError(err: (JsPath, scala.collection.Seq[JsonValidationError])): GuidanceError = {

    def flowError(jsPath: JsPath, id: String, arg: String, msg: String, msgs: Seq[String]): FlowError =
      msgs.headOption.collect{
        case "CalloutType" => UnknownCalloutType(id, arg)
        case "Stanza" => UnknownStanza(id, arg)
        case "ValueType" => UnknownValueType(id, arg)
        case "TestType" => UnknownTestType(id, arg)
        case "InputType" => UnknownInputType(id, arg)
        case "CalcOperationType" => UnknownCalcOperationType(id, arg)
      }.getOrElse(FlowParseError(id, msg, jsPath.toString))

    val (jsPath: JsPath, errs) = err
    val id = (jsPath.path.lift(1).fold("/unknown")(pathNode => pathNode.toString)).drop(1)
    val mainError = errs.head
    val arg = mainError.args.headOption.fold("")(_.toString)
    jsPath.path.headOption.fold("/unknown")(pathNode => pathNode.toString) match {
      case "/flow" => flowError(jsPath, id, arg, mainError.message, mainError.messages)
      case "/meta" => MetaParseError(id, mainError.message, arg)
      case "/phrases" => PhrasesParseError(id.dropRight(1), mainError.message, arg)
      case "/links" => LinksParseError(id.dropRight(1), mainError.message, arg)
      case "/timescales" => TimescalesParseError(id.dropRight(1), mainError.message, arg)
      case "/rates" => RatesParseError(id.dropRight(1), mainError.message, arg)
    }
  }

  def fromJsonValidationErrors(jsErrors: scala.collection.Seq[(JsPath, scala.collection.Seq[JsonValidationError])]): List[GuidanceError] =
    jsErrors.map(fromJsonValidationError).toList
}
