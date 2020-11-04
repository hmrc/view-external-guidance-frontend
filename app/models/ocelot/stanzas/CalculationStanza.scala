/*
 * Copyright 2020 HM Revenue & Customs
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

package models.ocelot.stanzas

import models.ocelot.{asCurrency, labelReference, labelReferences, Label, Labels}
import play.api.Logger
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Reads._

case class CalculationStanza(calcs: Seq[CalcOperation], override val next: Seq[String], stack: Boolean) extends Stanza {
  override val labels: List[Label] = calcs.map(op => Label(op.label)).toList
  override val labelRefs: List[String] = calcs.flatMap(op => labelReferences(op.left) ++ labelReferences(op.right)).toList
}

object CalculationStanza {

  implicit val calculationReads: Reads[CalculationStanza] =
    (
      (JsPath \ "calcs").read[Seq[CalcOperation]](minLength[Seq[CalcOperation]](1)) and
        (JsPath \ "next").read[Seq[String]](minLength[Seq[String]](1)) and
        (JsPath \ "stack").read[Boolean]
    )(CalculationStanza.apply _)

  implicit val calculationWrites: OWrites[CalculationStanza] =
    (
      (JsPath \ "calcs").write[Seq[CalcOperation]] and
        (JsPath \ "next").write[Seq[String]] and
        (JsPath \ "stack").write[Boolean]
      )(unlift(CalculationStanza.unapply))
}

sealed trait Operation {

  val logger: Logger = Logger(this.getClass)

  val left: String
  val right: String
  val label: String

  def eval(labels: Labels) : Labels

  def value(arg: String, labels: Labels): String = labelReference(arg).fold(arg){ref => labels.value(ref).getOrElse("")}

  def addString(s1: String, s2: String) : Option[String] = Option(s1 + s2)

  def subtractString(s1: String, s2: String) : Option[String] = {

    logger.error("Illegal string subtraction operation defined in guidance configuration")

    None
  }

  def op(f: (BigDecimal, BigDecimal) => BigDecimal, g: (String, String) => Option[String], labels: Labels) : Labels = {

    val x: String = value(left, labels)
    val y: String = value(right, labels)

    (asCurrency(x), asCurrency(y)) match {
      case (Some(bg1), Some(bg2)) => {

        val bg3 = f(bg1, bg2)

        labels.update(label, bg3.bigDecimal.toPlainString)

      }
      case _ =>
        // Treat both operands as strings
        g(x,y) match {
          case Some(value) => labels.update(label, value)
          case None => labels
      }
    }

  }

}

case class Add(left: String, right: String, label: String) extends Operation {

  def eval(labels: Labels): Labels = op(_ + _, addString, labels)

}

case class Subtract(left: String, right: String, label: String) extends Operation {

  def eval(labels: Labels): Labels = op(_ - _, subtractString, labels)

}

case class Calculation(override val next: Seq[String], calcs: Seq[Operation]) extends Stanza with Evaluate {

  override val labels: List[Label] = calcs.map(op => Label(op.label)).toList
  override val labelRefs: List[String] = calcs.flatMap(op => labelReferences(op.left) ++ labelReferences(op.right)).toList

  def eval(labels: Labels): (String, Labels) = {

    val updatedLabels: Labels = calcs.foldLeft(labels){case(l, f) => f.eval(l)}

    (next.last, updatedLabels)
  }

}

object Calculation {

  def apply(stanza: CalculationStanza): Calculation =

    Calculation(
      stanza.next,
      stanza.calcs.map{ c =>
        c.op match {
          case Addition => Add(c.left, c.right, c.label)
          case Subtraction => Subtract(c.left, c.right, c.label)
        }
      }
    )
}