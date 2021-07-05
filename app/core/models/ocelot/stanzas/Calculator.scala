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

// /*
//  * Copyright 2021 HM Revenue & Customs
//  *
//  * Licensed under the Apache License, Version 2.0 (the "License");
//  * you may not use this file except in compliance with the License.
//  * You may obtain a copy of the License at
//  *
//  *     http://www.apache.org/licenses/LICENSE-2.0
//  *
//  * Unless required by applicable law or agreed to in writing, software
//  * distributed under the License is distributed on an "AS IS" BASIS,
//  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  * See the License for the specific language governing permissions and
//  * limitations under the License.
//  */

// package core.models.ocelot.stanzas

// import java.time.LocalDate
// import play.api.Logger
// import core.models.ocelot.{Labels, labelReference, labelScalarValue, asDate, asDecimal}


// trait Operand
// final case class StringOperand(v: String) extends Operand
// final case class DecimalOperand(v: BigDecimal) extends Operand
// final case class DateOperand(v: LocalDate) extends Operand
// final case class ListOperand(v: List[String]) extends Operand


// trait Op {
//   def exec(l: Operand, r: Operand): String
// }

// object OpInstances {
//   implicit val strings: Op =
//     new Op{def exec(l: StringOperand, r: StringOperand): String = "f(l.v, r.v)"}

//   implicit val decimals: Op =
//     new Op{def exec(l: DecimalOperand, r: DecimalOperand): String = "f(l.v, r.v)"}

//   implicit val dates: Op =
//     new Op{def exec(l: DateOperand, r: DateOperand): String = "f(l.v, r.v)"}

//     implicit val lists: Op =
//     new Op{def exec(l: ListOperand, r: ListOperand): String = "f(l.v, r.v)"}

// }

// object Calculator {
//   def run[A](l: Operand, r: Operand)(implicit o: Op): String = o.exec(l, r)
// }

// import OpInstances._

// sealed trait Calculator {
//   val logger: Logger = Logger(this.getClass)

//   def listValue(arg: String, labels: Labels): Option[List[String]] =
//     labelReference(arg).fold[Option[List[String]]](None){ref => labels.valueAsList(ref)}

//   def operands(labels: Labels): String = {

//     def add(l: String, r: String): String = l + r
//     def addD(l: BigDecimal, r: BigDecimal): BigDecimal = l + r


//     Calculator.run(StringOperand("23"), StringOperand("44"))

//     ""
//   }

//   // def operand[_](s: String, labels: Labels): Operand[_] =
//   //   labelScalarValue(s)(labels) match {
//   //     case Some(v) =>
//   //       asDate(v) match {
//   //         case Some(dte) => Operand[LocalDate](dte)
//   //         case _ =>
//   //           asDecimal(v) match {
//   //             case Some(dec) => Operand[BigDecimal](dec)
//   //             case _ => Operand[String](v)
//   //           }
//   //       }
//   //     case _ =>
//   //       listValue(s, labels) match {
//   //         case Some(l) => Operand[List[String]](l)
//   //         case _ => Operand[String](s)
//   //       }
//   //   }

//   // def operand(s: String, labels: Labels): Operand =
//   //   labelScalarValue(s)(labels).fold[Operand](listValue(s, labels).fold[Operand](StringOperand(s))(l => ListOperand(l))){v =>
//   //     asDate(v).fold[Operand]{
//   //       asDecimal(v).fold[Operand](StringOperand(v))(dec => DecimalOperand(dec))
//   //     }(dte => DateOperand(dte))
//   //   }
// }