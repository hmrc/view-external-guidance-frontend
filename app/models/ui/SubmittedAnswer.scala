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

package models.ui

trait SubmittedAnswer {
  val text: String
}

case class StringAnswer(text: String) extends SubmittedAnswer

case class PassphraseAnswer(text: String) extends SubmittedAnswer

case class DateAnswer(day: String, month: String, year: String) extends SubmittedAnswer {
  override val text: String = day + "/" + month + "/" + year
}

case class ListAnswer(items: List[String]) extends SubmittedAnswer {
  override val text: String = items.mkString(",")
}
