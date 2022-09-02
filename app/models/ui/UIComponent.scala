/*
 * Copyright 2022 HM Revenue & Customs
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

import core.models.ocelot.asAnyInt
trait UIComponent {
  val text: Text
}

//
// Monolingual
//
sealed trait TextItem {
  def isEmpty: Boolean
  def toWords: Seq[String]
}

case class Words(s: String, bold: Boolean = false) extends TextItem {
  def isEmpty: Boolean = s.isEmpty
  def toWords: Seq[String] = s.split(" +").toSeq
  override def toString: String = s
}

case class Link(dest: String, text: String, window: Boolean = false, asButton: Boolean = false, hint:Option[String] = None) extends TextItem {
  override def toString: String = s"[${if(asButton) "button" else "link"}:$text:$dest:$window:$hint]"
  def isEmpty: Boolean = text.isEmpty
  def toWords: Seq[String] = text.split(" +").toSeq
  def getDest(backLink: Option[String]): String =
    backLink match {
      case Some(bl) if dest == bl => s"$dest?$PreviousPageLinkQuery"
      case _ => dest
    }
}

case class Text(items: Seq[TextItem]) {
  def isEmpty: Boolean = items.isEmpty
  def toWords: Seq[String] = items.flatMap(_.toWords)
  def asString: String = toWords.mkString(" ")
  override def toString: String = s"[${items.map(t => t.toString).mkString("")}]"
  def +(other: Text): Text = Text(items ++ other.items)
  lazy val isBold: Boolean = items.length == 1 && (items.head match {
    case w: Words => w.bold
    case _ => false
  })
  lazy val isNumeric: Boolean = items.length == 1 && (items.head match {
    case w: Words => asAnyInt(w.s.trim).isDefined
    case _ => false
  })
}

object Text {
  def apply(item: TextItem): Text = Text(Seq(item))
  def apply(itemText: String): Text = Text(Words(itemText))
  def apply(): Text = Text(Nil)
  //def labelRef(name: String): Text = Text(LabelRef(name))
  def link(dest: String, phraseString: String, window: Boolean = false, asButton: Boolean = false, hint: Option[String] = None): Text =
    Text(Link(dest, phraseString, window, asButton, hint.map(h => h)))
}
