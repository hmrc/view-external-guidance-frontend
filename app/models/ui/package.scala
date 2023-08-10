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

package models

import scala.language.implicitConversions
import core.models.ocelot.Phrase
import core.models.ocelot.stanzas.VisualStanza
import models.ocelot.stanzas.StackedGroup
import play.api.i18n.Lang
import scala.annotation.tailrec

package object ui {
  val PreviousPageLinkQuery: String = "p=1"

  implicit def toText(p: Phrase)(implicit lang: Lang): Text = Text(p.value(lang))

  @tailrec
  def stackStanzas(acc: Seq[Seq[VisualStanza]])(stanzas: Seq[VisualStanza]): Seq[VisualStanza] =
    (stanzas: @unchecked) match {
      case Nil => acc.collect{
        case s if s.length > 1 => StackedGroup(s)
        case s => s.head
      }
      case x :: xs if acc.isEmpty => stackStanzas(Seq(Seq(x)))(xs)
      case x :: xs if x.stack => stackStanzas(acc.init :+ (acc.last :+ x))(xs)
      case x :: xs => stackStanzas(acc :+ Seq(x))(xs)
    }
}
