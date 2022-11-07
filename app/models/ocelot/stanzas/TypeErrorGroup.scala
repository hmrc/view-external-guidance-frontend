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

package models.ocelot.stanzas

import core.models.ocelot.stanzas.{TypeErrorCallout, VisualStanza}
import core.models.ocelot.Phrase

case class TypeErrorGroup (override val next: Seq[String], group: Seq[TypeErrorCallout], stack: Boolean) extends VisualStanza {
  override def rendered(expand: Phrase => Phrase): VisualStanza =
    TypeErrorGroup(next, group.map(_.rendered(expand)).collect{case n: TypeErrorCallout => n}, stack)
}

object TypeErrorGroup {
  def apply(group: Seq[TypeErrorCallout]): TypeErrorGroup =
    TypeErrorGroup(group.lastOption.fold[Seq[String]](Seq.empty)(_.next), group, group.headOption.fold[Boolean](false)(_.stack))
}
