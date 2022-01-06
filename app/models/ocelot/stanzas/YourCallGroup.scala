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

import core.models.ocelot.stanzas.{YourCallCallout, VisualStanza, Populated}

case class YourCallGroup(override val next: Seq[String], group: Seq[YourCallCallout], stack: Boolean) extends VisualStanza with Populated

object YourCallGroup {
  def apply(group: Seq[YourCallCallout]): YourCallGroup =
    group match {
      case Nil => YourCallGroup(Seq.empty, Seq.empty, false)
      case _ => YourCallGroup(group.last.next, group, group.head.stack)
    }
}
