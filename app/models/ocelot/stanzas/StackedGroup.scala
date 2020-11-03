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

import models.ocelot.headingCallout

case class StackedGroup (override val next: Seq[String], group: Seq[VisualStanza], stack: Boolean) extends VisualStanza with Populated {
  lazy val containsHeading = group.exists{
    case c: Callout if headingCallout(c) => true
    case _ => false
  }
}

object StackedGroup {
  def apply(group: Seq[VisualStanza]): StackedGroup = StackedGroup(group.last.next, group, group.head.stack)
}