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

package models.admin

import core.models.ocelot._

// $COVERAGE-OFF$

case class LinkedPage(id: String, url: String, title: Option[String])
case class ProcessMapPage(
  id: String,
  url: String,
  title: Option[String],
  keyedStanzas: Seq[KeyedStanza],
  nexts: Seq[LinkedPage],
  links: Seq[LinkedPage],
  linkedFrom: Seq[String]
)