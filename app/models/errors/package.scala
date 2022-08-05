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

package models

import core.models.ocelot.RunMode
import core.models.errors.Error
import core.models.ocelot.errors._

package object errors {

  def fromRuntimeError(err: RuntimeError, stanzaId: String): String = err match {
    case e: UnsupportedOperationError => s"UnsupportedOperationError: Operation ${e.op}, left ${e.left} (${e.lvalue}), right ${e.right} (${e.rvalue}) on stanza $stanzaId"
    case NonTerminatingPageError => s"NonTerminatingPageError: Guidance contains non-terminating loop which includes stanza $stanzaId"
    case UnsupportedUiPatternError => s"UnsupportedUiPatternError: Stanza grouping (including stanza $stanzaId) does not form a GDS UI pattern"
  }

  def executionError(errs: List[RuntimeError], stanzId: String, runMode: RunMode): Error = Error(Error.ExecutionError, errs, Some(runMode), Some(stanzId))
  def executionError(err: RuntimeError, stanzId: String, runMode: RunMode): Error = executionError(List(err), stanzId, runMode)
}