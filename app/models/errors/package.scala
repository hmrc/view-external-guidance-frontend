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
import core.models.errors.{ErrorReport, Error}
import core.models.ocelot.errors._

package object errors {

  private def fromRuntimeError(err: RuntimeError, stanzId: String): ErrorReport = err match {
    case e: UnsupportedOperationError => ErrorReport(s"UnsupportedOperationError: Operation ${e.op}, left ${e.left} (${e.lvalue}), right ${e.right} (${e.rvalue}) on stanza $stanzId", stanzId)
    case e: NonTerminatingPageError => ErrorReport(s"NonTerminatingPageError: Guidance contains non-terminating loop which includes stanza $stanzId", stanzId)
  }
  private def fromRuntimeErrors(errs: List[RuntimeError], stanzId: String): List[ErrorReport] = errs.map(e => fromRuntimeError(e,stanzId))

  def executionError(errs: List[RuntimeError], stanzId: String, runMode: RunMode): Error =
    Error(Error.ExecutionError, Some(fromRuntimeErrors(errs, stanzId)), Some(runMode))
  def executionError(err: RuntimeError, stanzId: String, runMode: RunMode): Error = executionError(List(err), stanzId, runMode)

}
