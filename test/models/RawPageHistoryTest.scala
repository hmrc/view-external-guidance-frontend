/*
 * Copyright 2024 HM Revenue & Customs
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

import base.BaseSpec
import core.models.ocelot.{Delete, ScalarLabel, Update}
import play.api.libs.json.Json

class RawPageHistoryTest extends BaseSpec {

  trait Test {
    val rawPageHistoryWithNonEmptyRevertOperationsJson =
      """
        |{
        |  "stanzId":"aStanzId",
        |  "revertOps":[
        |    {"t":"D","name":"LabelToDelete"},
        |    {"t":"U","l":{"type":"scalar","name":"LabelToUpdate",
        |    "english":[],"welsh":[]}},
        |    {"t":"D","name":"AnotherLabelToDelete"}
        |  ],
        |  "flowStack":[]
        |}
        |  """
    val rawPageHistoryWithEmptyRevertOperationsJson =
    """
      |{
      |  "stanzId":"aStanzId",
      |  "revertOps":[],
      |  "flowStack":[]
      |}
    """
    val rawPageHistoryWithMissingRevertOperationsJson =
    """
        |{
        |  "stanzId":"aStanzId",
        |  "flowStack":[]
        |}"""
    val rawPageHistoryWithNonEmptyRevertOperation = RawPageHistory("aStanzId", List(Delete("LabelToDelete"), Update(ScalarLabel("LabelToUpdate")), Delete("AnotherLabelToDelete")), Nil)
    val rawPageHistoryWithEmptyRevertOperation = RawPageHistory("aStanzId", Nil, Nil)
  }

  "A raw page history" must {

    "Serialise to JSON when having revert operations" in new Test {
      Json.toJson(rawPageHistoryWithNonEmptyRevertOperation) shouldBe Json.parse(rawPageHistoryWithNonEmptyRevertOperationsJson.stripMargin)
    }

    "Deserialise from JSON when having revert operations" in new Test {
      Json.parse(rawPageHistoryWithNonEmptyRevertOperationsJson.stripMargin).as[RawPageHistory] shouldBe rawPageHistoryWithNonEmptyRevertOperation
    }

    "Serialise to JSON when having empty revert operations" in new Test {
      Json.toJson(rawPageHistoryWithEmptyRevertOperation) shouldBe Json.parse(rawPageHistoryWithEmptyRevertOperationsJson.stripMargin)
    }

    "Deserialise from JSON when having empty revert operations" in new Test {
      Json.parse(rawPageHistoryWithEmptyRevertOperationsJson.stripMargin).as[RawPageHistory] shouldBe rawPageHistoryWithEmptyRevertOperation
    }

    "Deserialise from JSON when having no revert operations" in new Test {
      Json.parse(rawPageHistoryWithMissingRevertOperationsJson.stripMargin).as[RawPageHistory] shouldBe rawPageHistoryWithEmptyRevertOperation
    }
  }

}
