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
