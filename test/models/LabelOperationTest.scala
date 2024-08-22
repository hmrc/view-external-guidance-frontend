package models

import base.BaseSpec
import play.api.libs.json.Json

class LabelOperationTest extends BaseSpec {

  trait Test {
    val deleteLabelOperation = """{"action":"D","name":"LabelToDelete"}"""
  }

  "Delete LabelOperation" must {

    "Serialise to JSON" in new Test {
      val labelOperation: LabelOperation = Delete("LabelToDelete")

      Json.toJson(labelOperation).toString() shouldBe deleteLabelOperation
    }

    "Deserialise from JSON" in new Test {
      val labelOperation: LabelOperation = Delete("LabelToDelete")

      Json.parse(deleteLabelOperation).as[Delete] shouldBe labelOperation
    }
  }
}
