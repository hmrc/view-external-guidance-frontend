package models

import base.BaseSpec
import core.models.ocelot.{Label, ScalarLabel}
import play.api.libs.json.Json

class LabelOperationSpec extends BaseSpec {

  trait Test {
    val deleteLabelOperation = """{"t":"D","name":"LabelToDelete"}"""
    val updateLabelOperation = """{"t":"U","l":{"type":"scalar","name":"LabelToUpdate","english":[],"welsh":[]}}"""
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
  "Update LabelOperation" must {

    "Serialise to JSON" in new Test {
      val labelOperation: LabelOperation = Update(ScalarLabel("LabelToUpdate"))

      Json.toJson(labelOperation).toString() shouldBe updateLabelOperation
    }

    "Deserialise from JSON" in new Test {
      val labelOperation: LabelOperation = Update(ScalarLabel("LabelToUpdate"))

      Json.parse(updateLabelOperation).as[Update] shouldBe labelOperation
    }
  }
}
