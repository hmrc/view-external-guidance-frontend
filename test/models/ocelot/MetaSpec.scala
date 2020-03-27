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

package models.ocelot

import base.BaseSpec
import play.api.libs.json._

class MetaSpec extends BaseSpec {

  val title = "Process title"
  val id = "abc90001"
  val ocelotVersion = 1
  val author = "1234567"
  val lastUpdate = 1500298931016L
  val version = 2
  val filename = s"$id.js"

  val validJson: JsObject = Json
    .parse(
      s"""
       |{
       |   "title": "$title",
       |   "id": "$id",
       |   "ocelot": $ocelotVersion,
       |   "lastAuthor": "$author",
       |   "lastUpdate": $lastUpdate,
       |   "version": $version,
       |   "filename": "$filename"
       |}
       |""".stripMargin
    )
    .as[JsObject]

  val validModel: Meta = Meta(id, title, ocelotVersion, author, lastUpdate, version, filename)

  "Meta section" must {

    "deserialise correctly" in {
      val result: Meta = validJson.as[Meta]
      result mustBe validModel
    }

    missingJsObjectAttrTests[Meta](validJson)
  }

}
