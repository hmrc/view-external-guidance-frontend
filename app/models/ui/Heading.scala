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

package models.ui

abstract class Heading( english: String, welsh: String ) extends UIComponent {

  val text: Text = Text( english, welsh )

}

case class H1( english: String, welsh: String ) extends Heading( english, welsh )

case class H2( english: String, welsh: String  ) extends Heading( english, welsh )

case class H3( english: String, welsh: String ) extends Heading( english, welsh )
