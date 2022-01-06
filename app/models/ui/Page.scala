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

package models.ui

sealed trait Page {
  val heading: Text
  val urlPath: String
  val components: Seq[UIComponent]
  val relativePath: String = urlPath.dropWhile(_ == '/')
  lazy val reduceHeadings: Boolean = components.exists{
    case c: SummaryList => true
    case t: Table => true
    case _ => false
  }
}

object Page {
  def apply(urlPath: String, components: Seq[UIComponent]): Page = {
    components match {
      case (fc: FormComponent) :: _ => FormPage(urlPath, fc)
      case _ => StandardPage(urlPath, components)
    }
  }
}

case class StandardPage(urlPath: String, components: Seq[UIComponent]) extends Page {
  val heading: Text = components
    .find {
      case _: H1 => true
      case _: ConfirmationPanel => true
      case _ => false
    }
    .fold(Text())(_.text)
}

case class FormPage(urlPath: String, formComponent: FormComponent) extends Page {
  val heading: Text = formComponent.text
  val components: Seq[UIComponent] = Seq(formComponent)
}
