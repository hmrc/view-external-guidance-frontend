/*
 * Copyright 2021 HM Revenue & Customs
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

package base

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.Elements

import play.twirl.api.Html

import scala.collection.JavaConverters._

import org.scalatest.{Matchers, WordSpec}

trait ViewSpec extends WordSpec with Matchers {

  def getSingleElementByTag(markUp: Html, htmlTag: String): Element = {

    val document: Document = Jsoup.parse(markUp.toString())

    getSingleElementByTag(document, htmlTag)
  }

  def getMultipleElementsByTag(markUp: Html, htmlTag: String, expectedNumberOfElements: Int): Elements = {

    val document: Document = Jsoup.parse(markUp.toString())

    getMultipleElementsByTag(document, htmlTag, expectedNumberOfElements)
  }

  def getSingleElementByTag(document: Element, htmlTag: String): Element = {

    val elements: Elements = document.getElementsByTag(htmlTag)

    elements.size() shouldBe 1

    elements.first()
  }

  def getMultipleElementsByTag(document: Document, htmlTag: String, expectedNumberOfElements: Int): Elements = {

    val elements: Elements = document.getElementsByTag(htmlTag)

    elements.size() shouldBe expectedNumberOfElements

    elements
  }

  def getSingleElementByClass(document: Document, clss: String): Element = {

    val elements: Elements = document.getElementsByClass(clss)

    elements.size() shouldBe 1

    elements.first()
  }

  def getElementById(markUp: Html, id: String): Option[Element] = {

    val document: Document = Jsoup.parse(markUp.toString())

    getElementById(document, id)
  }

  def getElementById(doc: Document, id: String) : Option[Element] = Option(doc.getElementById(id))

  def checkClassesForElement(element: Element, expectedClasses: List[String]): Unit = {

    for (expectedClass <- expectedClasses) {
      assert(element.hasClass(expectedClass), "\n Class " + expectedClass + " not found on element " + element.nodeName())
    }

  }

  def checkClassForElement(element: Element, expectedClass: String): Unit = {
    checkClassesForElement(element, List(expectedClass))
  }

  def checkHyperLink(link: Element, destination: String, text: String, targetBlank: Boolean): Unit = {

    val linkAttrs: Map[String, String] = link.attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap

    linkAttrs.contains("href") shouldBe true
    linkAttrs("href") shouldBe destination

    linkAttrs.contains("class") shouldBe true
    linkAttrs("class") shouldBe "govuk-link"

    if (targetBlank) {

      linkAttrs.contains("target") shouldBe true
      linkAttrs("target") shouldBe "_blank"

    } else {
      linkAttrs.contains("target") shouldBe false
    }

    val textNodes = link.textNodes().asScala

    textNodes.size shouldBe 1

    textNodes.head.text().trim shouldBe text
  }

}
