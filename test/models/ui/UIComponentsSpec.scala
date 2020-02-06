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

import base.BaseSpec
import play.api.i18n.Lang

class UIComponentsSpec extends BaseSpec {

  "UIComponents" must {

    "return appropriate lang text from Text and ParagraphText given Lang welsh" in {
      val welsh = "Welsh, Hello my name is ...."
      val english = "Hello my name is ...."
      val txt1 = ParagraphText(english, welsh)
      val txt2 = Text(english, welsh)

      implicit val lang: Lang = Lang("cy")

      txt1.value mustBe welsh
      txt2.value mustBe welsh
    }

    "return appropriate lang text from Text and ParagraphText given Lang english" in {
      val welsh = "Welsh, Hello my name is ...."
      val english = "Hello my name is ...."
      val txt1 = ParagraphText(english, welsh)
      val txt2 = Text(english, welsh)

      implicit val lang: Lang = Lang("en")

      txt1.value mustBe english
      txt2.value mustBe english

    }

    "return appropriate lang text from Text and ParagraphText given Lang unknown" in {
      val welsh = "Welsh, Hello my name is ...."
      val english = "Hello my name is ...."
      val txt1 = ParagraphText(english, welsh)
      val txt2 = Text(english, welsh)

      implicit val lang: Lang = Lang("jp")

      txt1.value mustBe english
      txt2.value mustBe english

    }

    "build into a page of text only paragraph and question" in {

      val langs1 = Vector("Hello my name is ....","Welsh, Hello my name is ....")
      val langs4 = Vector("Not bad", "Welsh, Not bad")
      val langs5 = Vector("you think this is not bad", "Welsh, you think this is not bad")
      val langs6 = Vector("ok","Welsh, ok")
      val langs7 = Vector("you think this is not bad", "Wels,you think this is not bad")
      val langs8 = Vector("What do you think of this example","Welsh, What do you think of this example")

      val txt1 = ParagraphText(langs1(0), langs1(1))
      val para = Paragraph(Seq(txt1))

      val answer1 = Answer(Text(langs4(0), langs4(1)), Some(Text(langs5(0), langs5(1))), "/firstpage/notbad")
      val answer2 = Answer(Text(langs6(0), langs6(1)), Some(Text(langs7(0), langs7(1))), "/firstpage/notbad")
      val answers = AnswerGroup(Seq(answer1, answer2), false)
      val question = Question(Text(langs8(0), langs8(1)), None, answers)

      val components = Seq(para, question)
      val page = Page("/firstpage", components)

      page.components.length mustBe 2

      page.components.foreach{ cmp =>
        cmp match {
          case p:Paragraph =>
            p.items.length mustBe 1
          case q: Question =>
            q.answerGroup.answers.length mustBe 2
        }
      }
    }

    "build into a page of text and link paragraph" in {

      val langs1 = Vector("Hello my name is ....","Welsh, Hello my name is ....")
      val langs2 = Vector(" and today is Wednesday","Welsh,  and today is Wednesday")
      val langs3 = Vector("Unsure?", "Welsh, Unsure?")
      val langs4 = Vector("Not bad", "Welsh, Not bad")
      val langs5 = Vector("you think this is not bad", "Welsh, you think this is not bad")
      val langs6 = Vector("ok","Welsh, ok")
      val langs7 = Vector("you think this is not bad", "Wels,you think this is not bad")
      val langs8 = Vector("What do you think of this example","Welsh, What do you think of this example")

      val txt1 = ParagraphText(langs1(0), langs1(1))
      val txt2 = ParagraphText(langs2(0), langs2(1))
      val link1 = ParagraphLink("/secondpage",Text(langs3(0), langs3(1)))
      val paraItems = Seq(txt1,link1,txt2)
      val paraWithLink = Paragraph(paraItems)

      val components = Seq(paraWithLink)
      val page = Page("/firstpage", components)

      page.components.length mustBe 1
      page.components.foreach{ cmp =>
        cmp match {
          case p:Paragraph =>
            p.items.length mustBe 3
        }
      }
    }

  }
}
