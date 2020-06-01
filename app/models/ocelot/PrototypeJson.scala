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

// $COVERAGE-OFF$Scratch data

package models.ocelot

//
// All Welsh translations are temporary placeholders and for testing purposes only
//
object PrototypeJson {

  val json =
    """
    {
      "meta": {
        "id": "ext90002",
        "title": "Telling HMRC about extra income",
        "ocelot": 1,
        "lastAuthor": "7902560",
        "lastUpdate": 1582651445320,
        "version": 9,
        "filename": "ext90002.js"
      },
      "flow": {
        "45": {
          "next": [
            "46",
            "53"
          ],
          "stack": false,
          "answers": [
            27,
            28
          ],
          "text": 38,
          "type": "QuestionStanza"
        },
        "98": {
          "next": [
            "99"
          ],
          "stack": true,
          "text": 58,
          "type": "InstructionStanza"
        },
        "113": {
          "next": [
            "114"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/you-need-to-tell-hmrc-rent-a-room2"
        },
        "34": {
          "next": [
            "35"
          ],
          "stack": true,
          "text": 31,
          "type": "InstructionStanza"
        },
        "67": {
          "next": [
            "68"
          ],
          "stack": true,
          "text": 48,
          "type": "InstructionStanza"
        },
        "120": {
          "next": [
            "79"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/did-you-share-the-income"
        },
        "153": {
          "next": [
            "154"
          ],
          "stack": true,
          "text": 71,
          "type": "InstructionStanza"
        },
        "93": {
          "next": [
            "94"
          ],
          "stack": true,
          "text": 36,
          "type": "InstructionStanza"
        },
        "158": {
          "next": [
            "158a"
          ],
          "stack": true,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/you-need-to-tell-hmrc-or-contact/final158"
        },
        "142": {
          "next": [
            "143",
            "158"
          ],
          "stack": false,
          "answers": [
            65,
            66
          ],
          "text": 25,
          "type": "QuestionStanza"
        },
        "147": {
          "next": [
            "148"
          ],
          "stack": true,
          "text": 36,
          "type": "InstructionStanza"
        },
        "12": {
          "next": [
            "13"
          ],
          "stack": true,
          "text": 12,
          "type": "InstructionStanza"
        },
        "66": {
          "next": [
            "67"
          ],
          "stack": true,
          "text": 54,
          "type": "InstructionStanza"
        },
        "89": {
          "next": [
            "end"
          ],
          "stack": true,
          "text": 44,
          "type": "InstructionStanza"
        },
        "51": {
          "next": [
            "52"
          ],
          "stack": true,
          "text": 43,
          "type": "InstructionStanza"
        },
        "124": {
          "next": [
            "125"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/did-you-share-the-income-no-income"
        },
        "84": {
          "next": [
            "85"
          ],
          "noteType": "Title",
          "stack": false,
          "text": 45,
          "type": "CalloutStanza"
        },
        "8": {
          "next": [
            "9"
          ],
          "stack": true,
          "text": 8,
          "type": "InstructionStanza"
        },
        "158a": {
          "next": [
            "end"
          ],
          "stack": true,
          "text": 64,
          "type": "InstructionStanza"
        },
        "78": {
          "next": [
            "120",
            "109"
          ],
          "stack": false,
          "answers": [
            27,
            28
          ],
          "text": 33,
          "type": "QuestionStanza"
        },
        "19": {
          "next": [
            "20"
          ],
          "noteType": "Section",
          "stack": false,
          "text": 18,
          "type": "CalloutStanza"
        },
        "100": {
          "next": [
            "101"
          ],
          "stack": true,
          "text": 59,
          "type": "InstructionStanza"
        },
        "23": {
          "next": [
            "24"
          ],
          "stack": true,
          "text": 22,
          "type": "InstructionStanza"
        },
        "62": {
          "next": [
            "63"
          ],
          "stack": true,
          "text": 50,
          "type": "InstructionStanza"
        },
        "135": {
          "next": [
            "136"
          ],
          "stack": true,
          "text": 36,
          "type": "InstructionStanza"
        },
        "128": {
          "next": [
            "129"
          ],
          "stack": true,
          "text": 61,
          "type": "InstructionStanza"
        },
        "4": {
          "next": [
            "5"
          ],
          "stack": true,
          "text": 4,
          "type": "InstructionStanza"
        },
        "121": {
          "next": [
            "122"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/have-you-rented-out-a-room-in-your-home-no-income"
        },
        "88": {
          "next": [
            "89"
          ],
          "stack": true,
          "text": 49,
          "type": "InstructionStanza"
        },
        "77": {
          "next": [
            "78"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/have-you-rented-out-a-room-in-your-home"
        },
        "40": {
          "next": [
            "45"
          ],
          "stack": true,
          "text": 37,
          "type": "InstructionStanza"
        },
        "110": {
          "next": [
            "111"
          ],
          "noteType": "Title",
          "stack": false,
          "text": 57,
          "type": "CalloutStanza"
        },
        "15": {
          "next": [
            "16"
          ],
          "stack": true,
          "text": 5,
          "type": "InstructionStanza"
        },
        "11": {
          "next": [
            "12"
          ],
          "noteType": "Section",
          "stack": false,
          "text": 11,
          "type": "CalloutStanza"
        },
        "104": {
          "next": [
            "105"
          ],
          "stack": true,
          "text": 46,
          "type": "InstructionStanza"
        },
        "90": {
          "next": [
            "91"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/do-you-want-to-use-the-rent-a-room-scheme"
        },
        "9": {
          "next": [
            "10"
          ],
          "stack": true,
          "text": 9,
          "type": "InstructionStanza"
        },
        "141": {
          "next": [
            "142"
          ],
          "noteType": "Error",
          "stack": false,
          "text": 63,
          "type": "CalloutStanza"
        },
        "139": {
          "next": [
            "140"
          ],
          "stack": true,
          "text": 61,
          "type": "InstructionStanza"
        },
        "132": {
          "next": [
            "133"
          ],
          "stack": true,
          "text": 34,
          "type": "InstructionStanza"
        },
        "44": {
          "next": [
            "40"
          ],
          "stack": true,
          "text": 36,
          "type": "InstructionStanza"
        },
        "33": {
          "next": [
            "34"
          ],
          "stack": true,
          "text": 24,
          "type": "InstructionStanza"
        },
        "117": {
          "next": [
            "118"
          ],
          "stack": true,
          "text": 48,
          "type": "InstructionStanza"
        },
        "22": {
          "next": [
            "23"
          ],
          "stack": true,
          "text": 21,
          "type": "InstructionStanza"
        },
        "56": {
          "next": [
            "57"
          ],
          "stack": true,
          "text": 47,
          "type": "InstructionStanza"
        },
        "55": {
          "next": [
            "56"
          ],
          "stack": true,
          "text": 46,
          "type": "InstructionStanza"
        },
        "26": {
          "next": [
            "2777"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/have-you-made-less-than-1000"
        },
        "134": {
          "next": [
            "135"
          ],
          "stack": true,
          "text": 36,
          "type": "InstructionStanza"
        },
        "50": {
          "next": [
            "51"
          ],
          "stack": true,
          "text": 42,
          "type": "InstructionStanza"
        },
        "123": {
          "next": [
            "124",
            "138"
          ],
          "stack": false,
          "answers": [
            27,
            28
          ],
          "text": 33,
          "type": "QuestionStanza"
        },
        "37": {
          "next": [
            "38"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/less-than-1000/have-you-rented-out-a-room-in-your-home"
        },
        "68": {
          "next": [
            "69"
          ],
          "stack": true,
          "text": 55,
          "type": "InstructionStanza"
        },
        "61": {
          "next": [
            "62"
          ],
          "noteType": "Title",
          "stack": false,
          "text": 39,
          "type": "CalloutStanza"
        },
        "107": {
          "next": [
            "108"
          ],
          "stack": true,
          "text": 49,
          "type": "InstructionStanza"
        },
        "13": {
          "next": [
            "14"
          ],
          "stack": true,
          "text": 13,
          "type": "InstructionStanza"
        },
        "46": {
          "next": [
            "47"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/less-than-1000/you-do-not-need-to-tell-hmrc-but-SA"
        },
        "99": {
          "next": [
            "100"
          ],
          "stack": true,
          "text": 42,
          "type": "InstructionStanza"
        },
        "24": {
          "next": [
            "25"
          ],
          "stack": true,
          "text": 5,
          "type": "InstructionStanza"
        },
        "155": {
          "next": [
            "156"
          ],
          "stack": true,
          "text": 73,
          "type": "InstructionStanza"
        },
        "94": {
          "next": [
            "95"
          ],
          "stack": true,
          "text": 36,
          "type": "InstructionStanza"
        },
        "83": {
          "next": [
            "84"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/you-need-to-tell-hmrc-rent-a-room"
        },
        "35": {
          "next": [
            "37",
            "60"
          ],
          "stack": false,
          "answers": [
            27,
            28
          ],
          "text": 32,
          "type": "QuestionStanza"
        },
        "16": {
          "next": [
            "17"
          ],
          "stack": true,
          "link": 0,
          "text": 15,
          "type": "InstructionStanza"
        },
        "79": {
          "next": [
            "80",
            "109"
          ],
          "stack": false,
          "answers": [
            27,
            28
          ],
          "text": 60,
          "type": "QuestionStanza"
        },
        "152": {
          "next": [
            "153"
          ],
          "noteType": "Title",
          "stack": false,
          "text": 70,
          "type": "CalloutStanza"
        },
        "5": {
          "next": [
            "6"
          ],
          "stack": true,
          "text": 5,
          "type": "InstructionStanza"
        },
        "103": {
          "next": [
            "104"
          ],
          "noteType": "Title",
          "stack": false,
          "text": 45,
          "type": "CalloutStanza"
        },
        "112": {
          "next": [
            "113",
            "90"
          ],
          "stack": false,
          "answers": [
            27,
            28
          ],
          "text": 25,
          "type": "QuestionStanza"
        },
        "72": {
          "next": [
            "74"
          ],
          "stack": true,
          "text": 56,
          "type": "InstructionStanza"
        },
        "10": {
          "next": [
            "11"
          ],
          "stack": true,
          "text": 10,
          "type": "InstructionStanza"
        },
        "159": {
          "next": [
            "159a"
          ],
          "stack": true,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/you-need-to-tell-hmrc-or-contact/final159"
        },
        "59": {
          "next": [
            "end"
          ],
          "stack": true,
          "text": 44,
          "type": "InstructionStanza"
        },
        "144": {
          "next": [
            "145"
          ],
          "stack": true,
          "text": 67,
          "type": "InstructionStanza"
        },
        "87": {
          "next": [
            "88"
          ],
          "stack": true,
          "text": 48,
          "type": "InstructionStanza"
        },
        "159a": {
          "next": [
            "end"
          ],
          "stack": true,
          "text": 64,
          "type": "InstructionStanza"
        },
        "48": {
          "next": [
            "49"
          ],
          "stack": true,
          "text": 40,
          "type": "InstructionStanza"
        },
        "21": {
          "next": [
            "22"
          ],
          "stack": true,
          "text": 20,
          "type": "InstructionStanza"
        },
        "116": {
          "next": [
            "117"
          ],
          "stack": true,
          "text": 47,
          "type": "InstructionStanza"
        },
        "76": {
          "next": [
            "77",
            "121"
          ],
          "stack": false,
          "answers": [
            27,
            28
          ],
          "text": 32,
          "type": "QuestionStanza"
        },
        "138": {
          "next": [
            "139"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/how-much-was-your-income"
        },
        "54": {
          "next": [
            "55"
          ],
          "noteType": "Title",
          "stack": false,
          "text": 45,
          "type": "CalloutStanza"
        },
        "43": {
          "next": [
            "44"
          ],
          "stack": true,
          "text": 36,
          "type": "InstructionStanza"
        },
        "148": {
          "next": [
            "149"
          ],
          "stack": true,
          "text": 68,
          "type": "InstructionStanza"
        },
        "127": {
          "next": [
            "128"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/was-your-income-more-than-3750-no-income"
        },
        "65": {
          "next": [
            "66"
          ],
          "stack": true,
          "text": 53,
          "type": "InstructionStanza"
        },
        "71": {
          "next": [
            "72"
          ],
          "stack": true,
          "text": 29,
          "type": "InstructionStanza"
        },
        "57": {
          "next": [
            "58"
          ],
          "stack": true,
          "text": 48,
          "type": "InstructionStanza"
        },
        "108": {
          "next": [
            "end"
          ],
          "stack": true,
          "text": 44,
          "type": "InstructionStanza"
        },
        "80": {
          "next": [
            "81"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/was-your-income-more-than-3750"
        },
        "106": {
          "next": [
            "107"
          ],
          "stack": true,
          "text": 48,
          "type": "InstructionStanza"
        },
        "137": {
          "next": [
            "97",
            "102"
          ],
          "stack": false,
          "answers": [
            27,
            28
          ],
          "text": 38,
          "type": "QuestionStanza"
        },
        "82": {
          "next": [
            "83",
            "90"
          ],
          "stack": false,
          "answers": [
            27,
            28
          ],
          "text": 62,
          "type": "QuestionStanza"
        },
        "49": {
          "next": [
            "50"
          ],
          "stack": true,
          "text": 41,
          "type": "InstructionStanza"
        },
        "6": {
          "next": [
            "7"
          ],
          "stack": true,
          "link": 0,
          "text": 6,
          "type": "InstructionStanza"
        },
        "126": {
          "next": [
            "127",
            "159"
          ],
          "stack": false,
          "answers": [
            27,
            28
          ],
          "text": 60,
          "type": "QuestionStanza"
        },
        "36": {
          "next": [
            "30"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/less-than-1000/do-you-receive-any-income"
        },
        "1": {
          "next": [
            "2"
          ],
          "noteType": "Lede",
          "stack": false,
          "text": 1,
          "type": "CalloutStanza"
        },
        "2777": {
          "next": [
            "27"
          ],
          "noteType": "Error",
          "stack": false,
          "text": 63,
          "type": "CalloutStanza"
        },
        "39": {
          "next": [
            "41"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/less-than-1000/do-you-want-to-use-the-rent-a-room-scheme"
        },
        "140": {
          "next": [
            "141"
          ],
          "stack": true,
          "text": 25,
          "type": "InstructionStanza"
        },
        "17": {
          "next": [
            "18"
          ],
          "noteType": "SubTitle",
          "stack": false,
          "text": 16,
          "type": "CalloutStanza"
        },
        "25": {
          "next": [
            "26"
          ],
          "stack": true,
          "link": 0,
          "text": 23,
          "type": "InstructionStanza"
        },
        "60": {
          "next": [
            "61"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/less-than-1000/you-do-not-need-to-tell-hmrc"
        },
        "14": {
          "next": [
            "15"
          ],
          "stack": true,
          "text": 14,
          "type": "InstructionStanza"
        },
        "133": {
          "next": [
            "134"
          ],
          "stack": true,
          "text": 35,
          "type": "InstructionStanza"
        },
        "47": {
          "next": [
            "48"
          ],
          "noteType": "Title",
          "stack": false,
          "text": 39,
          "type": "CalloutStanza"
        },
        "122": {
          "next": [
            "123"
          ],
          "noteType": "Error",
          "stack": false,
          "text": 63,
          "type": "CalloutStanza"
        },
        "111": {
          "next": [
            "112"
          ],
          "stack": true,
          "text": 24,
          "type": "InstructionStanza"
        },
        "102": {
          "next": [
            "103"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/you-need-to-tell-hmrc"
        },
        "31": {
          "next": [
            "33"
          ],
          "stack": true,
          "text": 30,
          "type": "InstructionStanza"
        },
        "96": {
          "next": [
            "97",
            "102"
          ],
          "stack": false,
          "answers": [
            27,
            28
          ],
          "text": 38,
          "type": "QuestionStanza"
        },
        "69": {
          "next": [
            "end"
          ],
          "stack": true,
          "text": 44,
          "type": "InstructionStanza"
        },
        "151": {
          "next": [
            "152"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/you-need-to-tell-hmrc-or-contact"
        },
        "95": {
          "next": [
            "96"
          ],
          "stack": true,
          "text": 37,
          "type": "InstructionStanza"
        },
        "58": {
          "next": [
            "59"
          ],
          "stack": true,
          "text": 49,
          "type": "InstructionStanza"
        },
        "145": {
          "next": [
            "146"
          ],
          "stack": true,
          "text": 67,
          "type": "InstructionStanza"
        },
        "64": {
          "next": [
            "65"
          ],
          "stack": true,
          "text": 52,
          "type": "InstructionStanza"
        },
        "53": {
          "next": [
            "54"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/less-than-1000/you-need-to-tell-hmrc"
        },
        "42": {
          "next": [
            "43"
          ],
          "stack": true,
          "text": 35,
          "type": "InstructionStanza"
        },
        "75": {
          "next": [
            "76"
          ],
          "stack": true,
          "text": 31,
          "type": "InstructionStanza"
        },
        "115": {
          "next": [
            "116"
          ],
          "stack": true,
          "text": 46,
          "type": "InstructionStanza"
        },
        "156": {
          "next": [
            "end"
          ],
          "stack": true,
          "text": 44,
          "type": "InstructionStanza"
        },
        "109": {
          "next": [
            "110"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/was-your-income-more-than-7500"
        },
        "149": {
          "next": [
            "150"
          ],
          "noteType": "Error",
          "stack": false,
          "text": 63,
          "type": "CalloutStanza"
        },
        "20": {
          "next": [
            "21"
          ],
          "stack": true,
          "text": 19,
          "type": "InstructionStanza"
        },
        "27": {
          "next": [
            "28"
          ],
          "stack": true,
          "text": 24,
          "type": "InstructionStanza"
        },
        "70": {
          "next": [
            "71"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/do-you-receive-any-income"
        },
        "2": {
          "next": [
            "3"
          ],
          "noteType": "SubTitle",
          "stack": false,
          "text": 2,
          "type": "CalloutStanza"
        },
        "86": {
          "next": [
            "87"
          ],
          "stack": true,
          "text": 47,
          "type": "InstructionStanza"
        },
        "38": {
          "next": [
            "39",
            "53"
          ],
          "stack": false,
          "answers": [
            27,
            28
          ],
          "text": 33,
          "type": "QuestionStanza"
        },
        "81": {
          "next": [
            "82"
          ],
          "stack": true,
          "text": 61,
          "type": "InstructionStanza"
        },
        "end": {
          "type": "EndStanza"
        },
        "160": {
          "next": [
            "1"
          ],
          "noteType": "Title",
          "stack": false,
          "text": 0,
          "type": "CalloutStanza"
        },
        "118": {
          "next": [
            "119"
          ],
          "stack": true,
          "text": 49,
          "type": "InstructionStanza"
        },
        "92": {
          "next": [
            "93"
          ],
          "stack": true,
          "text": 34,
          "type": "InstructionStanza"
        },
        "157a": {
          "next": [
            "end"
          ],
          "stack": true,
          "text": 64,
          "type": "InstructionStanza"
        },
        "125": {
          "next": [
            "126"
          ],
          "noteType": "Error",
          "stack": false,
          "text": 63,
          "type": "CalloutStanza"
        },
        "18": {
          "next": [
            "19"
          ],
          "stack": true,
          "text": 17,
          "type": "InstructionStanza"
        },
        "101": {
          "next": [
            "end"
          ],
          "stack": true,
          "text": 44,
          "type": "InstructionStanza"
        },
        "154": {
          "next": [
            "155"
          ],
          "stack": true,
          "text": 72,
          "type": "InstructionStanza"
        },
        "30": {
          "next": [
            "31"
          ],
          "stack": true,
          "text": 29,
          "type": "InstructionStanza"
        },
        "7": {
          "next": [
            "8"
          ],
          "noteType": "SubTitle",
          "stack": false,
          "text": 7,
          "type": "CalloutStanza"
        },
        "143": {
          "next": [
            "144"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/do-you-want-to-use-the-tax-free-allowance"
        },
        "97": {
          "next": [
            "161"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/you-do-not-need-to-tell-hmrc-rent-a-room"
        },
        "130": {
          "next": [
            "113",
            "131"
          ],
          "stack": false,
          "answers": [
            27,
            28
          ],
          "text": 62,
          "type": "QuestionStanza"
        },
        "114": {
          "next": [
            "115"
          ],
          "noteType": "Title",
          "stack": false,
          "text": 45,
          "type": "CalloutStanza"
        },
        "start": {
          "next": [
            "160"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/"
        },
        "129": {
          "next": [
            "130"
          ],
          "noteType": "Error",
          "stack": false,
          "text": 63,
          "type": "CalloutStanza"
        },
        "29": {
          "next": [
            "36",
            "70"
          ],
          "stack": false,
          "answers": [
            27,
            28
          ],
          "text": 26,
          "type": "QuestionStanza"
        },
        "41": {
          "next": [
            "42"
          ],
          "stack": true,
          "text": 34,
          "type": "InstructionStanza"
        },
        "105": {
          "next": [
            "106"
          ],
          "stack": true,
          "text": 47,
          "type": "InstructionStanza"
        },
        "63": {
          "next": [
            "64"
          ],
          "stack": true,
          "text": 51,
          "type": "InstructionStanza"
        },
        "150": {
          "next": [
            "151",
            "157"
          ],
          "stack": false,
          "answers": [
            27,
            28
          ],
          "text": 69,
          "type": "QuestionStanza"
        },
        "3": {
          "next": [
            "4"
          ],
          "stack": true,
          "text": 3,
          "type": "InstructionStanza"
        },
        "74": {
          "next": [
            "75"
          ],
          "stack": true,
          "text": 30,
          "type": "InstructionStanza"
        },
        "91": {
          "next": [
            "92"
          ],
          "stack": true,
          "text": 34,
          "type": "InstructionStanza"
        },
        "52": {
          "next": [
            "end"
          ],
          "stack": true,
          "text": 44,
          "type": "InstructionStanza"
        },
        "85": {
          "next": [
            "86"
          ],
          "stack": true,
          "text": 46,
          "type": "InstructionStanza"
        },
        "131": {
          "next": [
            "132"
          ],
          "stack": false,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/do-you-want-to-use-the-rent-a-room-scheme-no-income"
        },
        "28": {
          "next": [
            "29"
          ],
          "stack": true,
          "text": 25,
          "type": "InstructionStanza"
        },
        "119": {
          "next": [
            "end"
          ],
          "stack": true,
          "text": 44,
          "type": "InstructionStanza"
        },
        "161": {
          "next": [
            "98"
          ],
          "noteType": "Title",
          "stack": false,
          "text": 39,
          "type": "CalloutStanza"
        },
        "136": {
          "next": [
            "137"
          ],
          "noteType": "Error",
          "stack": false,
          "text": 63,
          "type": "CalloutStanza"
        },
        "157": {
          "next": [
            "157a"
          ],
          "stack": true,
          "type": "PageStanza",
          "url": "/rent/1000-or-more/you-need-to-tell-hmrc-or-contact/final157"
        },
        "146": {
          "next": [
            "147"
          ],
          "stack": true,
          "text": 67,
          "type": "InstructionStanza"
        }
      },
      "phrases": [
        [
          "Telling HMRC about extra income",
          "Dweud wrth Gyllid a Thollau EM am incwm ychwanegol"
        ],
        [
          "Check if you need to tell HMRC about extra money you've made by selling goods or services, or renting land or property.",
          "Gwiriwch a oes angen i chi ddweud wrth Gyllid a Thollau EM am arian ychwanegol rydych chi wedi'i wneud trwy werthu nwyddau neu wasanaethau, neu rentu tir neu eiddo."
        ],
        [
          "Overview",
          "Trosolwg"
        ],
        [
          "In some circumstances, you do not have to tell HMRC about extra income you've made. In each tax year you can earn up to £11,000, tax free, if you are: selling goods or services (trading)",
          "Mewn rhai amgylchiadau, nid oes rhaid i chi ddweud wrth Gyllid a Thollau EM am incwm ychwanegol rydych wedi'i wneud. Ymhob blwyddyn dreth gallwch ennill hyd at £ 11,000, yn ddi-dreth, os ydych chi: gwerthu nwyddau neu wasanaethau (masnachu)"
        ],
        [
          "In some circumstances, you do not have to tell HMRC about extra income you've made. In each tax year you can earn up to £11,000, tax free, if you are: renting land or property",
          "Mewn rhai amgylchiadau, nid oes rhaid i chi ddweud wrth Gyllid a Thollau EM am incwm ychwanegol rydych wedi'i wneud. Ymhob blwyddyn dreth gallwch ennill hyd at £ 11,000, yn ddi-dreth, os ydych chi: rhentu tir neu eiddo"
        ],
        [
          "A tax year runs from 6 April one year to 5 April the next.",
          "Mae blwyddyn dreth yn rhedeg rhwng 6 Ebrill un flwyddyn a 5 Ebrill y flwyddyn nesaf."
        ],
        [
          "Check if you need to tell HMRC about your extra income",
          "Gwiriwch a oes angen i chi ddweud wrth Gyllid a Thollau EM am eich incwm ychwanegol"
        ],
        [
          "I've made extra income from selling goods or services",
          "Rwyf wedi gwneud incwm ychwanegol o werthu nwyddau neu wasanaethau"
        ],
        [
          "This can include selling items or offering freelance services. If you make extra money in this way, you're likely to be trading.",
          "Gall hyn gynnwys gwerthu eitemau neu gynnig gwasanaethau ar eu liwt eu hunain. Os gwnewch arian ychwanegol fel hyn, rydych yn debygol o fod yn masnachu."
        ],
        [
          "Find out more about [link:how HMRC decides if you are trading or not.:https://www.youtube.com/watch?v=MYgCctGY_Ug]",
          "Darganfyddwch fwy am [link: sut mae Cyllid a Thollau EM yn penderfynu a ydych chi'n masnachu ai peidio.:https://www.youtube.com/watch?v=MYgCctGY_Ug]"
        ],
        [
          "If you've only sold personal possessions then you're probably not trading. You will not have to pay income tax on the money you make, but you might have to pay [link:Capital Gains Tax:https://www.gov.uk/capital-gains-tax].",
          "Os mai dim ond eiddo personol rydych chi wedi'i werthu yna mae'n debyg nad ydych chi'n masnachu. Ni fydd yn rhaid i chi dalu treth incwm ar yr arian a wnewch, ond efallai y bydd yn rhaid i chi dalu [link: Treth Enillion Cyfalaf:https://www.gov.uk/capital-gains-tax]."
        ],
        [
          "The trading allowance",
          "Y lwfans masnachu"
        ],
        [
          "The trading allowance lets you earn up to £11,000 from any trading, casual or miscellaneous income, tax free, in each tax year. For example: selling items online or face to face",
          "Mae'r lwfans masnachu yn caniatáu ichi ennill hyd at £ 11,000 o unrhyw incwm masnachu, achlysurol neu amrywiol, yn ddi-dreth, ym mhob blwyddyn dreth. Er enghraifft: gwerthu eitemau ar-lein neu wyneb yn wyneb"
        ],
        [
          "The trading allowance lets you earn up to £11,000 from any trading, casual or miscellaneous income, tax free, in each tax year. For example: selling freelance services (such as gardening or babysitting)",
          "Mae'r lwfans masnachu yn caniatáu ichi ennill hyd at £ 11,000 o unrhyw incwm masnachu, achlysurol neu amrywiol, yn ddi-dreth, ym mhob blwyddyn dreth. Er enghraifft: gwerthu gwasanaethau ar eu liwt eu hunain (fel garddio neu warchod plant)"
        ],
        [
          "The trading allowance lets you earn up to £11,000 from any trading, casual or miscellaneous income, tax free, in each tax year. For example: hiring out personal equipment (such as power tools)",
          "Mae'r lwfans masnachu yn caniatáu ichi ennill hyd at £ 11,000 o unrhyw incwm masnachu, achlysurol neu amrywiol, yn ddi-dreth, ym mhob blwyddyn dreth. Er enghraifft: llogi offer personol (fel offer pŵer)"
        ],
        [
          "Check if you need to tell HMRC about income you've made by selling goods or services",
          "Gwiriwch a oes angen i chi ddweud wrth Gyllid a Thollau EM am incwm rydych wedi'i wneud trwy werthu nwyddau neu wasanaethau"
        ],
        [
          "I've made extra income from renting land or property",
          "Rwyf wedi gwneud incwm ychwanegol o rentu tir neu eiddo"
        ],
        [
          "Property income can include any money you earn by renting land or buildings.",
          "Gall incwm eiddo gynnwys unrhyw arian rydych chi'n ei ennill trwy rentu tir neu adeiladau."
        ],
        [
          "The property allowance",
          "Y lwfans eiddo"
        ],
        [
          "The property allowance lets you earn up to £11,000 in rental income, tax free, in each tax year. For example: renting a flat or house",
          "Mae'r lwfans eiddo yn caniatáu ichi ennill hyd at £ 11,000 mewn incwm rhent, di-dreth, ym mhob blwyddyn dreth. Er enghraifft: rhentu fflat neu dŷ"
        ],
        [
          "The property allowance lets you earn up to £11,000 in rental income, tax free, in each tax year. For example: renting out a room in your home",
          "Mae'r lwfans eiddo yn caniatáu ichi ennill hyd at £ 11,000 mewn incwm rhent, di-dreth, ym mhob blwyddyn dreth. Er enghraifft: rhentu ystafell yn eich cartref"
        ],
        [
          "The property allowance lets you earn up to £11,000 in rental income, tax free, in each tax year. For example: renting short term holiday lets",
          "Mae'r lwfans eiddo yn caniatáu ichi ennill hyd at £ 11,000 mewn incwm rhent, di-dreth, ym mhob blwyddyn dreth. Er enghraifft: rhentu gosodiadau gwyliau tymor byr"
        ],
        [
          "The property allowance lets you earn up to £11,000 in rental income, tax free, in each tax year. For example: renting out a parking space or garage",
          "Mae'r lwfans eiddo yn caniatáu ichi ennill hyd at £ 11,000 mewn incwm rhent, di-dreth, ym mhob blwyddyn dreth. Er enghraifft: rhentu lle parcio neu garej"
        ],
        [
          "Check if you need to tell HMRC about income you've made by renting land or property",
          "Gwiriwch a oes angen i chi ddweud wrth Gyllid a Thollau EM am incwm rydych wedi'i wneud trwy rentu tir neu eiddo"
        ],
        [
          "",
          ""
        ],
        [
          "How much was your income from property and/or land?",
          "Faint oedd eich incwm o eiddo a / neu dir?"
        ],
        [
          "Was your income from land or property less than £11,000?",
          "A oedd eich incwm o dir neu eiddo yn llai na £ 11,000?"
        ],
        [
          "Yes [hint:You agree]",
          "Ydw"
        ],
        [
          "No [hint:You disagree]",
          "Na"
        ],
        [
          "This includes: a company you (or a relative) owns or controls",
          "Welsh: This includes: a company you (or a relative) owns or controls"
        ],
        [
          "Additional trade or property income includes any money you've received for providing services (such as babysitting), selling items you've bought or made, or renting property.",
          "Welsh: Additional trade or property income includes any money you've received for providing services (such as babysitting), selling items you've bought or made, or renting property."
        ],
        [
          "A relative includes your spouse or civil partner. It also includes your family members (excluding aunts, uncles or cousins), or their spouse or civil partner.",
          "Welsh: A relative includes your spouse or civil partner. It also includes your family members (excluding aunts, uncles or cousins), or their spouse or civil partner."
        ],
        [
          "Have you received additional trade or property income from a company or person you are connected to?",
          "Welsh: Have you received additional trade or property income from a company or person you are connected to?"
        ],
        [
          "Have you only rented a room in your main home?",
          "Welsh: Have you only rented a room in your main home?"
        ],
        [
          "You can choose whether to: use the Rent a Room Scheme. This means you are automatically entitled to £17,500 of tax-free property income (or £13,750 if you share your property income with other people)",
          "Welsh: You can choose whether to: use the Rent a Room Scheme. This means you are automatically entitled to £17,500 of tax-free property income (or £13,750 if you share your property income with other people)"
        ],
        [
          "You can choose whether to: claim allowable expenses (such as the cost of maintenance and a percentage of your mortgage interest) from your rental income. This reduces the amount of tax they pay on that income. It can also be used to declare a loss.",
          "Welsh: You can choose whether to: claim allowable expenses (such as the cost of maintenance and a percentage of your mortgage interest) from your rental income. This reduces the amount of tax they pay on that income. It can also be used to declare a loss."
        ],
        [
          "Your choice is likely to depend on how much youve earned and how much your expenses and allowances are.",
          "Welsh: Your choice is likely to depend on how much youve earned and how much your expenses and allowances are."
        ],
        [
          "Find out more about the [link:Rent a Room Scheme:https://www.gov.uk/government/publications/rent-a-room-for-traders-hs223-self-assessment-helpsheet/hs223-rent-a-room-scheme-2019],claiming [link:allowable expenses:https://www.gov.uk/expenses-if-youre-self-employed] and [link:deducting other financial costs from your rental income:https://www.gov.uk/expenses-if-youre-self-employed].",
          "Welsh: Find out more about the [link:Rent a Room Scheme:https://www.gov.uk/government/publications/rent-a-room-for-traders-hs223-self-assessment-helpsheet/hs223-rent-a-room-scheme-2019],claiming [link:allowable expenses:https://www.gov.uk/expenses-if-youre-self-employed] and [link:deducting other financial costs from your rental income:https://www.gov.uk/expenses-if-youre-self-employed]."
        ],
        [
          "Do you want to use the Rent a Room Scheme or claim allowable expenses?",
          "Welsh: Do you want to use the Rent a Room Scheme or claim allowable expenses?"
        ],
        [
          "You do not need to tell HMRC about your property income",
          "Welsh: You do not need to tell HMRC about your property income"
        ],
        [
          "Because you have received trade or property income from a company, partnership or your employer, you need to complete a Self Assessment tax return.",
          "Welsh: Because you have received trade or property income from a company, partnership or your employer, you need to complete a Self Assessment tax return."
        ],
        [
          "However, if your rental income is less than £17,500 (or £13,750 if you share your income), you are automatically entitled to use the Rent a Room Scheme. This means you do not need to pay tax on this income, and you do not need to tell HMRC.",
          "Welsh: However, if your rental income is less than £17,500 (or £13,750 if you share your income), you are automatically entitled to use the Rent a Room Scheme. This means you do not need to pay tax on this income, and you do not need to tell HMRC."
        ],
        [
          "If you want to opt out of the Rent a Room Scheme in future (for example, if you want to claim allowable expenses), you must let HMRC know within one year of 31 January following the end of the tax year.",
          "Welsh: If you want to opt out of the Rent a Room Scheme in future (for example, if you want to claim allowable expenses), you must let HMRC know within one year of 31 January following the end of the tax year."
        ],
        [
          "Find out more about the [link:Rent a Room Scheme:https://www.gov.uk/government/publications/rent-a-room-for-traders-hs223-self-assessment-helpsheet/hs223-rent-a-room-scheme-2019] and [link:Self Assessment tax returns:https://www.gov.uk/self-assessment-tax-returns].",
          "Welsh: Find out more about the [link:Rent a Room Scheme:https://www.gov.uk/government/publications/rent-a-room-for-traders-hs223-self-assessment-helpsheet/hs223-rent-a-room-scheme-2019] and [link:Self Assessment tax returns:https://www.gov.uk/self-assessment-tax-returns]."
        ],
        [
          "[link:Check if you need to tell HMRC about income you've made by selling goods or services:26].",
          "Welsh: [link:Check if you need to tell HMRC about income you've made by selling goods or services:26]."
        ],
        [
          "You need to tell HMRC about all your income on a Self Assessment tax return",
          "Welsh: You need to tell HMRC about all your income on a Self Assessment tax return"
        ],
        [
          "Self Assessment tax returns are a way of reporting your income to HMRC. You need to include the income you have received from trading on your Self Assessment tax return.",
          "Welsh: Self Assessment tax returns are a way of reporting your income to HMRC. You need to include the income you have received from trading on your Self Assessment tax return."
        ],
        [
          "You can deduct allowable expenses (such as the cost of maintenance and a percentage of your mortgage interest) from your rental income. This reduces the amount of tax you pay on that income. If you made a loss on renting your land or property, you may be able to offset future tax against this loss.",
          "Welsh: You can deduct allowable expenses (such as the cost of maintenance and a percentage of your mortgage interest) from your rental income. This reduces the amount of tax you pay on that income. If you made a loss on renting your land or property, you may be able to offset future tax against this loss."
        ],
        [
          "If you have not previously completed a Self Assessment, you will need to [link:register for Self Assessment:https://www.gov.uk/log-in-file-self-assessment-tax-return/register-if-youre-self-employed] by 5 October in the following year.",
          "Welsh: If you have not previously completed a Self Assessment, you will need to [link:register for Self Assessment:https://www.gov.uk/log-in-file-self-assessment-tax-return/register-if-youre-self-employed] by 5 October in the following year."
        ],
        [
          "Find out more about [link:claiming allowable expenses:https://www.gov.uk/expenses-if-youre-self-employed] and [link:deducting other financial costs from your rental income:https://www.gov.uk/guidance/changes-to-tax-relief-for-residential-landlords-how-its-worked-out-including-case-studies].",
          "Welsh: Find out more about [link:claiming allowable expenses:https://www.gov.uk/expenses-if-youre-self-employed] and [link:deducting other financial costs from your rental income:https://www.gov.uk/guidance/changes-to-tax-relief-for-residential-landlords-how-its-worked-out-including-case-studies]."
        ],
        [
          "You are automatically entitled to the £11000 tax-free property allowance. You do not need to report this income to HMRC.",
          "Welsh: You are automatically entitled to the £11000 tax-free property allowance. You do not need to report this income to HMRC."
        ],
        [
          "There are some circumstances where you may still want to tell HMRC about your income using a Self Assessment form. For example: you've made a loss and want to claim relief on a tax return",
          "Welsh: There are some circumstances where you may still want to tell HMRC about your income using a Self Assessment form. For example: you've made a loss and want to claim relief on a tax return"
        ],
        [
          "There are some circumstances where you may still want to tell HMRC about your income using a Self Assessment form. For example: you want to pay voluntary Class 2 National Insurance contributions to help qualify for some benefits",
          "Welsh: There are some circumstances where you may still want to tell HMRC about your income using a Self Assessment form. For example: you want to pay voluntary Class 2 National Insurance contributions to help qualify for some benefits"
        ],
        [
          "There are some circumstances where you may still want to tell HMRC about your income using a Self Assessment form. For example: you want to claim Tax Free Childcare for childcare costs based on your self employment income",
          "Welsh: There are some circumstances where you may still want to tell HMRC about your income using a Self Assessment form. For example: you want to claim Tax Free Childcare for childcare costs based on your self employment income"
        ],
        [
          "There are some circumstances where you may still want to tell HMRC about your income using a Self Assessment form. For example: you want to claim Maternity Allowance, based on your self-employment",
          "Welsh: There are some circumstances where you may still want to tell HMRC about your income using a Self Assessment form. For example: you want to claim Maternity Allowance, based on your self-employment"
        ],
        [
          "Find out more about [link:Tax-free allowances:https://www.gov.uk/guidance/tax-free-allowances-on-property-and-trading-income] and [link:Self Assessment tax returns:https://www.gov.uk/self-assessment-tax-returns].",
          "Welsh: Find out more about [link:Tax-free allowances:https://www.gov.uk/guidance/tax-free-allowances-on-property-and-trading-income] and [link:Self Assessment tax returns:https://www.gov.uk/self-assessment-tax-returns]."
        ],
        [
          "This includes: a partnership where you (or a relative) is a partner",
          "Welsh: This includes: a partnership where you (or a relative) is a partner"
        ],
        [
          "Was your income from renting a room/rooms more than £17,500?",
          "Welsh: Was your income from renting a room/rooms more than £17,500?"
        ],
        [
          "If your rental income is less than £17,500 (or £13,750 if you share your income), you are automatically entitled to use the Rent a Room Scheme. This means you do not need to pay tax on this income, and you do not need to tell HMRC.",
          "Welsh: If your rental income is less than £17,500 (or £13,750 if you share your income), you are automatically entitled to use the Rent a Room Scheme. This means you do not need to pay tax on this income, and you do not need to tell HMRC."
        ],
        [
          "Find out more about the [link:Rent a Room Scheme:https://www.gov.uk/government/publications/rent-a-room-for-traders-hs223-self-assessment-helpsheet/hs223-rent-a-room-scheme-2019].",
          "Welsh: Find out more about the [link:Rent a Room Scheme:https://www.gov.uk/government/publications/rent-a-room-for-traders-hs223-self-assessment-helpsheet/hs223-rent-a-room-scheme-2019]."
        ],
        [
          "Did you share the income with anyone else?",
          "Welsh: Did you share the income with anyone else?"
        ],
        [
          "To work this out, add up all the income you've received from your land or property. Include money received from tenants for rent, utility bills and food. Do not deduct any expenses.",
          "Welsh: To work this out, add up all the income you've received from your land or property. Include money received from tenants for rent, utility bills and food. Do not deduct any expenses."
        ],
        [
          "Was your income from renting a room/rooms more than £13,750?",
          "Welsh: Was your income from renting a room/rooms more than £13,750?"
        ],
        [
          "Please select",
          "Welsh: Please select"
        ],
        [
          "This has not been built yet",
          "Welsh: This has not been built yet"
        ],
        [
          "Between £11,000 and £12,500 [hint:A hint for the 'Between £11,000 and £12,500' answer]",
          "Welsh: Between £11,000 and £12,500 [hint:A hint for the 'Between £11,000 and £12,500' answer]"
        ],
        [
          "More than £12,500 [hint:A hint for the 'More than £12,500' answer]",
          "Welsh: More than £12,500 [hint:A hint for the 'More than £12,500' answer]"
        ],
        [
          "If you claim the £11000 tax-free property allowance, you cannot deduct allowable expenses or other allowances.",
          "Welsh: If you claim the £11000 tax-free property allowance, you cannot deduct allowable expenses or other allowances."
        ],
        [
          "Find out more about [link:Tax-free allowances:https://www.gov.uk/guidance/tax-free-allowances-on-property-and-trading-income], claiming [link:allowable expenses:https://www.gov.uk/expenses-if-youre-self-employed] and [link:deducting other financial costs from your rental income:https://www.gov.uk/guidance/changes-to-tax-relief-for-residential-landlords-how-its-worked-out-including-case-studies].",
          "Welsh: Find out more about [link:Tax-free allowances:https://www.gov.uk/guidance/tax-free-allowances-on-property-and-trading-income], claiming [link:allowable expenses:https://www.gov.uk/expenses-if-youre-self-employed] and [link:deducting other financial costs from your rental income:https://www.gov.uk/guidance/changes-to-tax-relief-for-residential-landlords-how-its-worked-out-including-case-studies]."
        ],
        [
          "Do you want to use the tax-free trading allowance?",
          "Welsh: Do you want to use the tax-free trading allowance?"
        ],
        [
          "You need tell HMRC about this income",
          "Welsh: You need tell HMRC about this income"
        ],
        [
          "You can tell HMRC about this income on a Self Assessment tax return.",
          "Welsh: You can tell HMRC about this income on a Self Assessment tax return."
        ],
        [
          "If you do not normally complete a Self Assessment, you can [link:contact HMRC:https://www.gov.uk/government/organisations/hm-revenue-customs/contact/income-tax-enquiries-for-individuals-pensioners-and-employees] to discuss alternative ways of reporting this income.",
          "Welsh: If you do not normally complete a Self Assessment, you can [link:contact HMRC:https://www.gov.uk/government/organisations/hm-revenue-customs/contact/income-tax-enquiries-for-individuals-pensioners-and-employees] to discuss alternative ways of reporting this income."
        ],
        [
          "Find out more about [link:Self Assessment tax returns:https://www.gov.uk/self-assessment-tax-returns].",
          "Welsh: Find out more about [link:Self Assessment tax returns:https://www.gov.uk/self-assessment-tax-returns]."
        ]
      ],
      "links": [
        {
          "id": 0,
          "dest": "26",
          "title": "",
          "window": false
        }
      ]
    }
  """
}
