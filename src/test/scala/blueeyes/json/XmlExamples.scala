/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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

package blueeyes {
package json {

import _root_.org.specs.Specification

object XmlExamples extends Specification {
  import JsonAST._
  import JsonDSL._
  import JsonParser.parse
  import Xml._
  import scala.xml.{Group, Text}
  
  noDetailedDiffs()

  "Basic conversion example" in {
    val json = toJson(users1)
    compact(render(json)) mustEqual """{"users":{"count":"2","user":[{"disabled":"true","id":"1","name":"Harry"},{"id":"2","name":"David","nickname":"Dave"}]}}"""
  }

  "Conversion transformation example 1" in {
    val json = toJson(users1).transform {
      case JObject(fields) => JObject(
        fields.map {
          case JField("id", JString(s)) => JField("id", JInt(s.toInt))
          case field => field
        }
      )
    }
    
    compact(render(json)) mustEqual """{"users":{"count":"2","user":[{"disabled":"true","id":1,"name":"Harry"},{"id":2,"name":"David","nickname":"Dave"}]}}"""
  }

  "Conversion transformation example 2" in {
    val json = toJson(users2).transform {
      case JObject(fields) => JObject(
        fields map {
          case JField("id", JString(s)) => JField("id", JInt(s.toInt))
          case JField("user", x: JObject) => JField("user", JArray(x :: Nil))
          case field => field
        }
      )
    }
    compact(render(json)) mustEqual """{"users":{"user":[{"id":1,"name":"Harry"}]}}"""
  }

  "Primitive array example" in {
    val xml = <chars><char>a</char><char>b</char><char>c</char></chars>
    compact(render(toJson(xml))) mustEqual """{"chars":{"char":["a","b","c"]}}"""
  }

  "Lotto example which flattens number arrays into encoded string arrays" in {
    def flattenArray(nums: List[JValue]) = JString(nums.collect{case JInt(v) => v}.mkString(","))

    val printer = new scala.xml.PrettyPrinter(100,2)
    val lotto: JObject = LottoExampleJson
    
    val obj2 = (lotto \ "lotto") match {
      case JObject(fields) => JObject(fields map {
        case JField("winning-numbers", JArray(nums)) => JField("winning-numbers", flattenArray(nums))
        
        case JField("winners", JArray(objs)) => JField("winners", JArray(objs map {
          case JObject(fields) => JObject(fields map {
            case JField("numbers", JArray(nums)) => JField("numbers", flattenArray(nums))
            case jfield => jfield
          })
          
          case v => v
        }))
        
        case jfield => jfield
      })
      
      case x => sys.error("This case should never arise (since we control the data): " + x)
    }
    
    val lotto2 = JObject(List(JField("lotto", obj2)))
    val xml = toXml(lotto2)

    printer.format(xml(0)) mustEqual printer.format(
      <lotto>
        <id>5</id>
        <winning-numbers>2,45,34,23,7,5,3</winning-numbers>
        <winners>
          <winner-id>23</winner-id>
          <numbers>2,45,34,23,3,5</numbers>
        </winners>
        <winners>
          <winner-id>54</winner-id>
          <numbers>52,3,12,11,18,22</numbers>
        </winners>
      </lotto>)
  }

  "Band example with namespaces" in {
    val json = toJson(band)
    json mustEqual parse("""{
  "b:band":{
    "name":"The Fall",
    "genre":"rock",
    "influence":"",
    "playlists":{
      "playlist":[{
        "name":"hits",
        "song":["Hit the north","Victoria"]
      },{
        "name":"mid 80s",
        "song":["Eat your self fitter","My new house"]
      }]
    }
  }
}""")
  }

  val band =
    <b:band>
      <name>The Fall</name>
      <genre>rock</genre>
      <influence/>
      <playlists>
        <playlist name="hits">
          <song>Hit the north</song>
          <song>Victoria</song>
        </playlist>
        <playlist name="mid 80s">
          <song>Eat your self fitter</song>
          <song>My new house</song>
        </playlist>
      </playlists>
    </b:band>

  "Grouped text example" in {
    val json = toJson(groupedText)
    compact(render(json)) mustEqual """{"g":{"group":"foobar","url":"http://example.com/test"}}"""
  }

  val users1 =
    <users count="2">
      <user disabled="true">
        <id>1</id>
        <name>Harry</name>
      </user>
      <user>
        <id>2</id>
        <name nickname="Dave">David</name>
      </user>
    </users>

  val users2 =
    <users>
      <user>
        <id>1</id>
        <name>Harry</name>
      </user>
    </users>

  val url = "test"
  val groupedText =
    <g>
      <group>{ Group(List(Text("foo"), Text("bar"))) }</group>
      <url>http://example.com/{ url }</url>
    </g>

  // Examples by Jonathan Ferguson. See http://groups.google.com/group/liftweb/browse_thread/thread/f3bdfcaf1c21c615/c311a91e44f9c178?show_docid=c311a91e44f9c178
  // This example shows how to use a transformation function to correct JSON generated by
  // default conversion rules. The transformation function 'attrToObject' makes following conversion:
  // { ..., "fieldName": "", "attrName":"someValue", ...}      ->
  // { ..., "fieldName": { "attrName": f("someValue") }, ... }
  def attrToObject(fieldName: String, attrName: String, f: JString => JValue)(json: JValue) = json.transform {
    case JObject(fields) => JObject(
      fields.flatMap {
        case JField(n, v: JString) if n == attrName => Some(JField(n, JObject(JField(attrName, f(v)) :: Nil)))
        case JField(n, JString("")) if n == fieldName => None
        case field => Some(field)
      }
    )
  } transform {
    case JObject(fields) => JObject(
      fields.map {
        case JField(n, x: JObject) if n == attrName => JField(fieldName, x)
        case field => field
      }
    )
  }

  "Example with multiple attributes, multiple nested elements " in  {
    val a1 = attrToObject("stats", "count", s => JInt(s.value.toInt)) _
    val a2 = attrToObject("messages", "href", identity) _
    val json = a1(a2(toJson(messageXml1)))
    compact(render(json)) mustEqual expected1
  }

  "Example with one attribute, one nested element " in {
    val a = attrToObject("stats", "count", s => JInt(s.value.toInt)) _
    compact(render(a(toJson(messageXml2)))) mustEqual expected2
    compact(render(a(toJson(messageXml3)))) mustEqual expected2
  }

  val messageXml1 =
    <message expiry_date="20091126" text="text" word="ant" self="me">
      <stats count="0"></stats>
      <messages href="https://domain.com/message/ant"></messages>
    </message>

//  val expected1 = """{"message":{"expiry_date":"20091126","word":"ant","text":"text","self":"me","stats":{"count":0},"messages":{"href":"https://domain.com/message/ant"}}}"""
  val expected1 = """{"message":{"expiry_date":"20091126","text":"text","self":"me","word":"ant","stats":{"count":0},"messages":{"href":"https://domain.com/message/ant"}}}"""

  val messageXml2 =
    <message expiry_date="20091126">
      <stats count="0"></stats>
    </message>

  val messageXml3 = <message expiry_date="20091126"><stats count="0"></stats></message>

  val expected2 = """{"message":{"expiry_date":"20091126","stats":{"count":0}}}"""

  case class Winner(`winner-id`: Long, numbers: List[Int])
  case class Lotto(id: Long, `winning-numbers`: List[Int], winners: List[Winner],
                   `draw-date`: Option[java.util.Date])

  val winners = List(Winner(23, List(2, 45, 34, 23, 3, 5)), Winner(54, List(52, 3, 12, 11, 18, 22)))
  val lotto = Lotto(5, List(2, 45, 34, 23, 7, 5, 3), winners, None)

  val LottoExampleJson =
    ("lotto" ->
      ("id" -> lotto.id) ~
      ("winning-numbers" -> lotto.`winning-numbers`) ~
      ("draw-date" -> lotto.`draw-date`.map(_.toString)) ~
      ("winners" ->
        lotto.winners.map { w =>
          (("winner-id" -> w.`winner-id`) ~
           ("numbers" -> w.numbers))}))

}

}
}
