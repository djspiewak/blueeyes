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

package blueeyes.json

import org.specs.Specification

object Examples extends Specification {
  import JsonAST._
  import JsonDSL._
  import JsonParser._

  val lotto = """{
    "lotto":{
      "lotto-id":5,
      "winning-numbers":[2,45,34,23,7,5,3],
      "winners":[ {
        "winner-id":23,
        "numbers":[2,45,34,23,3, 5]
      },{
        "winner-id" : 54 ,
        "numbers":[ 52,3, 12,11,18,22 ]
      }]
    }
  }"""

  val person = """{
    "person": {
      "name": "Joe",
      "age": 35,
      "spouse": {
        "person": {
          "name": "Marilyn",
          "age": 33
        }
      }
    }
  }"""

  val personDSL =
    ("person" ->
      ("name" -> "Joe") ~
      ("age" -> 35) ~
      ("spouse" ->
        ("person" ->
          ("name" -> "Marilyn") ~
          ("age" -> 33)
        )
      )
    )

  val objArray = """{ 
    "name": "joe",
    "address": {
      "street": "Bulevard",
      "city": "Helsinki"
    },
    "children": [
      {
        "name": "Mary",
        "age": 5
      },
      {
        "name": "Mazy",
        "age": 3
      }
    ]
  }"""

  val nulls = ("f1" -> null) ~ ("f2" -> List(null, "s"))
  val quoted = """["foo \" \n \t \r bar"]"""
  val symbols = ("f1" -> 'foo) ~ ("f2" -> 'bar)
  "Lotto example" in {
    val json = parse(lotto)
    val renderedLotto = compact(render(json))
    json mustEqual parse(renderedLotto)
  }

  "Person example" in {
    val json = parse(person)
    val renderedPerson = JsonDSL.pretty(render(json))
    json mustEqual parse(renderedPerson)
    render(json) mustEqual render(personDSL)
    compact(render(json \\ "name")) mustEqual """["Joe","Marilyn"]"""
    (json \ "person" \ "name") mustEqual JString("Joe")
    compact(render(json \ "person" \ "name")) mustEqual "\"Joe\""
  }

  "Transformation example" in {
    val uppercased = parse(person).transform { 
      case JObject(fields) => JObject(fields.map(field => JField(field.name.toUpperCase, field.value))) 
    }

    val rendered = compact(render(uppercased))
    rendered mustEqual
      """{"PERSON":{"NAME":"Joe","AGE":35,"SPOUSE":{"PERSON":{"NAME":"Marilyn","AGE":33}}}}"""
  }

  "Remove example" in {
    val json = parse(person) removeField { _ == JField("name", "Marilyn") }
    compact(render(json \\ "name")) mustEqual "[\"Joe\"]"
  }

  "Queries on person example" in {
    val json = parse(person)
    val filtered = json filter {
      case JObject(fields) => fields.exists(_.name == "name")
      case _ => false
    }
    filtered.map(_ \ "name") mustEqual List(JString("Joe"), JString("Marilyn"))

    val found = json find {
      case JObject(fields) => fields.exists(_.name == "name")
      case _ => false
    }
    found.map(_ \ "name") mustEqual Some(JString("Joe"))
  }

  "Object array example" in {
    val json = parse(objArray)
    compact(render(json \ "children" \\ "name")) mustEqual """["Mary","Mazy"]"""
    compact(render((json \ "children")(0) \ "name")) mustEqual "\"Mary\""
    compact(render((json \ "children")(1) \ "name")) mustEqual "\"Mazy\""
    (for (JObject(fields) <- json; JField("name", JString(y)) <- fields) yield y) mustEqual List("joe", "Mary", "Mazy")
  }

  "Unbox values using XPath-like type expression" in {
    (parse(objArray) \ "children" \\ JInt).map(_.unbox) mustEqual List(5, 3)
    (parse(lotto) \ "lotto" \ "winning-numbers" \ JInt).map(_.unbox) mustEqual List(2, 45, 34, 23, 7, 5, 3)
    (parse(lotto) \\ "winning-numbers" \\ JInt).map(_.unbox) mustEqual List(2, 45, 34, 23, 7, 5, 3)
  }

  "Quoted example" in {
    val json = parse(quoted)
    (json asUnsafe JArray).elements mustEqual List(JString("foo \" \n \t \r bar"))
  }

  "Null example" in {
    compact(render(parse(""" {"name": null} """))) mustEqual """{"name":null}"""
  }

  "Null rendering example" in {
    compact(render(nulls)) mustEqual """{"f1":null,"f2":[null,"s"]}"""
  }

  "Symbol example" in {
    compact(render(symbols)) mustEqual """{"f1":"foo","f2":"bar"}"""
  }

  "Unicode example" in {
    parse("[\" \\u00e4\\u00e4li\\u00f6t\"]") mustEqual JArray(List(JString(" \u00e4\u00e4li\u00f6t")))
  }

  "Exponent example" in {
    parse("""{"num": 2e5 }""") mustEqual JObject(List(JField("num", JDouble(200000.0))))
    parse("""{"num": -2E5 }""") mustEqual JObject(List(JField("num", JDouble(-200000.0))))
    parse("""{"num": 2.5e5 }""") mustEqual JObject(List(JField("num", JDouble(250000.0))))
    parse("""{"num": 2.5e-5 }""") mustEqual JObject(List(JField("num", JDouble(2.5e-5))))
  }

  "JSON building example" in {
    val json = JObject(JField("name", JString("joe")) :: JField("age", JInt(34)) :: Nil) ++ 
               JObject(JField("name", JString("mazy")) :: JField("age", JInt(31)) :: Nil)
    compact(render(json)) mustEqual """[{"name":"joe","age":34},{"name":"mazy","age":31}]"""
  }

  "JSON building with implicit primitive conversions example" in {
    import Implicits._
    val json = JObject(JField("name", "joe") :: JField("age", 34) :: Nil) ++ 
               JObject(JField("name", "mazy") :: JField("age", 31) :: Nil)

    compact(render(json)) mustEqual """[{"name":"joe","age":34},{"name":"mazy","age":31}]"""
  }

  "Example which folds up to form a flattened list" in {
    val json = parse(person)

    def form(list: JPath*): List[(JPath, JValue)] = list.toList.map { path =>
      (path, json(path))
    }

    val folded = (json.foldUpWithPath[List[(JPath, JValue)]](Nil) { (list, path, json) =>
      (path, json) :: list
    }).reverse.collect { case (p, j) if (!j.isInstanceOf[JField]) => (p, j) }

    val formed = form(
      JPath("person.name"),
      JPath("person.age"),
      JPath("person.spouse.person.name"),
      JPath("person.spouse.person.age"),
      JPath("person.spouse.person"),
      JPath("person.spouse"),
      JPath("person"),
      JPath.Identity
    )

    folded mustEqual formed
  }

  "Renders JSON as Scala code" in {
    val json = parse(lotto)
    println("json:" + json)
    val scalaDoc = renderScala(json)
    val rendered = Printer.compact(scalaDoc)
    println("rendered: " + Printer.pretty(scalaDoc))
    rendered mustEqual """JObject(JField("lotto",JObject(JField("lotto-id",JInt(5))::JField("winning-numbers",JArray(JInt(2)::JInt(45)::JInt(34)::JInt(23)::JInt(7)::JInt(5)::JInt(3)::Nil))::JField("winners",JArray(JObject(JField("winner-id",JInt(23))::JField("numbers",JArray(JInt(2)::JInt(45)::JInt(34)::JInt(23)::JInt(3)::JInt(5)::Nil))::Nil)::JObject(JField("winner-id",JInt(54))::JField("numbers",JArray(JInt(52)::JInt(3)::JInt(12)::JInt(11)::JInt(18)::JInt(22)::Nil))::Nil)::Nil))::Nil))::Nil)"""
  }

}
