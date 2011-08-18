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

package blueeyes
package json

import org.scalacheck._
import org.scalacheck.Prop._
import org.specs.Specification
import org.specs.ScalaCheck

object JsonASTSpec extends Specification with ScalaCheck with ArbitraryJPath with ArbitraryJValue {
  import JsonAST._

  override val defaultPrettyParams = Pretty.Params(2)

  val numWorkers = Runtime.getRuntime.availableProcessors
  
  noDetailedDiffs()
  
  "JValue" should {
    "Functor identity" in {
      val identityProp = (json: JValue) => json == (json mapUp identity)
      forAll(identityProp) must pass
    }
  
    "Functor composition" in {
      val compositionProp = (json: JValue, fa: JValue => JValue, fb: JValue => JValue) =>
        json.mapUp(fb).mapUp(fa) == json.mapUp(fa compose fb)
  
      forAll(compositionProp) must pass
    }

    "Remove all" in {
      val removeAllProp = (x: JValue) => (x remove { _ => true }) == JNothing
      forAll(removeAllProp) must pass
    }
  
    "Remove nothing" in {
      val removeNothingProp = (x: JValue) => (x remove { _ => false }) == x
      forAll(removeNothingProp) must pass
    }
  
    "Set and retrieve an arbitrary jvalue at an arbitrary path" in {
      runArbitraryPathSpec
    }
  }
  
  "JObject merge" should {
    "define {} as identity" in {
      val prop = forAll { obj: JObject =>
        (obj merge JObject(Nil)) mustEqual obj
        (JObject(Nil) merge obj) mustEqual obj
      }
      
      prop must pass
    }
    
    "be idempotent" in {
      val prop = forAll { obj: JObject =>
        (obj merge obj) mustEqual obj
      }
      
      prop must pass
    }
  }
  
  "JArray merge" should {
    "define [] as identity" in {
      val prop = forAll { arr: JArray =>
        (arr merge JArray(Nil)) mustEqual arr
        (JArray(Nil) merge arr) mustEqual arr
      }
      
      prop must pass
    }
    
    "be idempotent" in {
      val prop = forAll { arr: JArray =>
        (arr merge arr) mustEqual arr
      }
      
      prop must pass
    }
  }
  
  "JArray ++" should {
    "define [] as identity" in {
      val prop = forAll { arr: JArray =>
        (arr ++ JArray(Nil)) mustEqual arr
        (JArray(Nil) ++ arr) mustEqual arr
      }
      
      prop must pass
    }
    
    "preserve associativity" in {
      val prop = forAll { (x: JArray, y: JArray, z: JArray) =>
        (x ++ (y ++ z)) mustEqual ((x ++ y) ++ z)
      }
      
      prop must pass(set(minTestsOk -> 1600, maxSize -> 200, workers -> numWorkers))
    }
  }

  def runArbitraryPathSpec = {
    import org.scalacheck.Prop._

    implicit val arbJPath: Arbitrary[JPath] = Arbitrary {
      import Gen._

      val genIndex = for {
        index <- choose(0, 10)
      } yield JPathIndex(index)

      val genField = for {
        name <- identifier
      } yield JPathField(name)

      val genIndexOrField = Gen.frequency((1, genIndex), (9, genField))

      for {
        length      <- choose(0, 10)
        listOfNodes <- listOfN(length, genIndexOrField)
      } yield JPath(listOfNodes)
    }

    def badPath(jv: JValue, p: JPath): Boolean = {
      p.nodes match {
        case JPathIndex(index) :: xs => jv match {
          case JArray(nodes) => index > nodes.length || 
                                (index < nodes.length && badPath(nodes(index), JPath(xs))) ||
                                badPath(JArray(Nil), JPath(xs)) 

          case JObject(_) => true
          case _ => index != 0 || badPath(JArray(Nil), JPath(xs))
        }

        case JPathField(name) :: xs => jv match {
          case JArray(_) => true
          case _ => badPath(jv \ name, JPath(xs))
        }

        case Nil => false
      }
    } 

    val setProp = (jv: JValue, p: JPath, toSet: JValue) => {
      (!badPath(jv, p)) ==> ((p == JPath.Identity && jv.set(p, toSet) == toSet) || (jv.set(p, toSet).get(p) == toSet))
    }

    forAll(setProp) must pass
  }

  private def reorderFields(json: JValue) = json mapUp {
    case JObject(xs) => JObject(xs.reverse)
    case x => x
  }

  private def typePredicate(manifest: JManifest)(json: JValue) = manifest(json).isDefined
}
