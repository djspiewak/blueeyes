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
import org.scalacheck.Prop.forAll
import org.specs.Specification
import org.specs.ScalaCheck

object JsonASTSpec extends Specification with ScalaCheck with ArbitraryJPath with ArbitraryJValue {
  import JsonAST._

  override val defaultPrettyParams = Pretty.Params(2)

  "Functor identity" in {
    val identityProp = (json: JValue) => json == (json mapUp identity)
    forAll(identityProp) must pass
  }

  "Functor composition" in {
    val compositionProp = (json: JValue, fa: JValue => JValue, fb: JValue => JValue) =>
      json.mapUp(fb).mapUp(fa) == json.mapUp(fa compose fb)

    forAll(compositionProp) must pass
  }

  "Monoid identity" in {
    val identityProp = (json: JValue) => (json ++ JNothing == json) && (JNothing ++ json == json)
    forAll(identityProp) must pass
  }

  "Monoid associativity" in {
    val assocProp = (x: JValue, y: JValue, z: JValue) => x ++ (y ++ z) == (x ++ y) ++ z
    forAll(assocProp) must pass
  }

  "Merge identity" in {
    val identityProp = (json: JValue) => (json merge JNothing) == json && (JNothing merge json) == json
    forAll(identityProp) must pass
  }

  "Merge idempotency" in {
    val idempotencyProp = (x: JValue) => (x merge x) == x
    forAll(idempotencyProp) must pass
  }

  "Diff identity" in {
    val identityProp = (json: JValue) =>
      (json diff JNothing) == Diff(JNothing, JNothing, json) &&
      (JNothing diff json) == Diff(JNothing, json, JNothing)

    forAll(identityProp) must pass
  }

  "Diff with self is empty" in {
    val emptyProp = (x: JValue) => (x diff x) == Diff(JNothing, JNothing, JNothing)
    forAll(emptyProp) must pass
  }

  "Diff is subset of originals" in {
    val subsetProp = (x: JObject, y: JObject) => {
      val Diff(c, a, d) = x diff y
      y == (y merge (c merge a))
    }
    forAll(subsetProp) must pass
  }

  "Diff result is same when fields are reordered" in {
    val reorderProp = (x: JObject) => (x diff reorderFields(x)) == Diff(JNothing, JNothing, JNothing)
    forAll(reorderProp) must pass
  }

  "Remove all" in {
    val removeAllProp = (x: JValue) => (x remove { _ => true }) == JNothing
    forAll(removeAllProp) must pass
  }

  "Remove nothing" in {
    val removeNothingProp = (x: JValue) => (x remove { _ => false }) == x
    forAll(removeNothingProp) must pass
  }

  "Remove removes only matching elements (in case of a field, the field is removed)" in {
    val removeProp = (json: JValue, x: JManifest) => {
      val removed = json remove typePredicate(x)
      val Diff(c, a, d) = json diff removed

      removed.flatten.forall(v => !x(v).isDefined)
    }
    forAll(removeProp) must pass
  }

  "Set and retrieve an arbitrary jvalue at an arbitrary path" in {
    runArbitraryPathSpec
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
