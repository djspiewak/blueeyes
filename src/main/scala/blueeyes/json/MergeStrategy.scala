package blueeyes.json

import JsonAST._

trait MergeStrategy {
  def apply(left: JValue, right: JValue): JValue
}

object MergeStrategy {
  implicit val DefaultMergeStrategy = new MergeStrategy {
    def apply(left: JValue, right: JValue) = (left, right) match {
      case (left: JObject, right: JObject) => left.merge(right)(this)
      case (left: JArray, right: JArray) => left.merge(right)(this)
      case (JNothing, right) => right
      case (left, JNothing) => left
      case (_, right) => right        // right wins in the end
    }
  }
}

