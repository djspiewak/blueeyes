package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._
import Evaluators._
import blueeyes.json.JPathImplicits

class NeqFieldFilterEvaluatorSpec  extends Specification with JPathImplicits{
  "returns false for the same JValues" in {
    NeFieldFilterEvaluator(JString("foo"), JString("foo")) must be (false)
  }
  "returns true for different JValues" in {
    NeFieldFilterEvaluator(JString("bar"), JString("foo")) must be (true)
  }
  "returns false if value is JNothing and match value is null" in {
    NeFieldFilterEvaluator(JNothing, JNull) must be (false)
  }
  "returns true if value is JNothing and match value is not null" in {
    NeFieldFilterEvaluator(JNothing, JString("bar")) must be (true)
  }
}