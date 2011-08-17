package blueeyes.persistence.mongo.mock

import blueeyes.json.JPath
import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo.JPathExtension._

private[mock] trait JObjectFields{
  def selectByPath(selectionPath: JPath, jobject: JObject, transformer: (JValue) => Option[JValue], jobjectRestorer: (JPath, JValue) => JValue) = {
    val x = jobject.get(selectionPath)
    
    transformer(x).map(jobjectRestorer(selectionPath, _))
  }

  def selectFields(jobjects: List[JObject], selection : Set[JPath], transformer: (JValue) => Option[JValue], jobjectRestorer: (JPath, JValue) => JValue) = {
    if (!selection.isEmpty) {
      val allJFields = jobjects.map(jobject => selection.map(selectByPath(_, jobject, transformer, jobjectRestorer)))
      allJFields.map(jfields => {
        val definedJFields = jfields.flatten map { _ asUnsafe JObject }
        if (definedJFields.isEmpty)
          None
        else
          Some(definedJFields reduceLeft { _ ++ _ })
      }).flatten
    } else jobjects
  }

  def selectExistingFields(jobjects: List[JObject], selection : Set[JPath]) = {
    def updateValue(value: JValue) = value match{
      case JNothing => None
      case _ => Some(value)
    }
    selectFields(jobjects, selection, updateValue _, jvalueToJObject _)
  }

  def jvalueToJObject(path: JPath, value: JValue) = {
    val elements = toMongoField(path).split("\\.").reverse
    elements.tail.foldLeft(JObject(JField(elements.head, value) :: Nil)){(result, element) => JObject(JField(element, result) :: Nil)}
  }
}
