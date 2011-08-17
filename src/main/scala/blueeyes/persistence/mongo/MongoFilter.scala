package blueeyes.persistence.mongo

import scala.collection.immutable.ListSet
import scala.math.BigInt
import blueeyes.json.{JPath, JPathIndex, JPathField}
import blueeyes.json.JsonAST._
import blueeyes.util.ProductPrefixUnmangler

import scalaz._
import Scalaz._

object MongoFilterOperators {
  sealed trait MongoFilterOperator extends Product with ProductPrefixUnmangler {
    def symbol: String = unmangledName

    def unary_! : MongoFilterOperator

    override def toString = symbol
  }
  sealed trait MongoFilterOperatorBound extends MongoFilterOperator
  case object $gt   extends MongoFilterOperatorBound  { def unary_! = $lte; }
  case object $gte  extends MongoFilterOperatorBound  { def unary_! = $lt; }
  case object $lt   extends MongoFilterOperatorBound  { def unary_! = $gte; }
  case object $lte  extends MongoFilterOperatorBound  { def unary_! = $gt; }

  sealed trait MongoFilterOperatorEquality extends MongoFilterOperator
  case object $eq extends MongoFilterOperatorEquality { def unary_! = $ne; } // This is a virtual operator, it's not real!!!!
  case object $ne extends MongoFilterOperatorEquality { def unary_! = $eq; }
  case object $regex extends MongoFilterOperatorEquality { def unary_! : MongoFilterOperator  = sys.error("The $regex operator does not have a negation"); }

  sealed trait MongoFilterOperatorContainment extends MongoFilterOperator
  case object $in    extends MongoFilterOperatorContainment { def unary_! = $nin; }
  case object $nin   extends MongoFilterOperatorContainment { def unary_! = $in; }

  case object $mod         extends MongoFilterOperator { def unary_! : MongoFilterOperator = sys.error("The $mod operator does not have a negation"); }
  case object $all         extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $all operator does not have a negation"); }
  case object $size        extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $size operator does not have a negation"); }
  case object $exists      extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $exists operator does not have a negation"); }
  case object $type        extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $type operator does not have a negation"); }
  case object $or          extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $or operator does not have a negation"); }
  case object $each        extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $each operator does not have a negation"); }

  sealed trait MongoFilterOperatorGeo extends MongoFilterOperator
  case object $near        extends MongoFilterOperatorGeo { def unary_! : MongoFilterOperator  = sys.error("The $near operator does not have a negation"); }
  case object $nearSphere  extends MongoFilterOperatorGeo { def unary_! : MongoFilterOperator  = sys.error("The $nearSphere operator does not have a negation"); }
  case object $within      extends MongoFilterOperatorGeo { def unary_! : MongoFilterOperator  = sys.error("The $within operator does not have a negation"); }
}

import MongoFilterOperators._

sealed trait MongoFilter { self =>
  def filter: JValue

  def unary_! : MongoFilter

  def & (that: MongoFilter)  = &&(that)

  def & (collection: MongoCollection) = MongoFilterCollection(self, collection)

  def && (that: MongoFilter) = MongoFilterAndMonoid.append(self, that)

  def | (that: MongoFilter)  = ||(that)

  def || (that: MongoFilter) = MongoFilterOrMonoid.append(self, that)
}

object MongoFilter extends MongoFilterImplicits {
  def apply(lhs: JPath, operator: MongoFilterOperator, rhs: MongoPrimitive): MongoFilter = rhs match {
    case opt @ MongoPrimitiveOption(value) => value.map(MongoFieldFilter(lhs, operator, _)).getOrElse(MongoFilterAll)
    case _ => MongoFieldFilter(lhs, operator, rhs)
  }
}

case class MongoFilterCollection(filter: MongoFilter, collection: MongoCollection){ self =>
  def & (database: MongoDatabase) = MongoFilterCollectionDatabase(self, database)
}

case class MongoFilterCollectionDatabase(filter: MongoFilterCollection, database: MongoDatabase)

case object MongoFilterAll extends MongoFilter {
  def filter: JValue = JObject(Nil)

  def unary_! : MongoFilter = this
}

sealed case class MongoFieldFilter(lhs: JPath, operator: MongoFilterOperator, rhs: MongoPrimitive) extends MongoFilter { self =>
  def filter: JValue = {
    val value = operator match {
      case $eq | $regex | $near | $nearSphere | $within => rhs.toJValue

      case _ => JObject(JField(operator.symbol, rhs.toJValue) :: Nil)
    }
    lhs.nodes match {
      case Nil => value
      case _   => JObject(JField(JPathExtension.toMongoField(lhs), value) :: Nil)
    }
  }

  def unary_! : MongoFilter = MongoFieldFilter(lhs, !operator, rhs)
}

sealed case class MongoOrFilter(queries: ListSet[MongoFilter]) extends MongoFilter {
  def filter: JValue = JObject(JField($or.symbol, JArray(queries.toList.map(_.filter))) :: Nil)

  def unary_! : MongoFilter = MongoAndFilter(queries.map(!_))
}

sealed case class MongoAndFilter(queries: ListSet[MongoFilter]) extends MongoFilter { self =>
  def filter: JValue = {
    val (notEqs, eqs) = queries partition { 
      case MongoFieldFilter(lhs, $eq, rhs) => false
      case _ => true
    }

    JObject(notEqsQuery(notEqs).fields ::: eqsQuery(eqs).fields)
  }

  private def notEqsQuery(queries: ListSet[MongoFilter]) = {
    val objects = queries.map(_.filter).toList collect { case obj: JObject => obj }
    (JObject(Nil) /: objects) { _ merge _ }
  }
  private def eqsQuery(queries: ListSet[MongoFilter]) = {
    val fields = queries.collect{ case MongoFieldFilter(lhs, _, rhs) => JField(JPathExtension.toMongoField(lhs), rhs.toJValue) }.toList
    JObject(fields)
  }

  def unary_! : MongoFilter = MongoOrFilter(queries.map(!_))

  def elemMatch(path: JPath) = MongoElementsMatchFilter(path, self)
}

sealed case class MongoElementsMatchFilter(lhs: JPath, elementsQuery: MongoAndFilter) extends MongoFilter{
  def unary_! = sys.error("The $elemMatch operator does not have a negation")

  def filter = {
    val value = JObject(JField("$elemMatch", elementsQuery.filter) :: Nil)
    lhs.nodes match{
      case Nil => value
      case _   => JObject(JField(JPathExtension.toMongoField(lhs), value) :: Nil)
    }
  }
}

sealed trait MongoPrimitive {
  def toJValue: JValue
}

case class MongoPrimitiveOption(value: Option[MongoPrimitive]) extends MongoPrimitive {
  def toJValue = value.map(_.toJValue).getOrElse(JNothing)
}
case class MongoPrimitiveString(value: String) extends MongoPrimitive {
  def toJValue = JString(value)
}
case class MongoPrimitiveLong(value: Long) extends MongoPrimitive {
  def toJValue = JInt(value)
}
case class MongoPrimitiveInt(value: Int) extends MongoPrimitive {
  def toJValue = JInt(value)
}
case class MongoPrimitiveBigInt(value: BigInt) extends MongoPrimitive {
  def toJValue = JInt(value)
}
case class MongoPrimitiveDouble(value: Double) extends MongoPrimitive {
  def toJValue = JDouble(value)
}
case class MongoPrimitiveBoolean(value: Boolean) extends MongoPrimitive {
  def toJValue = JBool(value)
}
case class MongoPrimitiveArray(value: List[MongoPrimitive]) extends MongoPrimitive {
  def toJValue = JArray(value.map(_.toJValue))
}
case class MongoPrimitiveJObject(value: JObject) extends MongoPrimitive {
  def toJValue = value
}
case object MongoPrimitiveNull extends MongoPrimitive {
  def toJValue = JNull
}

sealed class MongoPrimitiveWitness[T](val typeNumber: Int)

trait MongoFilterImplicits {
  import MongoFilterOperators._

  implicit def mongoOperatorToSymbolString(op: MongoFilterOperator): String = op.symbol

  implicit def stringToMongoFilterBuilder(string: String): MongoFilterBuilder = MongoFilterBuilder(JPath(string))

  implicit def jpathToMongoFilterBuilder(jpath: JPath): MongoFilterBuilder = MongoFilterBuilder(jpath)

  implicit def jvalueToMongoPrimitive(value: JValue): MongoPrimitive = value match {
    case x: JString => MongoPrimitiveString(x.value)
    case x: JInt    => MongoPrimitiveLong(x.value.longValue())
    case x: JDouble => MongoPrimitiveDouble(x.value)
    case x: JBool   => MongoPrimitiveBoolean(x.value)
    case x: JObject => MongoPrimitiveJObject(x)
    case x: JArray  => MongoPrimitiveArray(x.elements.map(jvalueToMongoPrimitive))
    case JNull | JNothing => MongoPrimitiveNull
  }

  implicit def optionToMongoPrimitive[T <% MongoPrimitive](value: Option[T]) = MongoPrimitiveOption(value.map(a => a : MongoPrimitive))
  implicit def stringToMongoPrimitive(value: String)   = MongoPrimitiveString(value)
  implicit def longToMongoPrimitive(value: Long)       = MongoPrimitiveLong(value)
  implicit def intToMongoPrimitive(value: Int)         = MongoPrimitiveInt(value)
  implicit def bigIntToMongoPrimitive(value: BigInt)   = MongoPrimitiveBigInt(value)
  implicit def doubleToMongoPrimitive(value: Double)   = MongoPrimitiveDouble(value)
  implicit def booleanToMongoPrimitive(value: Boolean) = MongoPrimitiveBoolean(value)
  implicit def arrayToMongoPrimitive(value: List[MongoPrimitive]) = MongoPrimitiveArray(value)

  /*
  Double	 1
  String	 2
  Object	 3
  Array	 4
  Binary data	 5
  Object id	 7
  Boolean	 8
  Date	 9
  Null	 10
  Regular expression	 11
  JavaScript code	 13
  Symbol	 14
  JavaScript code with scope	 15
  32-bit integer	 16
  Timestamp	 17
  64-bit integer	 18
  Min key	 255
  Max key	 127
  */
  implicit val MongoPrimitiveJStringWitness = new MongoPrimitiveWitness[JString](2)
  implicit val MongoPrimitiveJDoubleWitness = new MongoPrimitiveWitness[JDouble](1)
  implicit val MongoPrimitiveJObjectWitness = new MongoPrimitiveWitness[JObject](3)
  implicit val MongoPrimitiveJArrayWitness  = new MongoPrimitiveWitness[JArray](4)
  implicit val MongoPrimitiveJBoolWitness   = new MongoPrimitiveWitness[JBool](8)
  implicit val MongoPrimitiveJNullWitness   = new MongoPrimitiveWitness[JNull.type](10)
  implicit val MongoPrimitiveJIntWitness    = new MongoPrimitiveWitness[JInt](18)
}
object MongoFilterImplicits extends MongoFilterImplicits

sealed trait WithinShape
case class Box(lowerLeft: (Double, Double), upperRight: (Double, Double))  extends WithinShape
case class Circle(center: (Double, Double), radius: Double)    extends WithinShape
case class Polygon(points: (Double, Double) *)                 extends WithinShape
case class CenterSphere(center: (Double, Double), radiusInRadians: Double) extends WithinShape

/** The MongoFilterBuilder creates mongo filters. Filters can be used in MongoQuery in where clause.
 * <p>
 * <pre>
 * import blueeyes.persistence.mongo.MongoImplicits._
 * import blueeyes.persistence.mongo.MongoQueryBuilder._
 *
 * val filter = "foo.bar" === "blahbMongoPrimitiveJObjectlah"
 *
 * val query  = selectOne().from("mycollection").where(filter)
 * val query2 = selectOne().from("mycollection").where("foo.bar" === "blahblah")
 * </pre>
 * <p>
 * Filters can be combined.
 * <p>
 * <pre>
 * val filter  = ("foo.bar" === "blahblah") | ("foo.bar" === "blah")
 * val filter2 = ("foo" === "blahblah") & ("bar" === "blah")
 * </pre>
 * <p>
 */
case class MongoFilterBuilder(jpath: JPath) {
  import MongoFilterImplicits._
  def === [T](value: MongoPrimitive) = MongoFieldFilter(jpath, $eq, value)

  def !== [T](value: MongoPrimitive) = MongoFieldFilter(jpath, $ne, value)

  def >  [T](value: MongoPrimitive) = MongoFieldFilter(jpath, $gt, value)

  def >= [T](value: MongoPrimitive) = MongoFieldFilter(jpath, $gte, value)

  def <  [T](value: MongoPrimitive) = MongoFieldFilter(jpath, $lt, value)

  def <= [T](value: MongoPrimitive) = MongoFieldFilter(jpath, $lte, value)

  def anyOf[T <: MongoPrimitive](items: T*) = MongoFieldFilter(jpath, $in, List(items: _*))

  def contains[T <: MongoPrimitive](items: T*) = MongoFieldFilter(jpath, $all, List(items: _*))

  def hasSize(length: Int) =  MongoFieldFilter(jpath, $size, length)

  def isDefined = MongoFieldFilter(jpath, $exists, true)

  def regex(regex: String, options: String = "") = MongoFieldFilter(jpath, $regex, JObject(List(JField("$regex", JString(regex)), JField("$options", JString(options)))))

  def hasType[T](implicit witness: MongoPrimitiveWitness[T]) = MongoFieldFilter(jpath, $type, witness.typeNumber)

  def near(x: Double, y: Double, maxDistance: Option[Double] = None) = nearQuery($near, x, y, maxDistance)

  def nearSphere(x: Double, y: Double, maxDistance: Option[Double] = None) = nearQuery($nearSphere, x, y, maxDistance)

  private def nearQuery(operator: MongoFilterOperator, x: Double, y: Double, maxDistance: Option[Double] = None) = {
    val nearField = JField(operator.unmangledName, JArray(List(JDouble(x), JDouble(y))))
    val fields    = maxDistance.map(v => List(JField("$maxDistance", JDouble(v)))).getOrElse(Nil)
    MongoFieldFilter(jpath, operator, JObject(nearField :: fields))
  }

  def within(shape: WithinShape) = {
    val withinValue = shape match{
      case x: Box          => JField("$box",     JArray(JArray(JDouble(x.lowerLeft._1) :: JDouble(x.lowerLeft._2) :: Nil) :: JArray(JDouble(x.upperRight._1) :: JDouble(x.upperRight._2) :: Nil) :: Nil))
      case x: Circle       => JField("$center",  JArray(JArray(JDouble(x.center._1) :: JDouble(x.center._2) :: Nil) :: JDouble(x.radius) :: Nil))
      case x: Polygon      => JField("$polygon", JArray(x.points.toList.map(point => JArray(JDouble(point._1) :: JDouble(point._2) :: Nil))))
      case x: CenterSphere => JField("$centerSphere", JArray(JArray(JDouble(x.center._1) :: JDouble(x.center._2) :: Nil) :: JDouble(x.radiusInRadians) :: Nil))
    }
    MongoFieldFilter(jpath, $within, JObject(JField("$within", JObject(withinValue :: Nil)) :: Nil))
  }
}

private[mongo] object JPathExtension{
  def toMongoField(path: JPath) = {
    val mongoPath = JPath(path.nodes.map{ node =>
      node match{
        case e: JPathIndex => JPathField(e.index.toString)
        case _ => node
      }
    })
    if (mongoPath.path.startsWith(".")) mongoPath.path.substring(1) else mongoPath.path
  }
}
