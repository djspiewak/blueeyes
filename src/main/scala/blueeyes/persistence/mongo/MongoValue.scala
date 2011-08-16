package blueeyes.persistence.mongo

import org.joda.time.DateTime

sealed trait MongoValue{
  type Unboxed
  def unbox: Unboxed
}

case class MongoBoolean(value: Boolean) extends MongoValue{
  type Unboxed = Boolean
  def unbox = value
}

case class MongoInt(value: Int) extends MongoValue{
  type Unboxed = Int
  def unbox = value
}

case class MongoLong(value: Long) extends MongoValue{
  type Unboxed = Long
  def unbox = value
}

case class MongoDouble(value: Double) extends MongoValue{
  type Unboxed = Double
  def unbox = value
}

case class MongoFloat(value: Float) extends MongoValue{
  type Unboxed = Float
  def unbox = value
}

case class MongoString(value: String) extends MongoValue{
  type Unboxed = String
  def unbox = value
}

case class MongoRegex(pattern: String, options: Option[Int]) extends MongoValue{
  type Unboxed = (String, Int)
  def unbox = (pattern, options.getOrElse(0))
}

case class MongoBinary(value: Array[Byte]) extends MongoValue{
  type Unboxed = Array[Byte]
  def unbox  = value
}

case class MongoDate(value: DateTime) extends MongoValue{
  type Unboxed = DateTime
  def unbox = value
}

case object MongoNull extends MongoValue{
  type Unboxed = Null
  def unbox = null
}

case class MongoField(name: String, value: MongoValue) extends MongoValue{
  type Unboxed = (String, value.Unboxed)
  def unbox = (name, value.unbox)
}

case class MongoArray(elements: List[MongoValue]) extends MongoValue{
  type Unboxed = List[Any]
  def unbox = elements.map(_.unbox)
}

case class MongoObject(fields: List[MongoField]) extends MongoValue{
  type Unboxed = Map[String, Any]
  def unbox = Map() ++ fields.map(_.unbox : (String, Any))

  override lazy val hashCode = Set(this.fields: _*).hashCode

  override def equals(that: Any): Boolean = that match {
    case that: MongoObject if (this.fields.length == that.fields.length) => Set(this.fields: _*) == Set(that.fields: _*)
    case _ => false
  }
}