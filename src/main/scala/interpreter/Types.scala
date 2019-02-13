package interpreter

import parser.PrefixOperator

import scala.collection.mutable
import scala.util.parsing.input.Positional

trait Identifiable extends Positional {
  private var context: Scope = Scope(mutable.Map())

  def setContext(context: Scope): Identifiable = {
    this.context = context
    this
  }

  def getContext: Scope = context
}

trait Value extends Identifiable

trait CollectionValue extends Value

trait SequenceValue extends CollectionValue {
  def values: Seq[Identifiable]

  def append(x: Identifiable): SequenceValue

  def concat(arr: SequenceValue): SequenceValue

  def wrap(arr: Seq[Identifiable]): SequenceValue

  override def setContext(context: Scope): Identifiable = {
    values.foreach(identifiable => identifiable.setContext(context))
    super.setContext(context)
  }
}

case class NumericValue(value: Double) extends Value {
  def asInt(): Int = value.toInt
}

case class IdentifierValue(value: String) extends Value

case class StringValue(value: String) extends Value

case class VectorValue(value: Seq[Identifiable]) extends Value with SequenceValue {
  override def values: Seq[Identifiable] = value

  override def append(x: Identifiable): SequenceValue = VectorValue(values :+ x)

  override def concat(arr: SequenceValue): SequenceValue = VectorValue(values ++ arr.values)

  override def wrap(arr: Seq[Identifiable]): SequenceValue = VectorValue(arr)
}

case class ListValue(value: Seq[Identifiable]) extends Value with SequenceValue {
  override def values: Seq[Identifiable] = value

  override def append(x: Identifiable): SequenceValue = ListValue(values :+ x)

  override def concat(arr: SequenceValue): SequenceValue = ListValue(values ++ arr.values)

  override def wrap(arr: Seq[Identifiable]): SequenceValue = ListValue(arr)
} //unevaluated list

case class PrefixedValue(prefix: PrefixOperator, value: Identifiable) extends Value

case class MapValue(map: mutable.Map[Identifiable, Identifiable]) extends Value with CollectionValue

case class NilValue() extends Value

case class BoolValue(value: Boolean) extends Value

object Types {
  def numeric: Class[_] = classOf[NumericValue]

  def string: Class[_] = classOf[StringValue]

  def identifier: Class[_] = classOf[IdentifierValue]

  def vector: Class[_] = classOf[VectorValue]

  def list: Class[_] = classOf[ListValue]

  def sequence: Class[_] = classOf[SequenceValue]

  def collection: Class[_] = classOf[CollectionValue]

  def map: Class[_] = classOf[MapValue]

  def function: Class[_] = classOf[Function]

  def bool: Class[_] = classOf[BoolValue]

  def nil: Class[_] = classOf[NilValue]

  def any: Class[_] = classOf[Identifiable]

  def getAs[A](args: Seq[Any], idx: Int): A = args(idx).asInstanceOf[A]

  def asBoolean(identifiable: Identifiable): Boolean = identifiable match {
    case VectorValue(Seq()) => false
    case ListValue(Seq()) => false // empty map should eval to false, but dunno how to match it
    case NilValue() => false
    case BoolValue(value) => value
    case _ => true
  }
}