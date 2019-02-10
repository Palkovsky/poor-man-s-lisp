package interpreter

import parser.PrefixOperator

import scala.collection.mutable

trait Identifiable

trait Value extends Identifiable

trait CollectionValue extends Value

trait SequenceValue extends CollectionValue

trait NumericValue extends Value {
  def asDouble(): Double

  def asInt(): Int

  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[NumericValue]) false
    obj.asInstanceOf[NumericValue].asDouble() == asDouble()
  }
}

case class FloatingValue(value: Double) extends NumericValue {
  override def asDouble(): Double = value

  override def asInt(): Int = value.toInt
}

case class IntValue(value: Int) extends NumericValue {
  override def asDouble(): Double = value.toDouble

  override def asInt(): Int = value
}

case class IdentifierValue(value: String) extends Value

case class StringValue(value: String) extends Value

case class VectorValue(value: Seq[Identifiable]) extends Value with SequenceValue

case class ListValue(value: Seq[Identifiable]) extends Value with SequenceValue //unevaluated list
case class PrefixedValue(prefix: PrefixOperator, value: Identifiable) extends Value

case class MapValue(map: mutable.Map[Identifiable, Identifiable]) extends Value with CollectionValue

case class NilValue() extends Value

case class BoolValue(value: Boolean) extends Value

object Types {
  def int: Class[_] = classOf[IntValue]

  def floating: Class[_] = classOf[FloatingValue]

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

  def validate(seq: Seq[Identifiable], types: Seq[Class[_]]): Either[ExecutionError, _] = {
    if (seq.length != types.length) return Left(InvalidNumberOfArgumentsError(seq.length, types.length))
    for ((arg, i) <- seq.view.zipWithIndex) {
      if (types(i) != identifier && !types(i).isAssignableFrom(arg.getClass)) return Left(InvalidTypeError(arg.getClass.toString, types(i).toString))
    }
    Right(0)
  }

  def asBoolean(identifiable: Identifiable): Boolean = identifiable match {
    case VectorValue(Seq()) => false
    case ListValue(Seq()) => false // empty map should eval to false, but dunno how to match it
    case NilValue() => false
    case BoolValue(value) => value
    case _ => true
  }
}