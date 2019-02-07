package interpreter

import parser.PrefixOperator

trait Identifiable

trait Value extends Identifiable

trait SequenceValue extends Value

trait NumericValue extends Value {
  def asDouble(): Double
  def asInt(): Int
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

object Types {
  def int: Class[_] = classOf[IntValue]
  def floating: Class[_] = classOf[FloatingValue]
  def numeric: Class[_] = classOf[NumericValue]
  def string: Class[_] = classOf[StringValue]
  def identifier: Class[_] = classOf[IdentifierValue]
  def vector: Class[_] = classOf[VectorValue]
  def list: Class[_] = classOf[ListValue]
  def sequence: Class[_] = classOf[SequenceValue]
  def function: Class[_] = classOf[Function]
  def any: Class[_] = classOf[Identifiable]
  def getAs[A](args: Seq[Any], idx: Int): A = args(idx).asInstanceOf[A]

  def validate(seq: Seq[Identifiable], types: Seq[Class[_]]): Either[ExecutionError, _] = {
    if (seq.length != types.length) return Left(InvalidNumberOfArgumentsError(seq.length, types.length))
    for ((arg, i) <- seq.view.zipWithIndex) {
     if (types(i) != identifier && !types(i).isAssignableFrom(arg.getClass)) return Left(InvalidTypeError(arg.getClass.toString, types(i).toString))
    }
    Right(0)
  }
}