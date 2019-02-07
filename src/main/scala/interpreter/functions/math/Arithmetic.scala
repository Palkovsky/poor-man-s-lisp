package interpreter.functions.math

import interpreter.{DividedByZeroError, ExecutionError, Executor, FloatingValue, Function, Identifiable, NumericValue, Types}

abstract class ArithmeticOperator extends Function {
  override val types: Seq[Class[_]] = List(Types.numeric, Types.numeric)

  override protected def run(args: Seq[Identifiable],executor: Executor): Either[ExecutionError, Identifiable] = {
    val a = Types.getAs[NumericValue](args, 0)
    val b = Types.getAs[NumericValue](args, 1)
    calculate(a.asDouble(), b.asDouble()).flatMap(d => Right(FloatingValue(d)))
  }

  protected def calculate(a: Double, b: Double): Either[ExecutionError, Double]
}

class Plus extends ArithmeticOperator {
  override protected def calculate(a: Double, b: Double): Either[ExecutionError, Double] = Right(a + b)
}

class Minus extends ArithmeticOperator {
  override protected def calculate(a: Double, b: Double): Either[ExecutionError, Double] = Right(a - b)
}

class Mult extends ArithmeticOperator {
  override protected def calculate(a: Double, b: Double): Either[ExecutionError, Double] = Right(a * b)
}

class Div extends ArithmeticOperator {
  override protected def calculate(a: Double, b: Double): Either[ExecutionError, Double] = {
    if (b == 0) Left(DividedByZeroError())
    Right(a / b)
  }
}

class Mod extends ArithmeticOperator{
  override protected def calculate(a: Double, b: Double): Either[ExecutionError, Double] = Right(a % b)
}