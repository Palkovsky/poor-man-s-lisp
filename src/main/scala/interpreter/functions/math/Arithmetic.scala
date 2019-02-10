package interpreter.functions.math

import interpreter.{ExecutionError, Executor, FloatingValue, Function, Identifiable, NumericValue, Types}

abstract class ArithmeticOperator(operator: (Double, Double) => Either[ExecutionError, Double]) extends Function {
  override val argTypes: Seq[Class[_]] = List(Types.numeric, Types.numeric)

  override protected def run(args: Seq[Identifiable],executor: Executor): Either[ExecutionError, Identifiable] = {
    val a = Types.getAs[NumericValue](args, 0)
    val b = Types.getAs[NumericValue](args, 1)
    operator(a.asDouble(), b.asDouble()).flatMap(d => Right(FloatingValue(d)))
  }
}

class Plus extends ArithmeticOperator((a, b) => Right(a+b))
class Minus extends ArithmeticOperator((a, b) => Right(a-b))
class Mult extends ArithmeticOperator((a, b) => Right(a*b))
class Div extends ArithmeticOperator((a, b) => Right(a/b))
class Mod extends ArithmeticOperator((a, b) => Right(a%b))
