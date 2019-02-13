package interpreter.functions.math

import interpreter.{ArgSet, ExecutionError, Function, Identifiable, NumericValue, TypeArg, Types}

abstract class ArithmeticOperator(operator: (Double, Double) => Either[ExecutionError, Double]) extends Function {
  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.numeric), TypeArg(Types.numeric))

  def run(a: NumericValue, b: NumericValue): Either[ExecutionError, Identifiable] = operator(a.value, b.value).flatMap(d => Right(NumericValue(d)))

}

class Plus extends ArithmeticOperator((a, b) => Right(a + b))
class Minus extends ArithmeticOperator((a, b) => Right(a - b))
class Mult extends ArithmeticOperator((a, b) => Right(a * b))
class Div extends ArithmeticOperator((a, b) => Right(a / b))
class Mod extends ArithmeticOperator((a, b) => Right(a % b))
