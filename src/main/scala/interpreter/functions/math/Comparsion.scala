package interpreter.functions.math

import interpreter.{ArgSet, BoolValue, ExecutionError, Function, Identifiable, NumericValue, TypeArg, Types}

abstract class ComparisonOperator(comparator: (Double, Double) => Either[ExecutionError, Boolean]) extends Function {
  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.numeric), TypeArg(Types.numeric))

  def run(a: NumericValue, b: NumericValue): Either[ExecutionError, Identifiable] = comparator(a.asDouble(), b.asDouble()).flatMap(d => Right(BoolValue(d)))
}

class Greater extends ComparisonOperator((a, b) => Right(a > b))

class GreaterOrEqual extends ComparisonOperator((a, b) => Right(a >= b))

class Lesser extends ComparisonOperator((a, b) => Right(a < b))

class LesserOrEqual extends ComparisonOperator((a, b) => Right(a <= b))
