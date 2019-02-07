package interpreter.functions.math
import interpreter.{BoolValue, ExecutionError, Executor, Function, Identifiable, NumericValue, Types}

abstract class ComparisonOperator extends Function {
  override val argTypes: Seq[Class[_]] = List(Types.numeric, Types.numeric)

  override protected def run(args: Seq[Identifiable],executor: Executor): Either[ExecutionError, Identifiable] = {
    val a = Types.getAs[NumericValue](args, 0)
    val b = Types.getAs[NumericValue](args, 1)
    calculate(a.asDouble(), b.asDouble()).flatMap(d => Right(BoolValue(d)))
  }

  protected def calculate(a: Double, b: Double): Either[ExecutionError, Boolean]
}

class Greater extends ComparisonOperator {
  override protected def calculate(a: Double, b: Double): Either[ExecutionError, Boolean] = Right(a > b)
}

class GreaterOrEqual extends ComparisonOperator {
  override protected def calculate(a: Double, b: Double): Either[ExecutionError, Boolean] = Right(a >= b)
}

class Lesser extends ComparisonOperator {
  override protected def calculate(a: Double, b: Double): Either[ExecutionError, Boolean] = Right(a < b)
}

class LesserOrEqual extends ComparisonOperator {
  override protected def calculate(a: Double, b: Double): Either[ExecutionError, Boolean] = Right(a <= b)
}