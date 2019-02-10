package interpreter.functions.math

import interpreter._

abstract class LogicOperator(operator: (Boolean, Boolean) => Either[ExecutionError, Boolean]) extends Function {
  override val argTypes: Seq[Class[_]] = List(Types.any, Types.any)

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    val a = Types.asBoolean(Types.getAs[Identifiable](args, 0))
    val b = Types.asBoolean(Types.getAs[Identifiable](args, 1))
    operator(a, b).flatMap(d => Right(BoolValue(d)))
  }
}

class And extends LogicOperator((a, b) => Right(a && b))

class Or extends LogicOperator((a, b) => Right(a || b))

class Not extends Function {
  override val argTypes: Seq[Class[_]] = List(Types.any)

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    val a = Types.asBoolean(Types.getAs[Identifiable](args, 0))
    Right(BoolValue(!a))
  }
}