package interpreter.functions.math

import interpreter._

abstract class LogicOperator(operator: (Boolean, Boolean) => Either[ExecutionError, Boolean]) extends Function {
  override val argSets: Seq[ArgSet] =  ArgSet.single(TypeArg(Types.any), TypeArg(Types.any))

  def run(a: Identifiable, b: Identifiable): Either[ExecutionError, Identifiable] = operator(Types.asBoolean(a), Types.asBoolean(b)).flatMap(d => Right(BoolValue(d)))

}

class And extends LogicOperator((a, b) => Right(a && b))

class Or extends LogicOperator((a, b) => Right(a || b))

class Not extends Function {
  override val argSets: Seq[ArgSet] =  ArgSet.single(TypeArg(Types.any))

  def run(a: Identifiable): Either[ExecutionError, Identifiable] = Right(BoolValue(!Types.asBoolean(a)))
}