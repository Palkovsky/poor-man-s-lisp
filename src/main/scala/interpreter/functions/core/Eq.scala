package interpreter.functions.core

import interpreter.{ArgSet, BoolValue, ExecutionError, Function, Identifiable, TypeArg, Types}

class Eq extends Function {
  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.any), TypeArg(Types.any))

  def run(a: Identifiable, b: Identifiable): Either[ExecutionError, Identifiable] = Right(BoolValue(a.equals(b)))
}