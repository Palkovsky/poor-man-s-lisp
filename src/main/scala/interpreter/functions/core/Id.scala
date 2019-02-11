package interpreter.functions.core

import interpreter.{ArgSet, ExecutionError, Function, Identifiable, TypeArg, Types}

class Id extends Function{
  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.any))

  def run(arg: Identifiable): Either[ExecutionError, Identifiable] = Right(arg)
}
