package interpreter.functions.core

import interpreter.{ArgSet, ExecutionError, Function, Identifiable, ListValue, SequenceValue, TypeArg, Types, VectorValue}

class Apply extends Function {
  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.function), TypeArg(Types.sequence))

  def run(f: Function, fArgs: SequenceValue): Either[ExecutionError, Identifiable] = fArgs match {
    case VectorValue(values) => f.apply(values, getExecutor)
    case ListValue(values) => f.apply(values, getExecutor)
  }
}
