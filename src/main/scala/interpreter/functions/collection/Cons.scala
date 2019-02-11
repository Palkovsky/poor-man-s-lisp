package interpreter.functions.collection

import interpreter.{ArgSet, ExecutionError, Function, Identifiable, ListValue, SequenceValue, TypeArg, Types, VectorValue}

class Cons extends Function {
  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.any), TypeArg(Types.sequence))

  def run(element: Identifiable, sequence: SequenceValue): Either[ExecutionError, Identifiable] = sequence match {
    case ListValue(values) => Right(ListValue(element +: values))
    case VectorValue(values) => Right(VectorValue(element +: values))
  }
}

class Conj extends Function {
  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.sequence), TypeArg(Types.any))

  def run(sequence: SequenceValue, element: Identifiable): Either[ExecutionError, Identifiable] = sequence match {
    case ListValue(values) => Right(ListValue(values :+ element))
    case VectorValue(values) => Right(VectorValue(values :+ element))
  }
}
