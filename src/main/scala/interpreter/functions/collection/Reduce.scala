package interpreter.functions.collection

import interpreter.{ArgSet, ExecutionError, Function, Identifiable, ListValue, SequenceValue, TypeArg, Types, VectorValue}

class Reduce extends Function {
  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.function), TypeArg(Types.any), TypeArg(Types.sequence))


  def run(function: Function, acc: Identifiable, sequence: SequenceValue): Either[ExecutionError, Identifiable] = sequence match {
    case ListValue(elements) => reduceSequence(function, acc, elements)
    case VectorValue(elements) => reduceSequence(function, acc, elements)
  }


  private def reduceSequence(f: Function, initAcc: Identifiable, sequence: Seq[Identifiable]): Either[ExecutionError, Identifiable] = {
    var acc = initAcc
    for (elem <- sequence) {
      f.apply(List(acc, elem), getExecutor) match {
        case Left(err) => return Left(err)
        case Right(value) => acc = value
      }
    }
    Right(acc)
  }
}
