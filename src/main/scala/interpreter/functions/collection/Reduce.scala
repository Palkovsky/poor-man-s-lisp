package interpreter.functions.collection

import interpreter.{CollectionValue, ExecutionError, Executor, Function, Identifiable, ListValue, Types, VectorValue}

class Reduce extends Function {
  override val argTypes: Seq[Class[_]] = List(Types.function, Types.any, Types.sequence)

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    val function = Types.getAs[Function](args, 0)
    val acc = Types.getAs[Identifiable](args, 1)
    Types.getAs[CollectionValue](args, 2) match {
      case ListValue(elements) => reduceSequence(function, acc, elements, executor)
      case VectorValue(elements) => reduceSequence(function, acc, elements, executor)
    }
  }

  private def reduceSequence(f: Function, initAcc: Identifiable, sequence: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    var acc = initAcc
    for (elem <- sequence) {
      f.apply(List(acc, elem), executor) match {
        case Left(err) => return Left(err)
        case Right(value) => acc = value
      }
    }
    Right(acc)
  }
}
