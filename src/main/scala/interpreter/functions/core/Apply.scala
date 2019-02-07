package interpreter.functions.core

import interpreter.{ExecutionError, Executor, Function, Identifiable, ListValue, SequenceValue, Types, VectorValue}

class Apply extends Function{
  override val argTypes: Seq[Class[_]] = List(Types.function, Types.sequence)

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    val f = Types.getAs[Function](args, 0)
    val fArgs = Types.getAs[SequenceValue](args, 1)
    fArgs match {
      case VectorValue(values) => f.apply(values, executor)
      case ListValue(values) => f.apply(values, executor)
    }
  }
}
