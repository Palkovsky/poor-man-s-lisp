package interpreter.functions.collection

import interpreter.{ExecutionError, Executor, Function, Identifiable, ListValue, SequenceValue, Types, VectorValue}

class Cons extends Function {
  override val types: Seq[Class[_]] = List(Types.any, Types.sequence)

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    val element = Types.getAs[Identifiable](args, 0)
    Right(Types.getAs[SequenceValue](args, 1) match {
      case ListValue(values) => ListValue(element +: values)
      case VectorValue(values) => VectorValue(element +: values)
    })
  }
}
