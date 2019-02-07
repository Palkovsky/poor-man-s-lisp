package interpreter.functions.collection

import interpreter.{ExecutionError, Executor, Function, Identifiable, ListValue, SequenceValue, Types, VectorValue}

class Cons extends Function {
  override val argTypes: Seq[Class[_]] = List(Types.any, Types.sequence)

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    val element = Types.getAs[Identifiable](args, 0)
    Right(Types.getAs[SequenceValue](args, 1) match {
      case ListValue(values) => ListValue(element +: values)
      case VectorValue(values) => VectorValue(element +: values)
    })
  }
}

class Conj extends Function {
  override val argTypes: Seq[Class[_]] = List(Types.sequence, Types.any)

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    val element = Types.getAs[Identifiable](args, 1)
    Right(Types.getAs[SequenceValue](args, 0) match {
      case ListValue(values) => ListValue(values :+ element)
      case VectorValue(values) => VectorValue(values :+ element)
    })
  }
}
