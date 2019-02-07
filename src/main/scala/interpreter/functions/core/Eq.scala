package interpreter.functions.core

import interpreter.{BoolValue, ExecutionError, Executor, Function, Identifiable, Types}

class Eq extends Function {
  override val argTypes: Seq[Class[_]] = List(Types.any, Types.any)

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    val a = Types.getAs[Identifiable](args, 0)
    val b = Types.getAs[Identifiable](args, 1)
    Right(BoolValue(a.equals(b)))
  }
}