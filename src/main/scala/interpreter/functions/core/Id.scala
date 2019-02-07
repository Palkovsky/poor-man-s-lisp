package interpreter.functions.core

import interpreter.{ExecutionError, Executor, Function, Identifiable, Types}

class Id extends Function{
  override val argTypes: Seq[Class[_]] = List(Types.any)

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = Right(Types.getAs[Identifiable](args, 0))
}
