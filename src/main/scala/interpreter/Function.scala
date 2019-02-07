package interpreter


trait Function extends Identifiable {
  val argTypes: Seq[Class[_]]

  protected def evaluatedArgs: Boolean = true

  protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable]

  final def apply(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    if(evaluatedArgs) executor.evalSequence(args).flatMap(evaluated => Types.validate(evaluated, argTypes).flatMap(_ => run(evaluated, executor)))
    else Types.validate(args, argTypes).flatMap(_ => run(args, executor))
  }
}
