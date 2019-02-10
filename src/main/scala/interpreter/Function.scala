package interpreter


trait Function extends Identifiable {
  val argTypes: Seq[Class[_]]

  protected def evaluatedArgs: Boolean = true
  protected def checkArity: Boolean = true
  protected def checkTypes: Boolean = true

  protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable]

  final def apply(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    if(evaluatedArgs) executor.evalSequence(args).flatMap(evaluated => Types.validate(evaluated, argTypes, checkTypes, checkArity).flatMap(_ => run(evaluated, executor)))
    else Types.validate(args, argTypes, checkTypes, checkArity).flatMap(_ => run(args, executor))
  }
}
