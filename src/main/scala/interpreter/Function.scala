package interpreter


trait Function extends Identifiable {

  protected val argSets: Seq[ArgSet]
  private var executor: Executor = _

  protected def evaluatedArgs: Boolean = true

  private def matchingArgSet(args: Seq[Identifiable]): Either[ExecutionError, ArgSet] = {
    for (argSet <- argSets) {
      if (argSet.matching(args)) return Right(argSet)
    }
    Left(InvalidTypeError(args, argSets))
  }

  protected def getExecutor: Executor = this.executor

  protected def run(args: Seq[Identifiable]): Either[ExecutionError, Identifiable] = Left(GenericError("Unable to locate function code."))

  final def apply(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    this.executor = executor


    val finalArgs: Either[ExecutionError, Seq[Identifiable]] = if (evaluatedArgs) executor.evalSequence(args) else Right(args)
    finalArgs.flatMap(evaluated => {
      matchingArgSet(evaluated).flatMap(argSet => {
        try {
          val method = argSet.findMethod(getClass, "run")
          method.invoke(this, evaluated: _*).asInstanceOf[Either[ExecutionError, Identifiable]]
        } catch {
          case _: NoSuchMethodException =>
            run(evaluated)
        }
      })
    })
  }
}
