package interpreter


trait Function extends Identifiable {
  val types: Seq[Class[_]]

  def arity: Int = types.length

  protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable]

  final def apply(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    Types.validate(args, types).flatMap(_ => run(args, executor))
  }
}
