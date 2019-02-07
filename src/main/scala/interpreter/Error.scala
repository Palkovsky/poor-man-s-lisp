package interpreter


trait ExecutionError
case class GenericError(msg: String) extends ExecutionError
case class UnknownIdentifier(name: String) extends ExecutionError
case class EmptyListError() extends ExecutionError
case class NoListIdentifierError() extends ExecutionError
case class InvalidNumberOfArgumentsError(passed: Int, expected: Int) extends ExecutionError
case class InvalidTypeError(passed: String, expected: String) extends ExecutionError
case class NotCallableError(value: String) extends ExecutionError
case class DividedByZeroError() extends ExecutionError