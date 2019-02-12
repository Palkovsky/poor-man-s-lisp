package interpreter

import scala.util.parsing.input.Positional


trait ExecutionError extends Positional {
  override def toString: String = s"$asString | Line ${pos.line} | Column ${pos.column}"

  protected def asString: String
}

case class GenericError(msg: String) extends ExecutionError {
  override def asString: String = msg
}
case class UnknownIdentifier(identifier: String) extends ExecutionError {
  override def asString: String = s"Unknown indetifier: $identifier."
}

case class InvalidNumberOfArgumentsError(passed: Int, expected: Int) extends ExecutionError {
  override def asString: String = s"Invalid number of arguments. Expected $expected, but passed $passed."
}

case class InvalidTypeError(passed: Seq[Identifiable], expected: Seq[ArgSet]) extends ExecutionError {
  override protected def asString: String = s"Passed '$passed', but expecting '$expected'."
}

case class NotCallableError(identifiable: Identifiable) extends ExecutionError {
  override def asString: String = s"'$identifiable' is not callable."
}

case class NotEvaluable(identifiable: Identifiable) extends ExecutionError {
  override def asString: String = s"$identifiable is not evaluable."
}

case class CriticalError(exception: Throwable) extends ExecutionError {
  override protected def asString: String = exception.toString
}