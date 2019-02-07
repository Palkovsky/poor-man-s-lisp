package interpreter


import parser.{BacktickOperator, RootExpression}

import scala.collection.mutable.ListBuffer

class Executor(astRoot: RootExpression) {

  val scopeManager: ScopeManager = ScopeManager.default()
  val program: Seq[Identifiable] = Converter.convert(astRoot)


  // Stop execution after first error.
  // If everything went good return from last expression.
  def run(): Either[ExecutionError, Identifiable] = program.foldLeft[Either[ExecutionError, Identifiable]](Right(IntValue(0)))((acc, list) => {
    if (acc.isLeft) acc else eval(list)
  })

  def eval(node: Identifiable): Either[ExecutionError, Identifiable] = node match {
    // For literals just return whatever it is
    case IntValue(value) => Right(IntValue(value))
    case FloatingValue(value) => Right(FloatingValue(value))
    case StringValue(value) => Right(StringValue(value))

    // Looking up an identifier
    case IdentifierValue(value) => scopeManager.get(value) match {
        case None => Left(UnknownIdentifier(value))
        case Some(identifiable) => Right(identifiable)
      }

    // Evaluate whole vector
    case VectorValue(values) => evalSequence(values).map(elems => VectorValue(elems))

    // Backtick operator prevents evaluation
    case PrefixedValue(BacktickOperator(), value) => Right(value)

    // Not implementing hashmaps forn now
    //case HashMapLiteral(map) => Right(map)

    // Lists allow for function calling
    case ListValue(Seq()) => Left(EmptyListError())
    case ListValue(ListValue(values) +: Seq()) => eval(ListValue(values)).flatMap(res => eval(res))
    case ListValue(ListValue(values) +: tail) => eval(ListValue(values)).flatMap(result => eval(ListValue(result +: tail)))

    case ListValue((f: Function) +: Seq()) => f.apply(List.empty, this)
    case ListValue((f: Function) +: tail) => evalSequence(tail).flatMap(args => f.apply(args, this))

    case ListValue(IdentifierValue(value) +: tail) => scopeManager.get(value) match {
      case Some(identifiable) =>
        if (identifiable.isInstanceOf[Value]) Left(NotCallableError())
        val function = identifiable.asInstanceOf[Function]
        evalSequence(tail).flatMap(args => function.apply(args, this))
      case None => Left(UnknownIdentifier(value))
    }
  }

  // Evaluates VectorLiteral
  private def evalSequence(values: Seq[Identifiable]): Either[ExecutionError, Seq[Identifiable]] = {
    val vector = ListBuffer[Identifiable]()
    for (value <- values) {
      eval(value) match {
        case Right(v) => vector += v
        case Left(err) => return Left(err) // maybe we should try accumulating errors?
      }
    }
    Right(vector.toList)
  }
}
