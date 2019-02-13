package interpreter


import parser.{BacktickOperator, RootExpression}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Executor() {

  val scopeManager: ScopeManager = ScopeManager.default()


  def run(astRoot: RootExpression): Either[ExecutionError, Identifiable] = {
    // Catch any exception so it will be passed ass ExecutionError
    try {
      val program: Seq[Identifiable] = ASTTranslator.convert(astRoot)

      // Stop execution after first error.
      // If everything went good return from last expression.
      program.foldLeft[Either[ExecutionError, Identifiable]](Right(NumericValue(0)))((acc, list) => {
        if (acc.isLeft) acc else evalWithPos(list)
      })
    } catch {
      case e: Throwable => Left(CriticalError(e))
    }
  }

  def evalWithPos(node: Identifiable): Either[ExecutionError, Identifiable] = eval(node) match {
    case Left(error) => Left(error.setPos(node.pos))
    case x => x
  }

  def eval(node: Identifiable): Either[ExecutionError, Identifiable] = node match {
    // For literals just return whatever it is
    case NumericValue(value) => Right(NumericValue(value))
    case StringValue(value) => Right(StringValue(value))
    case NilValue() => Right(NilValue())
    case BoolValue(value) => Right(BoolValue(value))

    // Looking up an identifier
    case IdentifierValue(value) => scopeManager.get(value) match {
      case None => Left(UnknownIdentifier(value))
      case Some(identifiable) => Right(identifiable)
    }

    // Evaluate whole vector
    case VectorValue(values) => evalSequence(values).map(elems => VectorValue(elems))

    // Backtick operator prevents evaluation
    case PrefixedValue(BacktickOperator(), value) => Right(value)


    case MapValue(map) =>
      val result: mutable.Map[Identifiable, Identifiable] = mutable.Map()
      for ((k, v) <- map) {
        evalWithPos(v) match {
          case Left(err) => return Left(err)
          case Right(value) => result += (k -> value)
        }
      }
      Right(MapValue(result))

    // Lists allow for function calling
    case ListValue(Seq()) => Right(ListValue(List()))
    case ListValue(ListValue(values) +: Seq()) => evalWithPos(ListValue(values)).flatMap(res => evalWithPos(res))
    case ListValue(ListValue(values) +: tail) => evalWithPos(ListValue(values)).flatMap(result => evalWithPos(ListValue(result +: tail)))

    // Function passed as first element
    case ListValue((f: Function) +: Seq()) => f.apply(List.empty, this)
    case ListValue((f: Function) +: tail) => f.apply(tail, this)

    // Identificator passed as first element
    case ListValue(IdentifierValue(value) +: tail) => scopeManager.get(value) match {
      case Some(identifiable) =>
        if (identifiable.isInstanceOf[Value]) Left(NotCallableError(IdentifierValue(value)))
        val function = identifiable.asInstanceOf[Function]
        function.apply(tail, this)
      case None => Left(UnknownIdentifier(value))
    }

    // Anything else inside list that wasn't matched
    case ListValue(value +: _) => Left(NotCallableError(value))

    // Function outside list expression is like literal
    case function: Function => Right(function)

    // Temporary solution to hide warnings
    case other => Left(NotEvaluable(other))
  }

  // Evaluates VectorLiteral
  def evalSequence(values: Seq[Identifiable]): Either[ExecutionError, Seq[Identifiable]] = {
    val vector = ListBuffer[Identifiable]()
    for (value <- values) {
      evalWithPos(value) match {
        case Right(v) => vector += v
        case Left(err) => return Left(err) // maybe we should try accumulating errors?
      }
    }
    Right(vector.toList)
  }
}
