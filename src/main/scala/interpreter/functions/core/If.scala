package interpreter.functions.core

import interpreter.{ArgSet, EqualArg, ExecutionError, Function, Identifiable, IdentifierValue, TypeArg, Types}

class If extends Function {

  override protected def evaluatedArgs: Boolean = false

  override protected val argSets: Seq[ArgSet] = ArgSet.single(
    TypeArg(Types.any),
    EqualArg(IdentifierValue("then")),
    TypeArg(Types.any),
    EqualArg(IdentifierValue("else")),
    TypeArg(Types.any)
  )

  def run(condition: Identifiable, t: IdentifierValue, a: Identifiable, e: IdentifierValue, b: Identifiable): Either[ExecutionError, Identifiable] = {
    getExecutor.evalWithPos(condition).flatMap(res => getExecutor.evalWithPos(if (Types.asBoolean(res)) a else b))
  }
}
