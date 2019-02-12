package interpreter.functions.core

import interpreter.{ArgSet, ExecutionError, Function, GenericError, Identifiable, IdentifierValue, MapValue, TypeArg, Types}

import scala.collection.mutable

class Let extends Function {

  override protected def evaluatedArgs: Boolean = false

  override protected val argSets: Seq[ArgSet] = ArgSet.single(
    TypeArg(Types.map),
    TypeArg(Types.any)
  )

  def run(bindings: MapValue, exprs: Identifiable): Either[ExecutionError, Identifiable] = {
    val testingArgSet = ArgSet(List.fill(bindings.map.size)(TypeArg(Types.identifier)))
    val keyList = bindings.map.keys.toList
    if (!testingArgSet.matching(keyList)) Left(GenericError("Expecting identifiers as let bindings key."))

    val evaluatedBindings = mutable.Map[String, Identifiable]()
    for ((key, value) <- bindings.map) {
      getExecutor.evalWithPos(value) match {
        case Left(err) => return Left(err)
        case Right(res) => evaluatedBindings += (key.asInstanceOf[IdentifierValue].value -> res)
      }
    }

    val newScope = interpreter.Scope(evaluatedBindings)
    getExecutor.scopeManager.enter(newScope)
    val result = getExecutor.evalWithPos(exprs)
    getExecutor.scopeManager.leave()
    result
  }
}
