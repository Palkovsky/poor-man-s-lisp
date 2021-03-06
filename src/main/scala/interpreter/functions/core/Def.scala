package interpreter.functions.core

import interpreter.{ArgSet, CollapsedArg, ExecutionError, Function, GenericError, Identifiable, IdentifierValue, ListValue, PrefixedValue, ScopeBuilder, TypeArg, Types, VectorValue}
import parser.{AmpersandOperator, BacktickOperator}

class Def extends Function {

  override def evaluatedArgs: Boolean = false

  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.identifier), TypeArg(Types.any))

  def run(identifier: IdentifierValue, storable: Identifiable): Either[ExecutionError, Identifiable] = {
    val name = identifier.value
    getExecutor.evalWithPos(storable).flatMap(evaluated => {
      getExecutor.scopeManager.put(name, evaluated)
      Right(evaluated)
    })
  }
}

class DefFn extends Function {
  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.identifier), TypeArg(Types.vector), TypeArg(Types.list))

  override def evaluatedArgs: Boolean = false

  def run(identifier: IdentifierValue, params: VectorValue, body: ListValue): Either[ExecutionError, Identifiable] = {
    val name = identifier.value
    new Lambda().apply(List(params, body), getExecutor).flatMap(function => new Def().apply(List(IdentifierValue(name), PrefixedValue(BacktickOperator(), function)), getExecutor))
  }
}

class UserDefinedFunction(override val argSets: Seq[ArgSet], expr: ListValue, params: Seq[IdentifierValue], collapsedArgs: Boolean) extends Function {

  override def run(args: Seq[Identifiable]): Either[ExecutionError, Identifiable] = {
    var finalArgs = args.slice(0, params.length)
    if (collapsedArgs) {
      val collapsed: VectorValue = VectorValue(args.slice(params.length - 1, args.length))
      finalArgs = finalArgs.updated(finalArgs.length - 1, collapsed)
    }

    val paramsBindings = params.view.zipWithIndex.foldLeft[ScopeBuilder](ScopeBuilder()) { case (acc, (identifier, idx)) => acc.put(identifier.value, finalArgs(idx)) }.build()
    val newScope = getContext.join(paramsBindings)
    getExecutor.scopeManager.enter(newScope)
    val result = getExecutor.evalWithPos(expr)
    getExecutor.scopeManager.leave()
    result.map(identifiable => identifiable.setContext(newScope))
  }
}

class Lambda extends Function {

  override def evaluatedArgs: Boolean = false

  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.vector), TypeArg(Types.list))

  def run(identifiersVector: VectorValue, body: ListValue): Either[ExecutionError, Identifiable] = {
    var params = identifiersVector.value
    val collapsedArgs = params.lastOption match {
      case Some(PrefixedValue(AmpersandOperator(), IdentifierValue(value))) =>
        params = params.updated(params.length - 1, IdentifierValue(value))
        true
      case _ => false
    }

    val testingArgSet = ArgSet(List.fill(params.length)(TypeArg(Types.identifier)))
    if (!testingArgSet.matching(params)) Left(GenericError("Expecting identifiers in function arguments vector."))

    val fParamNames = params.map(x => x.asInstanceOf[IdentifierValue])
    var fTypes = ArgSet(List.fill(params.length)(TypeArg(Types.any)))
    if (collapsedArgs) fTypes = ArgSet(fTypes.types.updated(fTypes.types.length - 1, CollapsedArg()))
    Right(new UserDefinedFunction(List(fTypes), body, fParamNames, collapsedArgs))
  }
}