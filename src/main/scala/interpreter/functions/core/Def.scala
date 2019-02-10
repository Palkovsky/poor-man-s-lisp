package interpreter.functions.core

import interpreter.{ExecutionError, Executor, Function, Identifiable, IdentifierValue, ListValue, PrefixedValue, ScopeBuilder, Types, VectorValue}
import parser.AmpersandOperator

class Def extends Function {
  override val argTypes: Seq[Class[_]] = List(Types.identifier, Types.any)

  override def evaluatedArgs: Boolean = false

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    val name = Types.getAs[IdentifierValue](args, 0).value
    val storable = Types.getAs[Identifiable](args, 1)
    executor.scopeManager.put(name, storable)
    Right(storable)
  }
}

class DefFn extends Function {
  override val argTypes: Seq[Class[_]] = List(Types.identifier, Types.vector, Types.list)

  override def evaluatedArgs: Boolean = false

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    val name = Types.getAs[IdentifierValue](args, 0).value
    val params = Types.getAs[VectorValue](args, 1)
    val body = Types.getAs[ListValue](args, idx = 2)
    new Lambda().apply(List(params, body), executor).flatMap(function => new Def().apply(List(IdentifierValue(name), function), executor))
  }
}

class UserDefinedFunction(override val argTypes: Seq[Class[_]], expr: ListValue, params: Seq[IdentifierValue], collapsedArgs: Boolean) extends Function {

  override protected def checkArity: Boolean = !collapsedArgs

  override protected def checkTypes: Boolean = false

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {

    var finalArgs = args.slice(0, params.length)
    if (collapsedArgs) {
      val collapsed: VectorValue = VectorValue(args.slice(params.length - 1, args.length))
      finalArgs = finalArgs.updated(finalArgs.length - 1, collapsed)
      println(finalArgs)
    }


    val newScope = params.view.zipWithIndex.foldLeft[ScopeBuilder](ScopeBuilder()) { case (acc, (identifier, idx)) => acc.put(identifier.value, finalArgs(idx)) }.build()
    executor.scopeManager.enter(newScope)
    val result = executor.eval(expr)
    executor.scopeManager.leave()
    result
  }

}

class Lambda extends Function {
  override val argTypes: Seq[Class[_]] = List(Types.vector, Types.list)

  override def evaluatedArgs: Boolean = false

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    var params = Types.getAs[VectorValue](args, 0).value
    val body = Types.getAs[ListValue](args, idx = 1)

    /*
      Collapsed args example:
      (defn sum [&args] (reduce + 0 args)) (sum 1 2 3) ==> 6
     */
    val collapsedArgs = params.last match {
      case PrefixedValue(AmpersandOperator(), IdentifierValue(value)) =>
        params = params.updated(params.length - 1, IdentifierValue(value))
        true
      case _ => false
    }

    Types.validate(params, List.fill(params.length)(Types.identifier), checkArity = !collapsedArgs)
      .flatMap(_ => {
        val fParamNames: Seq[IdentifierValue] = params.map(x => x.asInstanceOf[IdentifierValue])
        val fTypes: Seq[Class[_]] = List.fill(params.length)(Types.identifier)
        Right(new UserDefinedFunction(fTypes, body, fParamNames, collapsedArgs))
      })
  }
}