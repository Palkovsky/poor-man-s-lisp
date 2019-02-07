package interpreter.functions.core

import interpreter.{ExecutionError, Executor, Function, Identifiable, IdentifierValue, ListValue, Types, VectorValue}

class UserDefinedFunction(override val arity: Int, override val types: Seq[Class[_]], expr: ListValue, params: Seq[IdentifierValue]) extends Function {

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    for((identifier, idx) <- params.view.zipWithIndex){ //Bounding parameters with identifiers
      executor.scopeManager.put(identifier.value, args(idx))
    }
    val result = executor.eval(expr)
    params.foreach(identifier => executor.scopeManager.remove(identifier.value)) //Dropping bounded params
    result
  }

}


class Def extends Function {
  override val types: Seq[Class[_]] = List(Types.identifier, Types.vector, Types.list)

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    val name = Types.getAs[IdentifierValue](args, 0).value
    val params = Types.getAs[VectorValue](args, 1).value
    val body = Types.getAs[ListValue](args, idx = 2)
    Types.validate(params, List.fill(params.length)(Types.identifier))
      .flatMap(_ => {
        val fParamNames: Seq[IdentifierValue] = params.map(x => x.asInstanceOf[IdentifierValue])
        val fArity: Int = params.length
        val fTypes: Seq[Class[_]] = List.fill(fArity)(Types.identifier)
        val function: Function = new UserDefinedFunction(fArity, fTypes, body, fParamNames)
        executor.scopeManager.put(name, function)
        Right(function)
      })
  }
}
