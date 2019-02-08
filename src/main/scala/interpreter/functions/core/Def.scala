package interpreter.functions.core

import interpreter.{ExecutionError, Executor, Function, Identifiable, IdentifierValue, ListValue, Types, VectorValue}

class UserDefinedFunction(override val argTypes: Seq[Class[_]], expr: ListValue, params: Seq[IdentifierValue]) extends Function {

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    for ((identifier, idx) <- params.view.zipWithIndex) { //Bounding parameters with identifiers
      executor.scopeManager.put(identifier.value, args(idx))
    }
    val result = executor.eval(expr)
    params.foreach(identifier => executor.scopeManager.remove(identifier.value)) //Dropping bounded params
    result
  }

}

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

class Lambda extends Function {
  override val argTypes: Seq[Class[_]] = List(Types.vector, Types.list)

  override def evaluatedArgs: Boolean = false

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    val params = Types.getAs[VectorValue](args, 0).value
    val body = Types.getAs[ListValue](args, idx = 1)
    Types.validate(params, List.fill(params.length)(Types.identifier))
      .flatMap(_ => {
        val fParamNames: Seq[IdentifierValue] = params.map(x => x.asInstanceOf[IdentifierValue])
        val fTypes: Seq[Class[_]] = List.fill(params.length)(Types.identifier)
        Right(new UserDefinedFunction(fTypes, body, fParamNames))
      })
  }
}