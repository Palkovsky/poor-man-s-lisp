package interpreter.functions.core

import interpreter.{BoolValue, ExecutionError, Executor, Function, Identifiable, Types}

abstract class TypeCheck(val expected: Class[_]) extends Function {
  override val argTypes: Seq[Class[_]] = List(Types.any)

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    val matching = expected.isAssignableFrom(Types.getAs(args, 0).getClass)
    Right(BoolValue(matching))
  }
}

class IsNil extends TypeCheck(Types.nil)
class IsString extends TypeCheck(Types.string)
class IsSequence extends TypeCheck(Types.sequence)
class IsCollection extends TypeCheck(Types.collection)
class IsMap extends TypeCheck(Types.map)
class IsVector extends TypeCheck(Types.vector)
class IsList extends TypeCheck(Types.list)
class IsNumeric extends TypeCheck(Types.numeric)
class IsInt extends TypeCheck(Types.int)
class IsFloating extends TypeCheck(Types.floating)
class IsBool extends TypeCheck(Types.bool)
class IsIdentifier extends TypeCheck(Types.identifier)
class IsFunction extends TypeCheck(Types.function)