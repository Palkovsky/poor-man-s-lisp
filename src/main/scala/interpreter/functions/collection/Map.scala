package interpreter.functions.collection

import interpreter.{CollectionValue, ExecutionError, Executor, Function, Identifiable, ListValue, MapValue, Types, VectorValue}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Map extends Function {
  override val argTypes: Seq[Class[_]] = List(Types.function, Types.collection)

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    val function = Types.getAs[Function](args, 0)
    Types.getAs[CollectionValue](args, 1) match {
      case ListValue(elements) => mapSequence(function, elements, executor).map(mapped => ListValue(mapped))
      case VectorValue(elements) => mapSequence(function, elements, executor).map(mapped => VectorValue(mapped))
      case MapValue(map) => mapMap(function, map, executor)
    }
  }

  private def mapMap(f: Function, map: mutable.Map[Identifiable, Identifiable], executor: Executor): Either[ExecutionError, MapValue] = {
    val result: mutable.Map[Identifiable, Identifiable] = mutable.Map()
    for((k, v) <- map){
      f.apply(List(v), executor) match {
        case Left(err) => return Left(err)
        case Right(value) => result += (k->value)
      }
    }
    Right(MapValue(result))
  }

  private def mapSequence(f: Function, sequence: Seq[Identifiable], executor: Executor): Either[ExecutionError, Seq[Identifiable]] = {
    val result: ListBuffer[Identifiable] = ListBuffer()
    for (elem <- sequence) {
      f.apply(List(elem), executor) match {
        case Left(err) => return Left(err)
        case Right(value) => result += value
      }
    }
    Right(result)
  }
}
