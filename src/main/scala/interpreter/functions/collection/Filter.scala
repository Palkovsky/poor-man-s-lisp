package interpreter.functions.collection

import interpreter.{CollectionValue, ExecutionError, Executor, Function, Identifiable, ListValue, MapValue, Types, VectorValue}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Filter extends Function {
  override val argTypes: Seq[Class[_]] = List(Types.function, Types.collection)

  override protected def run(args: Seq[Identifiable], executor: Executor): Either[ExecutionError, Identifiable] = {
    val function = Types.getAs[Function](args, 0)
    Types.getAs[CollectionValue](args, 1) match {
      case ListValue(elements) => filterSequence(function, elements, executor).map(filtered => ListValue(filtered))
      case VectorValue(elements) => filterSequence(function, elements, executor).map(filtered => VectorValue(filtered))
      case MapValue(map) => filterMap(function, map, executor)
    }
  }

  private def filterMap(f: Function, map: mutable.Map[Identifiable, Identifiable], executor: Executor): Either[ExecutionError, MapValue] = {
    val result: mutable.Map[Identifiable, Identifiable] = mutable.Map()
    for ((k, v) <- map) {
      f.apply(List(v), executor) match {
        case Left(err) => return Left(err)
        case Right(value) => if (Types.asBoolean(value)) result += (k -> v)
      }
    }
    Right(MapValue(result))
  }

  private def filterSequence(f: Function, sequence: Seq[Identifiable], executor: Executor): Either[ExecutionError, Seq[Identifiable]] = {
    val result: ListBuffer[Identifiable] = ListBuffer()
    for (elem <- sequence) {
      f.apply(List(elem), executor) match {
        case Left(err) => return Left(err)
        case Right(value) => if (Types.asBoolean(value)) result += elem
      }
    }
    Right(result)
  }
}
