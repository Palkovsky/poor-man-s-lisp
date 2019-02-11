package interpreter.functions.collection

import interpreter.{ArgSet, CollectionValue, ExecutionError, Function, Identifiable, ListValue, MapValue, TypeArg, Types, VectorValue}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Map extends Function {
  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.function), TypeArg(Types.collection))

  def run(function: Function, collection: CollectionValue): Either[ExecutionError, Identifiable] = collection match {
      case ListValue(elements) => mapSequence(function, elements).map(mapped => ListValue(mapped))
      case VectorValue(elements) => mapSequence(function, elements).map(mapped => VectorValue(mapped))
      case MapValue(map) => mapMap(function, map)
  }

  private def mapMap(f: Function, map: mutable.Map[Identifiable, Identifiable]): Either[ExecutionError, MapValue] = {
    val result: mutable.Map[Identifiable, Identifiable] = mutable.Map()
    for((k, v) <- map){
      f.apply(List(v), getExecutor) match {
        case Left(err) => return Left(err)
        case Right(value) => result += (k->value)
      }
    }
    Right(MapValue(result))
  }

  private def mapSequence(f: Function, sequence: Seq[Identifiable]): Either[ExecutionError, Seq[Identifiable]] = {
    val result: ListBuffer[Identifiable] = ListBuffer()
    for (elem <- sequence) {
      f.apply(List(elem), getExecutor) match {
        case Left(err) => return Left(err)
        case Right(value) => result += value
      }
    }
    Right(result)
  }
}
