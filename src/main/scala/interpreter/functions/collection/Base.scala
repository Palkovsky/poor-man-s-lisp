package interpreter.functions.collection

import interpreter.{ArgSet, BoolValue, CollectionValue, ExecutionError, Function, GenericError, Identifiable, ListValue, MapValue, NilValue, NumericValue, SequenceValue, TypeArg, Types, VectorValue}


class Cons extends Function {
  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.any), TypeArg(Types.sequence))

  def run(element: Identifiable, sequence: SequenceValue): Either[ExecutionError, Identifiable] = sequence match {
    case ListValue(values) => Right(ListValue(element +: values))
    case VectorValue(values) => Right(VectorValue(element +: values))
  }
}

class Conj extends Function {
  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.sequence), TypeArg(Types.any))

  def run(sequence: SequenceValue, element: Identifiable): Either[ExecutionError, Identifiable] = sequence match {
    case ListValue(values) => Right(ListValue(values :+ element))
    case VectorValue(values) => Right(VectorValue(values :+ element))
  }
}

class Head extends Function {
  override protected val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.sequence))

  def run(sequence: SequenceValue): Either[ExecutionError, Identifiable] = sequence match {
    case ListValue(Seq()) => Right(NilValue())
    case VectorValue(Seq()) => Right(NilValue())
    case ListValue(head +: _) => Right(head)
    case VectorValue(head +: _) => Right(head)
  }
}

class Tail extends Function {
  override protected val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.sequence))

  def run(sequence: SequenceValue): Either[ExecutionError, Identifiable] = sequence match {
    case ListValue(Seq()) => Right(NilValue())
    case VectorValue(Seq()) => Right(NilValue())
    case ListValue(_ +: tail) => Right(ListValue(tail))
    case VectorValue(_ +: tail) => Right(VectorValue(tail))
  }
}

class Init extends Function {
  override protected val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.sequence))

  def run(sequence: SequenceValue): Either[ExecutionError, Identifiable] = sequence match {
    case ListValue(Seq()) => Right(NilValue())
    case VectorValue(Seq()) => Right(NilValue())
    case ListValue(values) => Right(ListValue(values.init))
    case VectorValue(values) => Right(VectorValue(values.init))
  }
}

class Take extends Function {
  override protected val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.numeric), TypeArg(Types.sequence))

  def run(numeric: NumericValue, sequence: SequenceValue): Either[ExecutionError, Identifiable] = sequence match {
    case lazyGenerator: LazyGenerator =>
      var taken: Seq[Identifiable] = Seq()
      for (_ <- 1 to numeric.asInt()) {
        lazyGenerator.next() match {
          case Left(err) => return Left(err)
          case Right(identifiable) => taken = taken :+ identifiable
        }
      }
      Right(lazyGenerator.sequence().wrap(taken))
    case ListValue(values) => Right(ListValue(values.take(numeric.asInt())))
    case VectorValue(values) => Right(VectorValue(values.take(numeric.asInt())))
  }
}

class Drop extends Function {
  override protected val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.numeric), TypeArg(Types.sequence))

  def run(numeric: NumericValue, sequence: SequenceValue): Either[ExecutionError, Identifiable] = sequence match {
    case lazyGenerator: LazyGenerator =>
      for (_ <- 1 to numeric.asInt()) {
        lazyGenerator.next() match {
          case Left(err) => return Left(err)
          case _ =>
        }
      }
      Right(lazyGenerator)
    case ListValue(values) => Right(ListValue(values.drop(numeric.asInt())))
    case VectorValue(values) => Right(VectorValue(values.drop(numeric.asInt())))
  }
}

class Concat extends Function {
  override protected val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.sequence), TypeArg(Types.sequence))

  def run(first: SequenceValue, second: SequenceValue): Either[ExecutionError, Identifiable] = (first, second) match {
    case (ListValue(f), ListValue(s)) => Right(ListValue(f ++ s))
    case (ListValue(f), VectorValue(s)) => Right(ListValue(f ++ s))
    case (VectorValue(f), ListValue(s)) => Right(VectorValue(f ++ s))
    case (VectorValue(f), VectorValue(s)) => Right(VectorValue(f ++ s))
    case (_, _) => Left(GenericError("Unable to concat.")) //it won't be matched
  }
}

class Empty extends Function {
  override protected val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.collection))

  def run(collection: CollectionValue): Either[ExecutionError, Identifiable] = Right(BoolValue(collection match {
    case seq: SequenceValue => seq.values.isEmpty
    case MapValue(map) => map.isEmpty
    case _ => false
  }))
}

class ToList extends Function {
  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.sequence))

  def run(sequence: SequenceValue): Either[ExecutionError, Identifiable] = Right(ListValue(sequence.values))
}

class ToVector extends Function {
  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.sequence))

  def run(sequence: SequenceValue): Either[ExecutionError, Identifiable] = Right(VectorValue(sequence.values))
}