package interpreter.functions.collection

import interpreter.{ArgSet, BoolValue, CollectionValue, ExecutionError, Function, Identifiable, ListValue, MapValue, NilValue, SequenceValue, TypeArg, Types, VectorValue}


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



class Empty extends Function {
  override protected val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.collection))

  def run(collection: CollectionValue): Either[ExecutionError, Identifiable] = Right(BoolValue(collection match {
    case ListValue(Seq()) => true
    case VectorValue(Seq()) => true
    case MapValue(map) => map.isEmpty
    case _ => false
  }))
}

class ToList extends Function {
  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.sequence))

  def run(sequence: SequenceValue): Either[ExecutionError, Identifiable] = sequence match {
    case ListValue(values) => Right(ListValue(values))
    case VectorValue(values) => Right(ListValue(values))
  }
}

class ToVector extends Function {
  override val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.sequence))

  def run(sequence: SequenceValue): Either[ExecutionError, Identifiable] = sequence match {
    case ListValue(values) => Right(VectorValue(values))
    case VectorValue(values) => Right(VectorValue(values))
  }
}