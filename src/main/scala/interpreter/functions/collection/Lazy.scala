package interpreter.functions.collection

import interpreter.{ArgSet, ExecutionError, Executor, Function, GenericError, Identifiable, IdentifierValue, ListValue, PrefixedValue, SequenceValue, TypeArg, Types, VectorValue}
import parser.BacktickOperator

case class LazyGenerator(_sequence: SequenceValue, _generator: Identifiable, _executor: Executor) extends SequenceValue {

  private var currentSequence: SequenceValue = _sequence
  private var currentGenerator: Identifiable = _generator
  private var baseScope = _executor.scopeManager.current

  def next(): Either[ExecutionError, Identifiable] = {
    _executor.scopeManager.enter(baseScope)
    val generated = _executor.evalWithPos(currentGenerator)

    generated match {
      case Right(g) => baseScope = baseScope.join(g.getContext)
      case _ => ;
    }

    val res = generated match {
      case Right(ListValue(IdentifierValue("cons") +: (element: Identifiable) +: Seq(nextGenerator: Identifiable))) =>
        _executor.evalWithPos(element).map(evaluated => {
          currentSequence = currentSequence.append(element)
          currentGenerator = nextGenerator
          evaluated
        })

      case Right(generator: LazyGenerator) =>
        generator.next().map(identifiable => {
          currentSequence = currentSequence.append(identifiable)
          currentGenerator = generator.generator()
          identifiable
        })

      case other =>
        println(other)
        Left(GenericError("Unable to yield next item.")) //Dunno if I should return here nil or throw error.
    }

    _executor.scopeManager.leave()
    res
  }

  def sequence(): SequenceValue = currentSequence

  def generator(): Identifiable = currentGenerator

  override def values: Seq[Identifiable] = currentSequence.values

  override def append(x: Identifiable): SequenceValue = LazyGenerator(sequence().append(x), generator(), _executor)

  override def concat(arr: SequenceValue): SequenceValue = LazyGenerator(sequence().concat(arr), generator(), _executor)

  override def wrap(arr: Seq[Identifiable]): SequenceValue = LazyGenerator(sequence().wrap(arr), generator(), _executor)
}

class LazySeq extends Function {

  override protected def evaluatedArgs: Boolean = false

  override protected val argSets: Seq[ArgSet] = ArgSet.single(TypeArg(Types.any))

  def run(identifiable: Identifiable): Either[ExecutionError, Identifiable] = {
    Right(LazyGenerator(VectorValue(List()), PrefixedValue(BacktickOperator(), identifiable), getExecutor))
  }
}
