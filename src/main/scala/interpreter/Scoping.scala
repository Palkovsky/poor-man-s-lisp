package interpreter

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

// ADT representing state in scope
case class Scope(identifiers: mutable.Map[String, Identifiable])

class ScopeManager(base: Scope) {
  val scopeStack: ListBuffer[Scope] = new ListBuffer[Scope]
  scopeStack += base

  def current: Scope = scopeStack.last

  def enter(scope: Scope): Unit = {
    scopeStack += Scope(current.identifiers ++ scope.identifiers)
  }

  def leave(): Scope = scopeStack.remove(scopeStack.size - 1)


  def get(key: String): Option[Identifiable] = current.identifiers.get(key)


  def getConcrete(key: String): Option[Identifiable] = get(key) match {
    case None => None
    case Some(IdentifierValue(value)) => getConcrete(value)
    case Some(concrete) => Some(concrete)
  }

  def put(key: String, value: Identifiable): Identifiable = {
    current.identifiers += (key -> value)
    value
  }

  def remove(key: String): String = {
    current.identifiers -= key
    key
  }
}

class ScopeBuilder {
  private val identifiers: mutable.Map[String, Identifiable] = mutable.Map()

  def put(key: String, value: Identifiable): ScopeBuilder = {
    identifiers += (key -> value)
    this
  }

  def build(): Scope = Scope(identifiers)
}

object ScopeBuilder {
  def apply(): ScopeBuilder = new ScopeBuilder()
}

object ScopeManager {
  def default() = new ScopeManager(Scope(mutable.Map(
    "+" -> new interpreter.functions.math.Plus(),
    "-" -> new interpreter.functions.math.Minus(),
    "*" -> new interpreter.functions.math.Mult(),
    "div" -> new interpreter.functions.math.Div(),
    "mod" -> new interpreter.functions.math.Mod(),
    "and" -> new interpreter.functions.math.And(),
    "or" -> new interpreter.functions.math.Or(),
    "not" -> new interpreter.functions.math.Not(),

    "eq" -> new interpreter.functions.core.Eq(),
    "gt" -> new interpreter.functions.math.Greater(),
    "gte" -> new interpreter.functions.math.GreaterOrEqual(),
    "lt" -> new interpreter.functions.math.Lesser(),
    "lte" -> new interpreter.functions.math.LesserOrEqual(),

    "def" -> new interpreter.functions.core.Def(),
    "defn" -> new interpreter.functions.core.DefFn(),
    "fn" -> new interpreter.functions.core.Lambda(),
    "apply" -> new interpreter.functions.core.Apply(),
    "if" -> new interpreter.functions.core.If(),

    "cons" -> new interpreter.functions.collection.Cons(),
    "conj" -> new interpreter.functions.collection.Conj(),
    "asList" -> new interpreter.functions.collection.ToList(),
    "asVector" -> new interpreter.functions.collection.ToVector(),

    "id" -> new interpreter.functions.core.Id(),

    "map" -> new interpreter.functions.collection.Map(),
    "filter" -> new interpreter.functions.collection.Filter(),
    "reduce" -> new interpreter.functions.collection.Reduce(),

    "nothing?" -> new interpreter.functions.core.IsNil(),
    "str?" -> new interpreter.functions.core.IsString(),
    "coll?" -> new interpreter.functions.core.IsCollection(),
    "seq?" -> new interpreter.functions.core.IsSequence(),
    "vector?" -> new interpreter.functions.core.IsVector(),
    "list?" -> new interpreter.functions.core.IsList(),
    "map?" -> new interpreter.functions.core.IsMap(),
    "num?" -> new interpreter.functions.core.IsNumeric(),
    "int?" -> new interpreter.functions.core.IsInt(),
    "float?" -> new interpreter.functions.core.IsFloating(),
    "func?" -> new interpreter.functions.core.IsFunction(),
    "bool?" -> new interpreter.functions.core.IsBool(),
    "identifier?" -> new interpreter.functions.core.IsIdentifier()
  )))

  def apply(base: Scope) = new ScopeManager(base)
}

