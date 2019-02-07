package interpreter

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

// ADT representing state in scope
case class Scope(identifiers: mutable.Map[String, Identifiable])

class ScopeManager(base: Scope) {
  val scopeStack: ListBuffer[Scope] = new ListBuffer[Scope]
  scopeStack += base

  def current: Scope = scopeStack.last

  /*
  def enter(scope: Scope): Unit = {
    scopeStack += current.copy(identifiers = current.identifiers.filterKeys(k => scope.identifiers.keySet.contains(k)))
  }

  def leave(): Scope = scopeStack.remove(scopeStack.size - 1)
*/

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

object ScopeManager {
  def default() = new ScopeManager(Scope(mutable.Map(
    "+" -> new interpreter.functions.math.Plus(),
    "-" -> new interpreter.functions.math.Minus(),
    "*" -> new interpreter.functions.math.Mult(),
    "/" -> new interpreter.functions.math.Div(),
    "%" -> new interpreter.functions.math.Mod(),

    "func" -> new interpreter.functions.core.Def(),
    "apply" -> new interpreter.functions.core.Apply(),
    "cons" ->  new interpreter.functions.collection.Cons(),
    "id" -> new interpreter.functions.core.Id(),
    "map" -> new interpreter.functions.collection.Map()
  )))

  def apply(base: Scope) = new ScopeManager(base)
}