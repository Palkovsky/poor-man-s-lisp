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
  def default() = new ScopeManager(Default.scope)

  def apply(base: Scope) = new ScopeManager(base)
}

