package interpreter

import java.lang.reflect.Method

sealed trait Arg {
  def matching(identifiable: Identifiable): Boolean
}

case class TypeArg(t: Class[_]) extends Arg {
  override def matching(arg: Identifiable): Boolean = t.isAssignableFrom(arg.getClass)
}

case class EqualArg(identifiable: Identifiable) extends Arg {
  override def matching(arg: Identifiable): Boolean = identifiable.equals(arg)
}

case class CollapsedArg() extends Arg {
  override def matching(identifiable: Identifiable): Boolean = true
}

case class ArgSet(types: Seq[Arg]) {
  def matching(args: Seq[Identifiable]): Boolean = {
    for ((arg, idx) <- args.zipWithIndex) {
      if (idx >= types.length && types.nonEmpty && types.last.isInstanceOf[CollapsedArg]) return true
      if (!types(idx).matching(arg)) return false
    }
    true
  }

  def findMethod(klass: Class[_], name: String): Method = {
    val argTypes = types.filter(arg => arg.isInstanceOf[TypeArg]).map(arg => arg.asInstanceOf[TypeArg].t)
    klass.getMethod(name, argTypes: _*)
  }
}

object ArgSet {
  def single(args: Arg*) = List(ArgSet(args))
}