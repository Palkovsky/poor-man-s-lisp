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

case class AltArg(options: Seq[Arg]) extends Arg {
  override def matching(identifiable: Identifiable): Boolean = options.foldLeft(false)((acc, arg) => acc || arg.matching(identifiable))
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
    val argTypes = types.map{
      case TypeArg(t) => t
      case EqualArg(identifiable) => identifiable.getClass
      case AltArg(_) => Types.any
      case CollapsedArg() => Types.any
    }

    klass.getMethod(name, argTypes: _*)
  }
}

object ArgSet {
  def single(args: Arg*) = List(ArgSet(args))
}