package interpreter

import scala.collection.mutable

object Default {
  def scope: Scope = Scope(mutable.Map(
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
    "concat" -> new interpreter.functions.collection.Concat(),
    "head" -> new interpreter.functions.collection.Head(),
    "tail" -> new interpreter.functions.collection.Tail(),
    "init" -> new interpreter.functions.collection.Init(),
    "take" -> new interpreter.functions.collection.Take(),
    "drop" -> new interpreter.functions.collection.Drop(),
    "empty?" -> new interpreter.functions.collection.Empty(),
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
  ))
}
