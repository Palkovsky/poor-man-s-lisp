import interpreter.Executor
import parser.{LispParser, RootExpression}

object Main extends App {
  val parser = new LispParser

  val code1 = "(func `sub `[x y] `(- x y)) (func `rev `[f x y] `(f y x)) (rev sub 5 1)"
  val code2 = "(func `sub `[x y] `(- x y)) (apply sub `(1 2))"
  val code3 = "(func `addTwo `[x] `(+ x 2)) (map addTwo {x 1 y 2 z nil})"

  parser.parseLisp(code3) match {
    case parser.Success(root: RootExpression, _) =>
      val executor: Executor = new Executor(root)
      println(root)
      println(executor.run())
    case parser.Failure(msg, _) => println(msg)
    case parser.Error(msg, _) => println(msg)
  }
}
