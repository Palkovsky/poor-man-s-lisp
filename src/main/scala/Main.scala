import interpreter.Executor
import parser.{LispParser, RootExpression}

object Main extends App {
  val parser = new LispParser

  val code1 = "(func `sub `[x y] `(- x y)) (func `rev `[f x y] `(f y x)) (rev sub 5 1)"
  val code2 = "(func `sub `[x y] `(- x y)) (apply sub `(1 2))"
  val code3 = "(func `addTwo `[x] `(+ x 2)) (map addTwo {x 1 y 2 z nil})"
  val code4 = "(func `even `[x] `(eq 0 (mod x 2))) (filter even {a 1 b 2 c 3 d 4 e 5})"
  val code5 = "(reduce conj [] `(1 2 3))"

  parser.parseLisp(code5) match {
    case parser.Success(root: RootExpression, _) =>
      val executor: Executor = new Executor(root)
      println(root)
      println(executor.run())
    case parser.Failure(msg, _) => println(msg)
    case parser.Error(msg, _) => println(msg)
  }
}
