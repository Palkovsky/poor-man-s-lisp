import interpreter.Executor
import parser.{LispParser, RootExpression}

import scala.io.StdIn

object Main extends App {


  override def main(args: Array[String]): Unit = {
    val executor = new Executor()
    while (true) {
      print("(poor-man-lisp)>> ")
      Console.flush()

      val line = StdIn.readLine()
      if (line.equals("q")) return
      run(line, executor)
      Console.flush()
    }
  }

  private def run(code: String, executor: Executor): Unit = LispParser.parseCode(code) match {
    case LispParser.Success(root: RootExpression, _) =>
      executor.run(root) match {
        case Right(result) => println(result)
        case Left(err) => println("ERROR: " + err)
      }
    case LispParser.Failure(msg, _) => println("PARSING ERROR: " + msg)
    case LispParser.Error(msg, _) => println("PARSING ERROR: " + msg)
  }
}
