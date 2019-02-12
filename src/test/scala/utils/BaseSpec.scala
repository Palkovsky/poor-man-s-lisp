package utils

import interpreter.{Executor, Identifiable}
import org.scalatest.{FunSpec, Matchers}
import parser.{LispParser, RootExpression}

class BaseSpec extends FunSpec with Matchers {

  def parse(code: String): RootExpression = LispParser.parseCode(code) match {
    case LispParser.Success(root: RootExpression, _) => root
    case LispParser.Failure(msg, _) => fail(msg)
    case LispParser.Error(msg, _) => fail(msg)
  }

  def exec(code: String): Identifiable = new Executor().run(parse(code)) match {
    case Right(identifiable) => identifiable
    case Left(err) => fail(err.toString)
  }
}
