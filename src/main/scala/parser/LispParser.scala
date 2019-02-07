package parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

trait ParserUtils extends RegexParsers {
  def between[T](start: Parser[Any], end: Parser[Any], p: Parser[T]): Parser[T] = start ~> p <~ end
}

class LispParser extends RegexParsers with ParserUtils {

  override protected val whiteSpace: Regex = "\\s".r
  override def skipWhitespace: Boolean = true


  // Matches digits sequence
  private def integer: Parser[IntegerLiteral] = "[0-9]+".r ^^ { str => IntegerLiteral(str.toInt) }

  // Matches number followed with a dot and at least one digit
  // Could be rewriten as integer ~ "[.]".r ~ integer ^^ {case a ~ _ ~ b => Floatingliteral("$a.$b".toDouble)}
  private def floating: Parser[FloatingLiteral] = "[0-9]+[.][0-9]+".r ^^ { str => FloatingLiteral(str.toDouble) }

  // Will match an sequence contained inside parenthesis
  private def string: Parser[StringLiteral] = "[\"]{1}.*?[\"]{1}".r ^^ { str => StringLiteral(str.substring(1, str.length - 1)) }

  // Will match any identifier-like sequence
  private def identifier: Parser[IdentifierLiteral] = "[_=!\\-\\+\\?\\*a-zA-Z]{1}[_=!\\-\\+\\?\\*a-zA-Z0-9]*".r ^^ { str => IdentifierLiteral(str) }

  // Will match list of expressions inside ( )
  private def list: Parser[ListExpression] = between("(", ")", rep[ASTNode](expression)) ^^ { expressions => ListExpression(expressions) }

  // Same as list, but for [ ]
  private def vector: Parser[VectorLiteral] = between("[", "]", rep[ASTNode](expression)) ^^ { expressions => VectorLiteral(expressions) }

  // Will match key/value pairs contained inside { }
  private def hashmap: Parser[HashMapLiteral] = {
    def keyValuePair: Parser[(ASTLiteral, ASTNode)] = hashable ~ expression ^^ {case key ~ value => (key, value)}
    between("{", "}",  rep[(ASTLiteral, ASTNode)](keyValuePair)) ^^ {pairs => HashMapLiteral(pairs.toMap)}
  }

  private def bool: Parser[BoolLiteral] = ("true".r | "false".r) ^^ {value => BoolLiteral(value.equals("true"))}

  // Nil value
  private def nil: Parser[NilLiteral] = "nil".r ^^ {_ => NilLiteral()}

  // Allowed prefix operators
  private def prefixOperators: Parser[String] = "~" | "`" | "."

  // Will match any prefixed expression with allowed
  private def prefixed: Parser[PrefixedExpression] = prefixOperators ~ expression ^^ { case pref ~ expr =>
    pref match {
      case "~" => PrefixedExpression(TildaOperator(), expr)
      case "`" => PrefixedExpression(BacktickOperator(), expr)
      case "." => PrefixedExpression(DotOperator(), expr)
    }
  }

  // Matches all literals
  private def literal: Parser[ASTLiteral] = positioned(floating | integer | nil | bool | identifier | string | vector | hashmap)

  // Matches all hashable epxressions
  private def hashable: Parser[ASTLiteral] = positioned(floating | integer | string | identifier)

  // Matches every type of expression
  private def expression: Parser[ASTNode] = positioned(list | literal | prefixed)

  // Matches lisp source code
  private def lisp: Parser[RootExpression] = rep1[ListExpression](list) ^^ { nodes => RootExpression(nodes) }

  def parseLisp(code: String):ParseResult[RootExpression] = parse(lisp, code)
}
