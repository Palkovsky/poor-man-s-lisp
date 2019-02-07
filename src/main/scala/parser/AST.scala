package parser

import scala.util.parsing.input.Positional


sealed trait ASTNode extends Positional
sealed trait ASTLiteral extends ASTNode
case class RootExpression(elements: Seq[ListExpression]) extends ASTNode
case class ListExpression(elements: Seq[ASTNode]) extends ASTNode
case class PrefixedExpression(operator: PrefixOperator, expr: ASTNode) extends ASTNode
case class IntegerLiteral(value: Int) extends ASTLiteral
case class FloatingLiteral(value: Double) extends ASTLiteral
case class StringLiteral(value: String) extends ASTLiteral
case class IdentifierLiteral(value: String) extends ASTLiteral
case class VectorLiteral(elements: Seq[ASTNode]) extends ASTLiteral
case class HashMapLiteral(map: Map[ASTLiteral, ASTNode]) extends ASTLiteral
case class NilLiteral() extends ASTLiteral

sealed trait PrefixOperator
case class TildaOperator() extends PrefixOperator
case class BacktickOperator() extends PrefixOperator
case class DotOperator() extends PrefixOperator

/*
  parser.ListExpression examples:
    (is (= 4 (+ 2 2)))
    (is (instance? Integer 256))
    (is (.startsWith "abcde" "ab"))
 */

