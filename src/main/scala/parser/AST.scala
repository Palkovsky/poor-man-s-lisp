package parser

import scala.util.parsing.input.Positional


sealed trait ASTNode extends Positional
sealed trait ASTLiteral extends ASTNode
sealed trait ASTHashable extends ASTLiteral
case class RootExpression(elements: Seq[ListExpression]) extends ASTNode
case class ListExpression(elements: Seq[ASTNode]) extends ASTNode
case class PrefixedExpression(operator: PrefixOperator, expr: ASTNode) extends ASTNode
case class IntegerLiteral(value: Int) extends ASTLiteral with ASTHashable
case class FloatingLiteral(value: Double) extends ASTLiteral with ASTHashable
case class StringLiteral(value: String) extends ASTLiteral with ASTHashable
case class IdentifierLiteral(value: String) extends ASTLiteral
case class VectorLiteral(elements: Seq[ASTNode]) extends ASTLiteral
case class HashMapLiteral(map: Map[ASTHashable, ASTNode]) extends ASTLiteral

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

