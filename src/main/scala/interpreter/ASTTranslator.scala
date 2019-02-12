package interpreter

import parser._

import scala.collection.mutable

/*
  Object responsible for translating AST into values used by executor.
 */
object ASTTranslator {

  def convert(root: RootExpression): Seq[Identifiable] = root.elements.map(e => toIdentifiable(e))

  def toIdentifiable(node: ASTNode): Identifiable = {
    val converted = node match {
      case IntegerLiteral(value) => IntValue(value)
      case FloatingLiteral(value) => FloatingValue(value)
      case StringLiteral(value) => StringValue(value)
      case IdentifierLiteral(value) => IdentifierValue(value)
      case VectorLiteral(value) => VectorValue(value.map(e => toIdentifiable(e)))
      case NilLiteral() => NilValue()
      case BoolLiteral(value) => BoolValue(value)
      case PrefixedExpression(prefix, value) => PrefixedValue(prefix, toIdentifiable(value))
      case ListExpression(children) => ListValue(children.map(e => toIdentifiable(e)))
      case HashMapLiteral(map) => MapValue(map.foldLeft(mutable.Map[Identifiable, Identifiable]()) { case (acc, (k, v)) => acc += (toIdentifiable(k) -> toIdentifiable(v)) })
    }
    converted.setPos(node.pos)
    converted
  }
}