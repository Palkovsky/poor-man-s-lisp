package parser

import utils.BaseSpec

class PrefixedExpressionParserTests extends BaseSpec{
  describe("Parser (prefixed expressions)") {

    it("Should parse tilda prefix") {
      parse("(nil ~(sub 3.2 1))") should equal(RootExpression(List(
        ListExpression(List(
          NilLiteral(),
          PrefixedExpression(TildaOperator(), ListExpression(List(
            IdentifierLiteral("sub"), FloatingLiteral(3.2), IntegerLiteral(1)
          )))
        ))
      )))
    }

    it("Should parse backtick prefix") {
      parse("(nil `(sub 3.2 true))") should equal(RootExpression(List(
        ListExpression(List(
          NilLiteral(),
          PrefixedExpression(BacktickOperator(), ListExpression(List(
            IdentifierLiteral("sub"), FloatingLiteral(3.2), BoolLiteral(true)
          )))
        ))
      )))
    }

    it("Should parse dot prefix") {
      parse("(nil (.sub 3.2 true))") should equal(RootExpression(List(
        ListExpression(List(
          NilLiteral(),
          ListExpression(List(
            PrefixedExpression(DotOperator(), IdentifierLiteral("sub")), FloatingLiteral(3.2), BoolLiteral(true)
          ))
        ))
      )))
    }

    it("Should parse ampersand prefix") {
      parse("(nil (&sub 3.2 true))") should equal(RootExpression(List(
        ListExpression(List(
          NilLiteral(),
          ListExpression(List(
            PrefixedExpression(AmpersandOperator(), IdentifierLiteral("sub")), FloatingLiteral(3.2), BoolLiteral(true)
          ))
        ))
      )))
    }
  }
}
