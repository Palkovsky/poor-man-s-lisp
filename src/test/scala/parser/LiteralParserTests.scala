package parser

import utils.BaseSpec

class LiteralParserTests extends BaseSpec{
  describe("Parser (literals)") {

    it("Should parse nils") {
      parse("(nil nil)") should equal(RootExpression(List(
        ListExpression(List(
          NilLiteral(), NilLiteral()
        ))
      )))
    }

    it("Should parse booleans") {
      parse("(true false)") should equal(RootExpression(List(
        ListExpression(List(
          BoolLiteral(true), BoolLiteral(false)
        ))
      )))
    }

    it("Should parse integers") {
      parse("(1 2 3 4)") should equal(RootExpression(List(
        ListExpression(List(
          IntegerLiteral(1), IntegerLiteral(2), IntegerLiteral(3), IntegerLiteral(4)
        ))
      )))
    }

    it("Should parse floats") {
      parse("(1.1 22.4 0.4 21.5)") should equal(RootExpression(List(
        ListExpression(List(
          FloatingLiteral(1.1), FloatingLiteral(22.4), FloatingLiteral(0.4), FloatingLiteral(21.5)
        ))
      )))
    }

    it("Should parse identifiers") {
      parse("(x y z)") should equal(RootExpression(List(
        ListExpression(List(
          IdentifierLiteral("x"), IdentifierLiteral("y"), IdentifierLiteral("z")
        ))
      )))
    }
  }
}
