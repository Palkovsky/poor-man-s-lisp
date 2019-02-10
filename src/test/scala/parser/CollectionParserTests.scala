package parser

import utils.BaseSpec

class CollectionParserTests extends BaseSpec {

  describe("Parser (collections)") {

    it("Should parse lists") {
      parse("(1 2)") should equal(RootExpression(List(
        ListExpression(List(
          IntegerLiteral(1), IntegerLiteral(2)
        ))
      )))
    }

    it("Should parse vectors") {
      parse("([1 2] 2)") should equal(
        RootExpression(List(
          ListExpression(List(
            VectorLiteral(List(IntegerLiteral(1), IntegerLiteral(2))),
            IntegerLiteral(2)
          ))
        )))
    }

    it("Should parse maps") {
      parse("({key value} 2)") should equal(
        RootExpression(List(
          ListExpression(List(
            HashMapLiteral(Map(IdentifierLiteral("key") -> IdentifierLiteral("value"))),
            IntegerLiteral(2)
          ))
        )))
    }
  }
}
