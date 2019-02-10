package interpreter

import utils.BaseSpec

class CustomFunctionTests extends BaseSpec{

  describe("Interpreter (user-defined functions)") {
    it ("Should be able to define, call functions and pass them as parameters"){
      exec("(defn sub [x y] (- x y)) (defn rev [f x y] (f y x)) (rev sub 5 1)") should equal(FloatingValue(-4.0))
    }
  }

}
