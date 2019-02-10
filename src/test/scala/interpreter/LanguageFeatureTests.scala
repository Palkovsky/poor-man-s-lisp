package interpreter

import utils.BaseSpec

class LanguageFeatureTests extends BaseSpec {

    describe("Interpreter (language features)") {
      it ("Should be able to pass varying number of arguments to function"){
        exec("(defn sum [&args] (reduce + 0 args)) (sum 1 2 3)") should equal(FloatingValue(6.0))
      }

      it ("Should be able to pass varying number of arguments to function at the end"){
        exec("(defn sum [x &args] (- (reduce + 0 args) x)) (sum 10 1 2 3)") should equal(FloatingValue(-4.0))
      }
    }

}
