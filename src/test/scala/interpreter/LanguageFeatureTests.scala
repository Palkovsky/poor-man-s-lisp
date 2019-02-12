package interpreter

import utils.BaseSpec

class LanguageFeatureTests extends BaseSpec {

    describe("Interpreter (language features)") {
      it ("Should be able to pass varying number of arguments to function"){
        exec("(defn sum [&args] (reduce + 0 args)) (sum 1 2 3)") should
          equal(FloatingValue(6.0))
      }

      it ("Should be able to pass varying number of arguments to function at the end"){
        exec("(defn sum [x &args] (- (reduce + 0 args) x)) (sum 10 1 2 3)") should
          equal(FloatingValue(-4.0))
      }

      it("Simle let bindings") {
        exec("((fn [x] (let {pow2 (* x 2)} (+ pow2 pow2))) 2)") should
          equal(FloatingValue(8.0))
      }

      it("More complex let bindings") {
        exec ("(defn comp [f g] (fn [&args] (let {res ((cons g (asList args)))} (f res)))) (defn addTwo [x] (+ x 2)) (def f (comp addTwo *)) (f 2 2)") should
          equal(FloatingValue(6.0))
      }

      it("Nested let bindings") {
        exec ("(let {x 1 y 2 z 3} (let {x (+ y z)} (+ x z)))") should
          equal(FloatingValue(8.0))
      }
    }

}
