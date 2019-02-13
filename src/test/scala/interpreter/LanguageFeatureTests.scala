package interpreter

import utils.BaseSpec

class LanguageFeatureTests extends BaseSpec {

  describe("Interpreter (language features)") {
    it("Should be able to pass varying number of arguments to function") {
      exec("(defn sum [&args] (reduce + 0 args)) (sum 1 2 3)") should
        equal(NumericValue(6.0))
    }

    it("Should be able to pass varying number of arguments to function at the end") {
      exec("(defn sum [x &args] (- (reduce + 0 args) x)) (sum 10 1 2 3)") should
        equal(NumericValue(-4.0))
    }

    it("Simle let bindings") {
      exec("((fn [x] (let {pow2 (* x 2)} (+ pow2 pow2))) 2)") should
        equal(NumericValue(8.0))
    }

    it("Let scoping") {
      exec("(defn f [x] (let {x 2} x)) (f 3)") should
        equal(NumericValue(2))
    }

    it("More complex let bindings") {
      exec("(defn comp [f g] (fn [&args] (let {res ((cons g (asList args)))} (f res)))) (defn addTwo [x] (+ x 2)) (def f (comp addTwo *)) (f 2 2)") should
        equal(NumericValue(6.0))
    }

    it("Nested let bindings") {
      exec("(let {x 1 y 2 z 3} (let {x (+ y z)} (+ x z)))") should
        equal(NumericValue(8.0))
    }

    it("Recursive lambda in let") {
      exec("(defn func [] (let {f (fn [x] (if (eq x 1) then x else (* x (f (- x 1)))))} (f 4))) (func)") should equal(NumericValue(24.0))
    }


    it("Lazy sequences") {
      exec("(defn fib [] (let {f (fn [a b] (lazy-seq (cons b (f b (+ a b)))))} (f 0 1) )) (take 3 (drop 2 (fib)))") should
        equal(VectorValue(List(NumericValue(2.0), NumericValue(3.0), NumericValue(5.0))))
    }
  }

}
