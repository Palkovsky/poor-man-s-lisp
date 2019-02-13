package interpreter

import utils.BaseSpec

class CustomFunctionTests extends BaseSpec{

  describe("Interpreter (user-defined functions)") {
    it ("Should be able to define, call functions and pass them as parameters"){
      exec("(defn sub [x y] (- x y)) (defn rev [f x y] (f y x)) (rev sub 5 1)") should equal(NumericValue(-4.0))
    }

    it("Own composition function"){
      exec("(defn comp [f g] (fn [x] (f (g x)))) (defn addTwo [x] (+ x 2)) (defn multTwo [x] (* x 2)) (def f (comp addTwo multTwo)) (f 3)") should equal(NumericValue(8.0))
    }

    it("Multi arg composition function"){
      exec("(defn comp [f g] (fn [&args] (f ((cons g (asList args))) ))) (defn addTwo [x] (+ x 2)) (def f (comp addTwo +)) (f 3 1)") should equal(NumericValue(6.0))
    }

    it("Recursive pow function"){
      exec("(defn pow [x a] (if (eq a 1) then x else (* x (pow x (- a 1))))) (pow 4 3)") should equal(NumericValue(64.0))
    }

    it("Factorial function with accumulator") {
      exec("(defn fact [x] (let {f (fn [acc x] (if (eq x 1) then acc else (f (* acc x) (- x 1))))} (f 1 x))) (fact 5)") should
        equal(NumericValue(120))
    }

    it("Range function") {
      exec("(defn range [l u] (if (gt l u) then [] else (lazy-seq (cons l (range (+ l 1) u))))) (take 2 (range 1 100))") should
        equal(VectorValue(List(NumericValue(1), NumericValue(2))))
    }

    it("Multiples generator") {
      exec("(defn mults [num] (let {f (fn [i] (* i num)) g (fn [i] (lazy-seq (cons (f i) (g (+ i 1)))))}  (g 1))) (take 5 (mults (- 0 5)))") should
        equal(VectorValue(List(NumericValue(-5), NumericValue(-10), NumericValue(-15), NumericValue(-20), NumericValue(-25))))
    }
  }

}
