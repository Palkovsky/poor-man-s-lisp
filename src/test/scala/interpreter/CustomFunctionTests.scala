package interpreter

import utils.BaseSpec

class CustomFunctionTests extends BaseSpec{

  describe("Interpreter (user-defined functions)") {
    it ("Should be able to define, call functions and pass them as parameters"){
      exec("(defn sub [x y] (- x y)) (defn rev [f x y] (f y x)) (rev sub 5 1)") should equal(FloatingValue(-4.0))
    }

    it("Own composition function"){
      exec("(defn comp [f g] (fn [x] (f (g x)))) (defn addTwo [x] (+ x 2)) (defn multTwo [x] (* x 2)) (def f (comp addTwo multTwo)) (f 3)") should equal(FloatingValue(8.0))
    }

    it("Multi arg composition function"){
      exec("(defn comp [f g] (fn [&args] (f ((cons g (asList args))) ))) (defn addTwo [x] (+ x 2)) (def f (comp addTwo +)) (f 3 1)") should equal(FloatingValue(6.0))
    }
  }

}
