package interpreter

import utils.BaseSpec

import scala.collection.mutable

class CoreFunctionTests extends BaseSpec {

  describe("Interpreter (core functions)") {

    it("Cons function") {
      exec("((cons - `(1 2)))") should equal(FloatingValue(-1.0))
    }

    it("Fn lambda function") {
      exec("(def x 3) ((fn [x] (eq 0 (mod x 2))) x)") should equal(BoolValue(false))
    }

    it("Apply function") {
      exec("(defn sub [x y] (- x y)) (apply sub `(1 2))") should equal(FloatingValue(-1.0))
    }

    it("Simple if") {
      exec("(if (eq 2 (+ 1 1)) then 2 else \"xD\")") should equal(IntValue(2))
    }

    it("Map function") {
      exec("(defn addTwo [x] (+ x 2)) (map addTwo {x 1 y 2 z 3})") should equal(MapValue(mutable.Map(
        IdentifierValue("x") -> IntValue(3),
        IdentifierValue("y") -> IntValue(4),
        IdentifierValue("z") -> IntValue(5))
      ))
    }

    it("Filter function") {
      exec("(defn even [x] (eq 0 (mod x 2))) (filter even {a 1 b 2 c 3 d 4 e 5})") should equal(MapValue(mutable.Map(
        IdentifierValue("b") -> IntValue(2),
        IdentifierValue("d") -> IntValue(4)
      )))
    }

    it("Reduce function") {
      exec("(def x 2)(def x 3) (reduce + 0 `((- 2 1) x (+ 2 1)))") should equal(IntValue(7))
    }
  }

  it("nothing? function") {
    exec("(nothing? ((fn [x] (id nil)) 2) )") should equal(BoolValue(true))
  }

  it("bool? function") {
    exec("(bool? (and false true))") should equal(BoolValue(true))
  }

  it("num? function"){
    exec("(num? ((fn [x] (+ x 2)) 2))") should equal(BoolValue(true))
  }

  it("func? function"){
    exec("(func? (fn [x] (+ x 2)) )") should equal(BoolValue(true))
  }

  it("seq? function for map"){
    exec("(seq? {x 1 y 2})") should equal(BoolValue(false))
  }

  it("seq? function for list"){
    exec("(seq? `(x 1 y 2))") should equal(BoolValue(true))
  }

  it("coll? for map"){
    exec("(coll? {x 1 y 2})") should equal(BoolValue(true))
  }

  it("list? for vector"){
    exec("(list? [1 2 3])") should equal(BoolValue(false))
  }

  it("logic operators") {
    exec("(not (or (and true true) (and true false)))") should equal(BoolValue(false))
  }

  it("head function") {
    exec("(eq (head [1 2 3]) (head `(1 2 3)))") should equal(BoolValue(true))
  }

  it("head function on empty sequence") {
    exec("(and (nothing? (head [])) (nothing? (head `())))") should equal(BoolValue(true))
  }

  it("tail function"){
    exec("(eq (tail [1 2 3]) (asVector (tail `(5 2 3))))") should equal(BoolValue(true))
  }

  it("tail function on empty sequence") {
    exec("(and (nothing? (tail [])) (nothing? (tail `())))") should equal(BoolValue(true))
  }

  it("tail function on singleton") {
    exec("(and (empty? (tail [1])) (empty? (tail `(1))))") should equal(BoolValue(true))
  }

  it("init function"){
    exec("(eq (init [1 2 3]) (asVector (init `(1 2 5))))") should equal(BoolValue(true))
  }

  it("init function on empty sequence") {
    exec("(and (nothing? (init [])) (nothing? (init `())))") should equal(BoolValue(true))
  }

  it("init function on singleton") {
    exec("(and (empty? (init [1])) (empty? (init `(1))))") should equal(BoolValue(true))
  }

  it("take and drop functions") {
    exec ("(concat (take 4 [1 2 3 4 5]) (drop 3 `(1 2 3 4 5)))") should equal(VectorValue(List(IntValue(1), IntValue(2), IntValue(3), IntValue(4), IntValue(4), IntValue(5))))
  }

  it("empty? function on map") {
    exec("(empty? {})") should equal(BoolValue(true))
  }
}