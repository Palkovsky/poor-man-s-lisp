# Poor Man's LISP

Minimalistic LISP dialect implemented in Scala.

## Structures

* strings `(def str "wololo")`
* numeric `(def num1 123)` `(def num2 12.32)`
* collections
    * maps `{k1 v1 k2 v2}`
    * sequences
        * lists `` `(a b c)``
        * vectors `[a b c]`
* nil `nil`
* prefix-operators
    * backtick `` ` `` - prevents evaluation
    * ampersand `&` - for passing varargs
    * tilda `~` - unimplemented
    * dot `.` - unimplemented

## Features

* #### basic arithmetic 
    ```
    +, -, *, div, mod
    ```
* #### comparators 
    ```clojure
    eq, lt, lte, gt, gte
    ```
* #### logic 
    ```clojure
    and, or, not
    ```
* #### type-check predicates
    ```clojure
    nothing?, str?, coll?, seq?, map?, vector?, list?, num?, func?, bool?, identifier?
    ```
* #### sequence manipulators
    * ```clojure
      head seq, tail seq, init seq
      ```
    * ```clojure
      take n seq, drop n seq
      ```
    * ```clojure
      conj seq x, cons x seq
      ```
* #### if statement
    ```clojure
    (if (eq 2 1) then "2 equals 1" else "1 equals 1") ==> "1 equals 1"
    ```
* #### standard higher order functions
    * ```clojure
      (map f coll)
      ```
    * ```clojure
      (filter f coll)
      ```
    * ```clojure
      (reduce f start coll)
      ```
* #### lambdas 
    * ```clojure
      (fn [x y] (+ x y))
      ```
* #### definitions
    * ```clojure
      (def x 3)
      ```
    * ```clojure
      (def x "wololo")
      ```
    * ```clojure
      (def func (fn [x y] (+ x y)))
      ```
        or shorter:
      ```clojure
      (defn func [x y] (+ x y))
      ```
* #### let statements
    * ```clojure
      (let {foo 21 bar 37} (+ foo bar)) ==> 58
      ```
    * ```clojure
      (defn fact [x] (let 
          {f (fn [acc x] 
              (if (eq x 1) then acc else (f (* acc x) (- x 1))))} 
      (f 1 x)))
      (fact 5) ==> 120
      ```
* #### varargs
    ```clojure
    (defn sum [&nums] (reduce + 0 nums)) (sum 1 2 3)  ==> 6
    ```
* #### lazy sequences
    * ##### Fibonacci number generator: 
        ```clojure
        (defn fib [] (let {f (fn [a b] (lazy-seq (cons b (f b (+ a b)))))} (f 0 1) ))
        (take 3 (drop 2 (fib))) ==> [2, 3 ,5]
        ```
    * ##### Multiples generator
        ```clojure
        (defn mults [num] 
            (let {f (fn [i] (* i num)) 
                  g (fn [i] (lazy-seq (cons (f i) (g (+ i 1)))))}  
            (g 1)))
        (take 5 (mults 5)) ==> [5, 10, 15, 20, 25]
        ```

## What it does not have
* negative numbers parsing, to make one: `(- 0 num)` 
* maps manipulation functions
* IO
* some kind of better conditions handling, like `cond` from Clojure
* macros definitions
* and lots of other stuff, too
    
## Code examples

Check [here](https://github.com/Palkovsky/poor-man-s-lisp/tree/master/src/test/scala/interpreter)
