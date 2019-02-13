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
    * backtick `` ` `` - prevents calling by list
    * ampersand `&` - for varargs passing
    * tilda `~` - unimplemented
    * dot `.` - unimplemented

## Features

* #### basic arithmetic 
    `+, -, *, div, mod`
* #### comparators 
    `eq, lt, lte, gt, gte`
* #### logic 
    `and, or, not`
* #### type-check predicates
    `nothing?, str?, coll?, seq?, map?, vector?, list?, num?, func?, bool?, identifier?`
* #### sequence manipulators
    * `head seq, tail seq, init seq`
    * `take n seq, drop n seq`
    * `conj seq x, cons x seq`
* #### if statement
    `(if (eq 2 1) then "2 equals 1" else "1 equals 1") ==> "1 equals 1"`
* #### standard higher order functions
    * `(map f coll)`
    * `(filter f coll)`
    * `(reduce f base coll)`
* #### lambdas 
    * `(fn [x y] (+ x y))`
* #### definitions
    * `(def x 3)`
    * `(def x "wololo")`
    * `(def func (fn [x y] (+ x y)))`, shorter: `(defn func [x y] (+ x y))`
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
    * Fibonacci number generator: 
        ```clojure
        (defn fib [] (let {f (fn [a b] (lazy-seq (cons b (f b (+ a b)))))} (f 0 1) ))
        (take 3 (drop 2 (fib))) ==> [2, 3 ,5]
        ```
    * Multiples generator
        ```clojure
            (defn mults [num] 
                (let {f (fn [i] (* i num)) 
                      g (fn [i] (lazy-seq (cons (f i) (g (+ i 1)))))}  
                (g 1)))
            (take 5 (mults 5)) ==> [5, 10, 15, 20, 25]
        ```

    
## Code examples

Check [here](https://github.com/Palkovsky/poor-man-s-lisp/tree/master/src/test/scala/interpreter)