(ns dnf.core
  (:gen-class)
  (:require clojure.set))

(ns dnf.core-test
  (:require [clojure.test :refer [deftest testing is]]))

(defn constant "Constant creation" [num]
  (list ::c num))

(def isConstansTrue (constant true))
(def isConstansFalse (constant false))

(defn isConstant "Check if expression is constant" [expr]
  (= (first expr) ::c))

(defn variable "Variable creation" [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn isVariable "Check if expression is variable" [expr]
  (= (first expr) ::var))

(defn getVariableName "Get variable name" [expr]
  (second expr))

(defn conjuction "Conjuction creation" [expr & rest]
  (cons ::conj (cons expr rest)))

(defn isConjuction "Check if expression is conjuction" [expr]
  (= (first expr) ::conj))

(defn disjunction "Disjunction creation" [expr & rest]
  (cons ::disj (cons expr rest)))

(defn isDisjunction "Check if expression is disjunction" [expr]
  (= (first expr) ::disj))

(defn implication "Implication creation" [expr1 expr2]
  (list ::impl expr1 expr2))

(defn isImplication "Check if expression is implication" [expr]
  (= (first expr) ::impl))

(defn args "Get expression arguments" [expr]
  (rest expr))

(defn invert "Create invertion expression" [expr]
  (list ::inv expr))

(defn isInvert "Check if expression is invertion" [expr]
  (= (first expr) ::inv))

(defn isTypeEquals "Check if two expression has same type" [expr1, expr2]
  (= (first expr1) (first expr2)))

(defn isEquals "Check if two expression absolutely similar" [expr1, expr2]
  (if (isTypeEquals expr1 expr2)
    (if (and (or (isConstant (first expr1)) (isVariable (first expr1)))
             (or (isConstant (first expr2)) (isVariable (first expr2))))
      (= (second expr1) (second expr2))
      (or (and (isEquals (second expr1) (second expr2)) (isEquals (last expr1) (last expr2)))
          (and (isEquals (second expr1) (last expr2)) (isEquals (last expr1) (second expr2)))))
    false))

(defn isElementaryFunction "Check if expression is elementary function" [expr]
  (or (isConstant expr) (isVariable expr)
   (and (isInvert expr) (or (isConstant (last expr)) (isVariable (last expr))))))

(defn isDnf "Check if expression is dnf" [expr]
  (cond (isElementaryFunction expr) true
    (isConjuction expr)
      (reduce (fn [acc x] (and acc x)) (map isElementaryFunction (args expr)))
    (isDisjunction expr)
      (reduce (fn [acc x] (and acc x)) (map isDnf (args expr)))
  :else false))

(defn deMorganRule "Apply De Morgan rule to expression" [expr]
  (if (isConjuction expr)
    (disjunction (invert (second expr)) (invert (last expr)))
    (conjuction (invert (second expr)) (invert (last expr)))))

(defn isAlmostElementary "Check if expression is elementary or simple conjuction or disjunction" [expr]
  (or (isElementaryFunction expr)
    (and (isConjuction expr) (isElementaryFunction (second expr)) (isElementaryFunction (last expr)))
    (and (isDisjunction expr) (isElementaryFunction (second expr)) (isElementaryFunction (last expr)))))


(defn distribution "Apply distribution to expression"[expr]
  (if (isAlmostElementary expr)
    expr
    (if (isConjuction expr)
      (if (isDisjunction (last expr))
        (disjunction (conjuction (second expr) (second (last expr))) (conjuction (second expr) (last (last expr))))
        (disjunction (conjuction (last expr) (second (second expr))) (conjuction (last expr) (last (second expr))))
      )
      (if (isConjuction (last expr))
       (conjuction (disjunction (second expr) (second (last expr))) (disjunction (second expr) (last (last expr))))
       (conjuction (disjunction (last expr) (second (second expr))) (disjunction (last expr) (last (second expr))))
      )
    )
  ))

(defn recurDistribution "Apply resur distribution to expression" [expr]
  (if (isConjuction expr)
    (distribution (conjuction (recurDistribution (second expr)) (recurDistribution (last expr))))
    (if (isDisjunction expr)
      (disjunction (recurDistribution (second expr)) (recurDistribution (last expr)))
      expr)))

(defn deleteInvert "Apply double invertion rule to expression" [expr]
  (last (last expr)))

(defn isReducable "Check if expression is conjuction of disjunction or disjunction of conjunction" [expr]
  (cond (and (isConjuction expr) (isVariable (second expr)) (isDisjunction (last expr))) true
        (and (isConjuction expr) (isVariable (last expr)) (isDisjunction (second expr))) true
        (and (isDisjunction expr) (isVariable (last expr)) (isConjuction (second expr))) true
        (and (isDisjunction expr) (isVariable (second expr)) (isConjuction (last expr))) true
        :else false))

(defn reducing "Apply reducing" [expr]
  (cond (and (isConjuction expr) (isVariable (second expr)) (isDisjunction (last expr))) (second expr)
        (and (isConjuction expr) (isVariable (last expr)) (isDisjunction (second expr))) (last expr)
        (and (isDisjunction expr) (isVariable (last expr)) (isConjuction (second expr))) (last expr)
        (and (isDisjunction expr) (isVariable (second expr)) (isConjuction (last expr))) (second expr)))

(defn isArgsContainsConst "Check if expression arguments contains const" [expr const]
  (some (fn [x] (and (isConstant x) (= const x))) (args expr)))

(defn checkSameVariableNeg "Reduce from variable with this invertion" [expr]
  (if (and (or (isConjuction expr) (isDisjunction expr))
    (< 0 (count
      (clojure.set/intersection
        (apply hash-set (filter (fn [x] (isVariable x)) (args expr)))
        (apply hash-set (map (fn [x] (first (args x))) (filter (fn [x] (and (isInvert x) (isVariable (first (args x)))))(args expr))))))))
    (cond (isConjuction expr) isConstansFalse (isDisjunction expr) isConstansTrue)
    expr))

(defn updateArgs "Add new agrument to expression" [expr new-args]
  (if (> (count new-args) 1)
    (cons (first expr) new-args)
    (list (first expr) (first new-args))))

(defn collectArgs "Collect same types expression" [expr]
  (if (isElementaryFunction expr)
    (list expr)
    (apply concat
      (map (fn [arg] (if (isTypeEquals expr arg)
        (collectArgs arg)
        (list arg))) (args expr)))))

(defn decompose "Decompose expression" [expr]
  (if (and (not (isElementaryFunction expr))
      (or (isDisjunction expr) (isConjuction expr)))
    (updateArgs expr (map (fn [x] (decompose x)) (collectArgs expr))) expr))

(defn simplify "Simplify conjunction and disjunction if constant present" [expr]
  (let [expr (decompose expr)]
    (if (isElementaryFunction expr)
      expr
      (cond (and (isConjuction expr) (isArgsContainsConst expr isConstansFalse)) isConstansFalse
                 (isConjuction expr) (checkSameVariableNeg (apply isConjuction
                    (distinct (filter (fn [x] (not (= x isConstansTrue))) (args expr)))))
            (and (isDisjunction expr) (isArgsContainsConst expr isConstansTrue)) isConstansTrue
                 (isDisjunction expr) (checkSameVariableNeg (apply isDisjunction
                    (distinct (filter (fn [x] (not (= x isConstansFalse))) (args expr))))) :else expr))))

(defn dnf "Apply dnf" [expr]
  (cond
    (empty? expr) expr
    (isElementaryFunction expr) expr
    (isAlmostElementary expr) (cons (first expr) (list (dnf (second expr)) (dnf (last expr))))
    (isImplication expr) (disjunction (invert (second expr)) (last expr))
    (and (isInvert expr) (or (isConjuction (last expr)) (isDisjunction (last expr)))) (dnf (deMorganRule (last expr)))
    (and (isInvert expr) (isInvert (last expr))) (deleteInvert expr)
    (or (and (isConjuction expr) (or (isDisjunction (second expr)) (isDisjunction (last expr))))
        (and (isDisjunction expr) (or (isConjuction (second expr)) (isConjuction (last expr))))) (recurDistribution expr)
    (isReducable expr) (reducing expr)
    (or (isConjuction expr) (isDisjunction expr)) (dnf (cons (first expr) (list (dnf (second expr)) (dnf (last expr)))))
    :else (dnf (list (first expr) (dnf (last expr))))))

(defn dnfWithConstant "Apply dnf with constant" [expr]
  (simplify (dnf expr)))

(defn substitution "Variable substitution constant" [expr, dictionary]
  (simplify (cond
    (isVariable expr)
      (if (contains? dictionary (getVariableName expr))
        (get dictionary (getVariableName expr))
        expr)
    (isConstant expr) expr
    :else (cons (first expr) (map (fn [x] (substitution x dictionary)) (args expr))))))

(println(dnf (conjuction (variable :a) (disjunction (variable :a) (variable :b)))))



(println "======================START TESTS======================")

(deftest isDnfTest
  (testing "HELP, I fail."
    (is (= false (isDnf (implication :x :y))))
    (is (= true (isDnf (constant true))))
    (is (= true (isDnf (variable :x))))
    (is (= true (isDnf (conjuction (variable :x) (variable :y)))))
    (is (= true (isDnf (disjunction (variable :x) (variable :y)))))
    (is (= true (isDnf (conjuction (variable :y) (invert (variable :x))))))))

(deftest substitutionTest
  (testing "HELP, I fail."
    (is (= (implication isConstansFalse isConstansTrue) (substitution (implication (variable :x) (variable :y)) {:x isConstansFalse :y isConstansTrue})))))

(deftest dnfWithConstantTest
  (testing "HELP, I fail."
    (is (= isConstansTrue (dnfWithConstant (substitution (implication (variable :x) (variable :y)) {:x isConstansFalse :y isConstansTrue}))))))

(deftest simplifyTest
  (testing "HELP, I fail."
    (is (= isConstansTrue (simplify (disjunction isConstansTrue isConstansFalse))))))

(deftest decomposeTest
  (testing "HELP, I fail."
    (is (= isConstansTrue (simplify (disjunction isConstansTrue isConstansFalse))))))


(deftest dnfTest
  (testing "HELP, I fail."
    (is (= (conjuction (invert (variable :x)) (invert (variable :y))) (dnf (invert (disjunction (variable :x) (variable :y))))))
    (is (= (variable :x) (dnf (variable :x))))
    (is (= (constant true) (dnf (constant true))))
    (is (= (invert (variable :x)) (dnf (invert (variable :x)))))
    (is (= (conjuction (variable :x) (variable :y)) (dnf (conjuction (variable :x) (variable :y)))))
    (is (= (conjuction (variable :x) (variable :y)) (dnf (conjuction (variable :x) (variable :y)))))
    (is (= (disjunction (variable :x) (variable :y)) (dnf (disjunction (variable :x) (variable :y)))))
    (is (= (conjuction (invert (variable :x)) (invert (variable :y))) (dnf (invert (disjunction (variable :x) (variable :y))))))
    (is (= (disjunction (conjuction (variable :b) (variable :c)) (conjuction (variable :b) (variable :d))) (dnf (conjuction (variable :b) (disjunction (variable :c) (variable :d))))))
    (is (= (disjunction (variable :a) (disjunction (conjuction (variable :b) (variable :c)) (conjuction (variable :b) (variable :d)))) (dnf (disjunction (variable :a) (conjuction (variable :b) (disjunction (variable :c) (variable :d)))))))
    (is (= (disjunction (invert (variable :x)) (variable :y)) (dnf (implication (variable :x) (variable :y)))))
    (is (= (disjunction (invert (variable :x)) (invert (variable :y))) (dnf (invert (conjuction (variable :x) (variable :y))))))
    (is (= (conjuction (invert (variable :x)) (invert (variable :y))) (dnf (invert (disjunction (variable :x) (variable :y))))))
    (is (= (conjuction (variable :x) (invert (variable :y))) (dnf (invert (implication (variable :x) (variable :y))))))
    (is (= (disjunction (variable :z) (conjuction (variable :x) (invert (variable :y)))) (dnf (disjunction (variable :z) (invert (implication (variable :x) (variable :y)))))))
    (is (= (disjunction (disjunction (invert (variable :z)) (variable :x)) (conjuction (variable :x) (invert (variable :y)))) (dnf (disjunction (implication (variable :z) (variable :x)) (invert (implication (variable :x) (variable :y)))))))))

(println "======================END TESTS======================")
(println "!!!!!!If there is nothing between the start and the end, then everything is OK!!!!!!")
