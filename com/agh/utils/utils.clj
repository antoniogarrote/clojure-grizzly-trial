(clojure/comment
 "Some commons utilities for clojure development"
)

;;
;; @author Antonio Garrote Hernández
;;

(ns com.agh.utils)


(defn trace [msg form]
  "Traces a message and keep on evaluating form"
  (do (println msg)
      form))

;; lambda things

(defmacro curry [func & args]
  "Currifies a function. If we have f(x,y) -> z, then
   curry(f): x -> (y -> z).
   It is possible to obtain the same effect with:
   curry(f) = lambda(x).f(x,y)."
  `(fn [x#] (~func ~@args x#)))

(defmacro c_ [& args]
  "Convenience notations for currying"
  `(curry ~@args))

;; private
(defmacro compose-inner [ x & fs ]
  (let [ fst# (first fs)
         rst# (rest fs) ]
    (if (nil? rst#)
      (if (re-find #"^\(fn[\*]? " (str `~fst#))
        `(~fst# ~x)
        `((curry ~@fst#) ~x))
      (if (re-find #"^\(fn[\*]? " (str `~fst#))
        `(~fst# (compose-inner ~x ~@rst#))
        `( (curry ~@fst#) (compose-inner ~x ~@rst#))))))

(defmacro compose
  "Allow the composition of several currified or single argument
   lambda function, returning a single currified function"
  ([ f ]
     (if (re-find #"^\(fn[\*]? " (str `~f))
       `(fn [x#] (~f x#))
       `(fn [x#] ((curry ~@f) x#))))
  ([ f & fs ]
     (if (re-find #"^\(fn[\*]? "(str `~f))
       `(fn [x#] (~f (compose-inner x# ~@fs)))
       `(fn [x#] ((curry ~@f) (compose-inner x# ~@fs))))))

(defmacro c._ [& args]
  "Convenience notations for composition"
  `(compose ~@args))


(defmacro compose-apply
  "Compose the chain of curryfied/single argument functions and
   applies the last argument as the resulting currified expression"
  ([f x]
     `((compose ~f) ~x))
  ([f g & fsx]
     (let [x# (last fsx)
           fs# (conj (conj (drop-last fsx) g) f)]
       `((compose ~@fs#) ~x#))))

(defmacro c.a_ [& args]
  "Convenience notations for composition and application"
  `(compose-apply ~@args))



(clojure/comment
  "Tests"
)

(use 'clojure.contrib.test-is)

(deftest test-curry
  (is (= ((curry + 1) 3)
         ((fn [x] (+ x 1)) 3))))

(deftest test-composisition-and-curry
  (is (= ((compose (+ 1) (+ 1) (+ 1)) 3)
         ((fn [z] (+ z 1)) ((fn [y] (+ y 1)) ((fn [x] (+ 1 x)) 3))))))

(deftest test-mixture-composition-lambda
  (is (= ((compose (+ 1) (fn [x] (+ x 1)) (+ 1)) 3)
         ((fn [z] (+ z 1)) ((fn [y] (+ y 1)) ((fn [x] (+ 1 x)) 3))))))

(deftest test-filter-curry
  (is (= (filter (c_ = 1) [1 2 3 1 1 2 3])
         (filter (fn [x] (= x 1)) [1 2 3 1 1 2 3]))))

(deftest test-compose-and-apply
  (is (= (compose-apply (+ 1) (+ 1) (+ 1) 3)
         ((fn [z] (+ z 1)) ((fn [y] (+ y 1)) ((fn [x] (+ 1 x)) 3))))))

(deftest test-compose-filters
  (is (= (c.a_ (map (c_ + 2)) (filter (c_ = 1)) [1 2 3 4 1])
         [3 3])))

