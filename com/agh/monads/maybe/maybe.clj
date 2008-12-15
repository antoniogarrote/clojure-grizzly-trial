(clojure/comment
   "A sample implementation of the Maybe monad from Haskell"
)

;;
;;  @author Antonio Garrote Hernández
;;

(ns com.agh.monads.maybe
  (:use com.agh.monads))

;;
;; Maybe X = Just X | Nothing
;;

(defn nothing?
  "Checks if the monad is a Nothing Maybe monad"
  [m] (= (:monad-subtype m) :Nothing))

(defn from-maybe
  "Extracts the content from a Maybe monad"
  [m] (:content m))

(defn just
  "Creates a new Maybe monad with just v as the content"
  {:monad :Maybe}
  [v] (return :Maybe :Just v))

(defn nothing 
  "Createas a new Maybe monad with Nothing"
  {:monad :Maybe}
  [] (return :Maybe :Nothing nil))

(defmethod >>= :Maybe [f m]
  "Instance of the >>= function for the Maybe monad. If the computation
   result is Just X adds f to the chaing, if the computations is Nothing
   breaks the chain and returns Nothing"
   (if (nothing? m)
          (nothing)
          (f (:content m))))


(clojure/comment
  "tests"
)

(use 'clojure.contrib.test-is)

(defn check-random
  ([acumulator]
    (let [value (.. (new java.util.Random)
                  (nextInt 10))]
      (if (< value 5)
        (just (str acumulator value))
        (nothing))))
  ([] (check-random "")))


(deftest test-maybe-monad
  (is (= (:monad-type (>>= check-random
                        (>>= check-random
                          (>>= check-random
                            (just "")))))
        :Maybe)))

(deftest test-maybe-monad-do
  (is (= (:monad-type (do->>= check-random (just "")
                              check-random
                              check-random         ))
        :Maybe)))

(clojure/comment

(deftest test-maybe-monad-lambdas
  (is (= (>>= (fn [y] (just (str y x "a")))
           (>>= (fn [x] (just "a"))
             (>>= (fn [] (just "a"))
               (just ""))))
        "aaa")))

)

(clojure/comment

   (do-b->>= (just "") (check-random :as x)
                       (check-random :as y)
                       (fn [] (do (println (str x y)) 
                                  (check-random))))

)