(clojure/comment
 "A monad wrapping a piece of state such as a hash map and transforms
  it sequentially"
)

;;
;; @ Antonio Garrote Hernández
;;

(ns com.agh.monads.state
  (:use com.agh.monads)
  (:use com.agh.utils))

(defn applied-to 
  "Wraps the data into the state monad"
  {:monad :State}
  ([data] (return :State data)))

(defmethod >>= :State [f s]
  "pass the modified content to the next step of the
   transformation"
  (f (:content s)))

(defn transform
  "Modifies the state stored in the State monad returning the modified
   state of the monad"
  {:monad :State}
  ([keyword func state]
    (let [current-value (keyword state)]
      (return :State (assoc state keyword (func current-value))))))


(clojure/comment
  "tests"
)

(use 'clojure.contrib.test-is)

(deftest test-mg-applied-to
  (is (= (applied-to "test")
         (struct Monad :State nil "test"))))

(deftest test-m-assoc-currified
  (is (=
        (do->>= (curry transform :count (curry + 1)) (applied-to {:count 1})
                (curry transform :count (curry + 1))
                (curry transform :count (curry + 1)))
        (return :State {:count 4}))))

(deftest test-m-assoc-currified-simp
  (is (=
        (do->>= (c_ transform :count (c_ + 1)) (applied-to {:count 1})
                (c_ transform :count (c_ + 1))
                (c_ transform :count (c_ + 1)))
        (return :State {:count 4}))))