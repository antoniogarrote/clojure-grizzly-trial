(clojure/comment
 "A monad wrapping a piece of state such as a hash map and transforms
  it sequentially"
)

(ns 'com.agh.monads.state)

(use 'com.agh.monads)

(defn stateful
  "Wraps the data into the state monad"
  {:monad :State}
  ([data] (return :State {:state {} :data data}))
  ([state data] (return :State {:state state :data data})))

(defmethod >>= :State [f m]
  "pass the modified content to the next step of the
   transformation"
    (f m))

(defn put
  "stores some data in the state monad"
  {:monad :State}
  ([name value f m]
    (let [state (:state m)
          data (:data m)]
      (let [res (f data)]
        (stateful (from-monad (transform value  (fn [x] res) state)) nil)))))

(defn pass
  "Carries on the computation passing the state to the next step"
  {:monad :State}
  ([f m]
    (let [state (:state m)
          data (:data m)]
      (let [res (f data)]
        (stateful state data)))))