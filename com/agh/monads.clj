(comment
 "A simple implementation of a monadic system"
)

;;
;; @author Antonio Garrote Hernández
;;

(ns com.agh.monads)

;; Basic structure of a monad: Ms T
;; Subtype is meant to simulate Algebraic Data Types.
(defstruct Monad :monad-type :monad-subtype :content)

;; Binding operation interface
(defmulti >>= (fn [f m] (:monad-type m)))

(defn return

  "Injects content in a monad"

  ([monad-type value]
    (with-meta (struct Monad monad-type nil value) {:monad monad-type}))

  ([monad-type monad-subtype value]
    (with-meta (struct Monad monad-type monad-subtype value) {:monad monad-type})))

(defn from-monad

  "Returns the content of a monad"

  ([m] (:content m)))


(defmacro do->>=

  "Alternative notation for threading monadic functions similar to Haskell's
   do notation"

  ([initial-form initial-value]
    `(>>= ~initial-form ~initial-value))

  ([initial-form  initial-value & rest-forms]
     (let [fst# (first rest-forms)
           others# (rest rest-forms)]
       (if (nil? others#)
         `(>>=  ~fst# (>>= ~initial-form ~initial-value))
         `(do->>= ~fst# (>>= ~initial-form ~initial-value) ~@others#)))))
