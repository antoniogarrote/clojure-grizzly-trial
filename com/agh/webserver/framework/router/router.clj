(comment
  "A mechanism for translating URL requests into the selection of a dispatch
   function"
)

;;
;; @author Antonio Garrote Hernández
;;

(ns com.agh.webserver.framework.router)

(defn parse-request-params
  "Translates a HTTP request params string (k1=v1&k2=v2...) into a params hash"
  ([request-params]
     (reduce
      (fn [h1 h2] (merge h1 h2))
      (map #(hash-map (keyword (first %)) (second %))
           (map #(. % split "=")
                (. request-params split "&"))))))


(defn tokenize-request-path
  "Translates a request URL minus the params into a sequence of tokens"
  ([request-path]
     (filter #(not= % "") (. request-path split "/"))))


(defn extract-request-method
  "Parses the REQUEST_METHOD Rack environment variable and returns the
   correct request method symbol"
  ([rack-env]
     (keyword (. (. rack-env (get "REQUEST_METHOD")) toLowerCase))))


(defn rehash-rack-env
  "Transforms a rack environment to a simple hash.
   keys are granted to be smallcase keywords."
  ([rack-env]
     (let [keys (keys rack-env)]
       (reduce
        (fn [ac i] (conj { (keyword (. i toLowerCase)) (. rack-env (get i))} ac))
        {}
        keys))))


;; Route parser

(use 'com.agh.monads.maybe)

;; lexer
(defn is-token-url-part?
 [token]
 (= (:type token) :url-part))

(defn is-token-http-method?
 [token]
 (= (:type token) :http-method))

(defn token-value [token] (:value token))


;; parser combinators
(defn run-one
  "Accepts a parser, a stream of tokens and a state
   and runs the the parser on the first token of the
   stream with the given state"
  {:monad :Maybe}
  ([parser computation]
     (let [tokens (first computation)
           state (second computation)] ;; we extract the tokens and state
       (if (nil? tokens) ;; no more tokens?
         (nothing)  ;; error, there should be something more to parse
         (let [parsing-result (parser (first tokens) state)] ;; run the parser
           (if (nothing? parsing-result) ;; parsing error
             (nothing)
             (just (list (rest tokens) ;; on success return rest of tokens and new state
                         (from-maybe parsing-result)))))))))

(defn run-many
  "Accepts a parser, a stream of tokens and a state
   and runs the parser on every token of the stream
   till the parser fails or there
   are no more tokens left, returning success"
  {:monad :Maybe}
  ([parser computation]
     (loop [current-computation computation] ;; keep on looping till no more tokens or parser error
       (let [tokens (first current-computation)]
         (if (nil? tokens) ;; end of stream?
           (just computation) ;; then success with the current state
           (let [parsing-result (run-one parser current-computation)] ;; run parser one time
             (if (nothing? parsing-result) ;; error?
               (just current-computation) ;; return last successful state and stream
               (recur (from-maybe parsing-result))))))))) ;; let's parse another token


(defn run-and
  "Accepts a list of parsers and run then sequentially on the
   same token"
  {:monad :Maybe}
  ([parsers computation]
     (let [tokens (first computation)
           state (second computation)]
       (if (nil? tokens)
         (nothing)
         (loop [the-parsers parsers
                current-state state]
           (let [the-parser (first the-parsers)
                 result (the-parser (first tokens) current-state)]
             (if (nothing? result)
               (nothing)
               (if (nil? (rest parsers))
                 (just (list (rest tokens)
                             (from-maybe result)))
                 (recur (rest parsers)
                        (from-maybe result))))))))))


(defn run-or
  "Accepts a list of parsers and returns the result of the firs
   successful parser or nothing if every parser fails"
  {:monad :Maybe}
  ([parsers computation]
     (let [tokens (first computation)
           state (second computation)]
       (if (nil? tokens)
         (nothing)
         (loop [the-parsers parsers
                current-state state]
           (let [the-parser (first the-parsers)
                 result (the-parser (first tokens) current-state)]
             (if (nothing? result)
                 (recur (rest parsers)
                        (from-maybe result))
                 (just (list (rest tokens)
                             (from-maybe result))))))))))




;; parser
(defn parse-string-eq
  "Parses a token checking for string equality"
  {:monad :Maybe}
  ([string token state]
     (if (is-token-url-part? token)
       (if (= (token-value token) string)
         (just state)
         (nothing))
       (nothing))))

(defn parse-http-method
  "Parses a token looking for a HTTP method"
  {:monad :Maybe}
  ([method token state]
     (if (is-token-http-method? token)
       (if (= (token-value token) method)
         (just state)
         (nothing))
       (nothing))))

(defn parse-variable
  "Captures the token as a variable with the given name"
  {:monad :Maybe}
  ([name token state]
     (let [keyword-name (if (keyword? name) name (keyword name))]
     (if (is-token-url-part? token)
       (just (merge {keyword-name (token-value token)} state))
       (nothing)))))

(defn parse-regex
  "Parses a token checking for a provided regular expression"
  {:monad :Maybe}
  ([regex token state]
     (if (is-token-url-part? token)
       (if (re-matches regex (token-value token))
         (just state)
         (nothing)))))

(defn parse-anything
  "Parses any token correctly"
  {:monad :Maybe}
  ([token state] (just state)))


(clojure/comment
  "Tests"
)

(use 'clojure.contrib.test-is)

(defn mock-environment [kv]
  (let [r (new java.util.HashMap)]
    (loop [pair (first kv)
           pairs (rest kv)]
      (. r (put (first pair) (second pair)))
      (if (not (nil? pairs))
          (recur (first pairs) (rest pairs))))
    r))

(use 'com.agh.utils)
(use 'com.agh.monads)

(deftest test-extract-request-method
  (is (=
       (extract-request-method {"REQUEST_METHOD" "GET"})
       :get)))

(deftest test-rehash-rac-env
  (is (=
       (rehash-rack-env (mock-environment {"HOLA" 1 "OTHER" 2 "teSt" 3}))
       { :hola 1, :other 2, :test 3 })))

;; parser

(deftest test-parse-string-eq-ok
  (is (= (parse-string-eq "test" {:type :url-part :value "test"} {})
         (just {}))))

(deftest test-parse-string-eq-nok
  (is (= (parse-string-eq "test" {:type :url-part :value "other"} {})
         (nothing))))

(deftest test-parse-string-eq-wrong-type
  (is (= (parse-string-eq "test" {:type :http-method :value :get} {})
         (nothing))))

(deftest test-parse-method-eq-ok
  (is (= (parse-http-method :get {:type :http-method :value :get} {})
         (just {}))))

(deftest test-parse-method-eq-nok
  (is (= (parse-http-method :post {:type :http-method :value :get} {})
         (nothing))))

(deftest test-parse-method-eq-wrong-type
  (is (= (parse-http-method :get {:type :url-part :value "test"} {})
         (nothing))))

(deftest test-parse-var-ok
  (is (= (parse-variable :test {:type :url-part :value "23"} {:a 1})
         (just {:a 1 :test "23"}))))

(deftest test-parse-regex-ok
  (is (= (parse-regex '#"[a-z]+" {:type :url-part :value "test"} {})
         (just {}))))

(deftest test-parse-regex-nok
  (is (= (parse-regex '#"[a-z]+" {:type :url-part :value "1111"} {})
         (nothing))))

(defn mock-request-http-tokens [method & parts]
  (let [result (if (nil? method)
                 '()
                 (list {:type :http-method :value method}))]
    (loop [parts-left parts
           tmp result]
      (if (nil? parts-left)
        (reverse tmp)
        (recur (rest parts-left) (conj tmp {:type :url-part :value (first parts-left)}))))))

(deftest test-run-one-ok
  (is (= (run-one (partial parse-string-eq  "test")
                  (list (mock-request-http-tokens nil "test") {}))
         (just (list nil {})))))

(deftest test-run-one-nok
  (is (= (run-one (partial parse-string-eq  "test")
                  (list (mock-request-http-tokens nil "other") {}))
         (nothing))))

(deftest test-run-many-ok
  (is (= (run-many (partial parse-string-eq  "test")
                   (list (mock-request-http-tokens nil "test" "test" "other") {}))
         (just (list (mock-request-http-tokens nil "other") {})))))

(deftest test-run-many-nok
  (is (= (run-many (partial parse-string-eq  "test")
                   (list (mock-request-http-tokens :get "test" "test" "other") {}))
         (just (list (mock-request-http-tokens :get "test" "test" "other") {})))))

(deftest test-monadic-http-token-composition-ok-1
  (let [request (list (mock-request-http-tokens :get "google" "com") {:test :a})]
    (is (=
         (do->>= (c_ run-one (partial parse-http-method :get)) (just request)
                 (c_ run-one (partial parse-string-eq "google"))
                 (c_ run-one (partial parse-string-eq "com")))
         (just '(nil {:test :a}))))))

(deftest test-monadic-http-token-composition-nok-1
  (let [request (list (mock-request-http-tokens :get "amazon" "com") {:test :a})]
    (is (=
         (do->>= (c_ run-one (partial parse-http-method :get)) (just request)
                 (c_ run-one (partial parse-string-eq "google"))
                 (c_ run-one (partial parse-string-eq "com")))
         (nothing)))))

(deftest test-monadic-http-token-composition-ok-2
  (let [request (list (mock-request-http-tokens :get "google" "google" "google" "other" "google" "com") {:test :a})]
    (is (=
         (do->>= (c_ run-one (partial parse-http-method :get)) (just request)
                 (c_ run-many (partial parse-string-eq "google"))
                 (c_ run-one (partial parse-string-eq "other"))
                 (c_ run-one (partial parse-string-eq "google"))
                 (c_ run-one (partial parse-string-eq "com")))
         (just '(nil {:test :a}))))))