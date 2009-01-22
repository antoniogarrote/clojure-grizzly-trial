(comment
  "A mechanism for translating URL requests into the selection of a dispatcher
   function"
)

;;
;; @author Antonio Garrote Hernandez
;;

(ns com.agh.webserver.framework.router)

(use 'com.agh.utils)
(use 'com.agh.monads)
(use 'com.agh.monads.maybe)

(defn parse-request-params
  "Translates a HTTP request params string (k1=v1&k2=v2...) into a params hash"
  ([request-params]
     (reduce
      (fn [h1 h2] (merge h1 h2))
      (map #(hash-map (keyword (first %)) (second %))
           (map #(if (not (= -1 (. % indexOf "=")))
                   (. % split "=")
                   (list % %))
                (. request-params split "&"))))))


(defn tokenize-request-path
  "Translates a request URL minus the params into a sequence of tokens"
  ([request-path]
     (loop [parts (filter #(not= % "") (. request-path split "/"))
            acum []]
       (if (nil? parts)
         acum
         (let [this-part (first parts)]
           (if (not (= -1 (. this-part (indexOf "."))))
             (let [splt-part (. this-part (split "\\."))]
               (recur (rest parts)
                      (conj (conj acum (first splt-part)) (second splt-part))))
             (recur (rest parts)
                    (conj acum this-part))))))))

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
        (fn [ac i] (conj { (keyword (. i toLowerCase)) (get rack-env i)} ac))
        {}
        keys))))


;; Parser tokens building functions

(defn build-http-method-tokens
  "Obtains a http-method token from the rack environment"
  ([rack-env]
     {:type :http-method :value (extract-request-method rack-env)}))

(defn build-url-parts-tokens
  "Transforms the PATH_TRANSLATED environment property into a list of url parts"
  ([rack-env]
     (let [ parts (tokenize-request-path (get rack-env "PATH_TRANSLATED")) ]
       (map (fn [part] {:type :url-part :value part}) parts))))

(defn build-parameters-query-tokens
  "parses the parameters of the request query into a map of items"
  ([rack-env]
     (let [params-map (parse-request-params (get rack-env "QUERY_STRING"))]
       {:type :url-parameters
        :value (reduce-maps-list (map (fn [key] {key (key params-map)}) (keys params-map)))})))

(defn tokenize-rack-request
  "Parses a rack request and builds a list of tokens suitable to be checked
   against a route pattern"
  ([rack-env]
     (let [ m (trace (str "METHOD " (list (build-http-method-tokens rack-env))) (list (build-http-method-tokens rack-env)))
            p (trace (str "PARTS " (build-url-parts-tokens rack-env))(build-url-parts-tokens rack-env))
            q (trace (str "PARAMS " (list (build-parameters-query-tokens rack-env))) (list (build-parameters-query-tokens rack-env))) ]
       (concat m p q))))

;; Route parser


;; lexer
(defn is-token-url-part?
  [token]
  (= (:type token) :url-part))

(defn is-token-http-method?
  [token]
  (= (:type token) :http-method))

(defn is-token-url-parameters?
  [token]
  (= (:type token) :url-parameters))

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
               (if (nil? (rest the-parsers))
                 (just (list (rest tokens)
                             (from-maybe result)))
                 (recur (rest the-parsers)
                        (from-maybe result))))))))))


(defn run-or
  "Accepts a list of parsers and returns the result of the first
   successful parser or nothing if every parser fails"
  {:monad :Maybe}
  ([parsers computation]
     (let [tokens (first computation)
           state (second computation)]
       (if (nil? tokens)
         (nothing)
         (loop [the-parsers parsers]
           (let [the-parser (first the-parsers)
                 result (the-parser (first tokens) state)]
             (if (nothing? result)
               (if (not (= nil (rest the-parsers)))
                 (recur (rest the-parsers))
                 (nothing))
               (just (list (rest tokens) (from-maybe result))))))))))




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
  ([token state] (just state))
  ([name token state] (just state)))

(defn parse-parameter-presence
  "Parses the presence of a variable with any given value
   in the parameters reuest"
  {:monad :Maybe}
  [param-name token state]
  (if (is-token-url-parameters? token)
    (if (not (nil? (param-name (token-value token))))
      (just state)
      (nothing))
    (nothing)))

(defn parse-parameter-value
  "Parses the presence of a variable with any given value
   in the parameters reuest"
  {:monad :Maybe}
  [param-name param-value token state]
  (if (is-token-url-parameters? token)
    (let [value (param-name (token-value token))]
      (if (not (nil? value))
        (if (= value param-value)
          (just state)
          (nothing))
        (nothing)))
    (nothing)))


;; routing macros

(defn parser-factory-inner [t]
  "Inner private macro, doesn't wrap in a combinator."
  (if (= (class t) clojure.lang.Symbol)
    (let [kw (keyword (. (str t) toLowerCase))]
    (c_ parse-http-method kw))
  (if (= (class t) java.lang.String)
    (c_ parse-string-eq t)
  (if (= (class t) clojure.lang.Keyword)
    (c_ parse-variable t)
  (if (= (class t) java.util.regex.Pattern)
    (c_ parse-regex t)
  (if (= (class t) clojure.lang.PersistentList)
    (let [symb  (first t)
          parsers (rest t)]
      (if (= (quote symb) '*)
        (let [nt (first parsers)]
              (c_ run-many (parser-factory-inner nt)))
      (if (= (quote symb) '&&)
        (let [others (map (fn[x] (parser-factory-inner x)) parsers)]
          (c_ run-and others))
      (if (= (quote symb) '||)
        (let [others (map (fn[x] (parser-factory-inner x)) parsers)]
          (c_ run-or others))
        (parse-anything t)))))
    (parse-anything t)))))))

(defn parser-factory-params [t]
  "Builds parsers for the params DSL"
  (do (trace (str "voy a parsear factory para " t)
  (if (= (class t) clojure.lang.Keyword)
    (c_ parse-parameter-presence t)
  (if (= (class t) clojure.lang.PersistentList)
    (let [symb  (first t)
          parsers (rest t)]
      (if (= (str symb) "&&")
        (let [others (map (fn[x] (parser-factory-inner x)) parsers)]
          (c_ run-and others ))
      (if (= (str symb) "||")
        (let [others (map (fn[x] (parser-factory-inner x)) parsers)]
          (c_ run-or  others))
        (c_ parse-parameter-value symb (first parsers)))))
    (c_ run-one (parse-anything t)))))))

(defn parser-factory [t]
  "Looks for a correct parser for simplified token notation:
   - \"string\" -> parser-string-eq \"string\"
   - SYMBOL  -> parser-http-method :symbol
   - :keyword -> parser-variable :keyword
   - #\"regex\" -> parser-regex #\"regex\"
   - (* token) -> (run-many token)
   - (&& tokens) -> (run-and (parser tokens))
   - (|| tokens) -> (run-or (parser tokens))
   - (params params) -> parsers combination for params token"
  (if (= (class t) clojure.lang.Symbol)
    (let [kw (keyword (. (str t) toLowerCase))]
    (c_ run-one (c_ parse-http-method kw)))
  (if (= (class t) java.lang.String)
    (c_ run-one (c_ parse-string-eq t))
  (if (= (class t) clojure.lang.Keyword)
    (c_ run-one (c_ parse-variable t))
  (if (= (class t) java.util.regex.Pattern)
    (c_ run-one (c_ parse-regex t))
  (if (= (class t) clojure.lang.PersistentList)
    (let [symb  (first t)
          parsers (rest t)]
      (if (= (str symb) "params")
        (let [others (map (fn[x] (parser-factory-params x)) parsers)]
          (c_ run-and others ))
      (if (= (str symb) "*")
        (let [nt (first parsers)]
              (c_ run-many (parser-factory-inner nt)))
      (if (= (str symb) "&&")
        (let [others (map (fn[x] (parser-factory-inner x)) parsers)]
          (c_ run-and others ))
      (if (= (str symb) "||")
        (let [others (map (fn[x] (parser-factory-inner x)) parsers)]
          (c_ run-or  others))
        (c_ run-one (parse-anything t)))))))
    (c_ run-one (parse-anything t))))))))

;;(defn apply-if-routed route

(defn check-route [tokens route-and-env]
  "Transforms a routing pattern into a monadic parser
   and applies the parser to certain route"
  (if (nil? tokens)
    (nothing)
    (let [all-combinators (map (fn [s] (parser-factory s)) tokens)
          fst-combinator (first all-combinators)
          rest-combinators (rest all-combinators)]
      (loop [combinators rest-combinators
             tmp (>>= fst-combinator (just route-and-env))]
        (if (nil? combinators)
          tmp
          (recur (rest combinators) (>>= (first combinators) tmp)))))))

;; routing table

(defstruct router-entry
  :pattern ;; a route template to match against the rack requests
  :handler ;; that function that will handle the request if the template matches the rack request
  :before-filters ;; a list of filters to be applied sequentially before the request can be passed to the handler
  :after-filters ;; a list of filters to be applied sequentially after the request has been passed to the handler
)

(def *router-table* (ref []))

(defmacro url-pattern [& tokens]
  `(quote [ ~@tokens ]))

(defn route!
  "Add a route to the router table"
  ([pattern handler]
     (dosync
      (commute *router-table*
               (fn [table pattern handler]
                 (conj table
                       (struct router-entry
                               pattern
                               handler
                               []
                               [])))
               pattern handler))))


(defn check-route-for-rack-request
  "Checks if there is a handler defined for the rack-request"
  ([rack-request environment]
     (let [tokens (tokenize-rack-request rack-request)
           route-and-env (list tokens environment)]
       (dosync
        (loop [routes @*router-table*]
          (if (nil? routes)
            nil
            (let [entry (first routes)
                  result (check-route (:pattern entry) route-and-env)]
              (if (nothing? result)
                (recur (rest routes))
                {:parse-result result :routed entry}))))))))

(comment

(defn main-server-handler
  "Main handler for requests in the server"
  ([rack-request]
     (let [routing-result (try
                           (check-route-for-rack-request rack-request {})
                            (catch Exception ex (nothing))) ]
       (if (nothing? routing-result)
         (trace "NOHTING ROUTING ERROR" "")
         (let [parameters (second (:content (:parse-result routing-result)))
               request (rehash-rack-env rack-request)
               io-monad (wrap-request request (create-rack-response) parameters)
               routing-entry (:routed routing-result)
               functions (concat (:before-filters routing-entry)
                                 (list (:handler routing-entry))
                                 (:after-filters routing-entry))]
)))))

)



(comment
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

(defn test-rack-request-router
  ([]
     { "HTTP_USER_AGENT" "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.5; es-ES; rv:1.9.0.5) Gecko/2008120121 Firefox/3.0.5 XPCOMViewer/1.0a1"
       "PATH_TRANSLATED" "/dasfdasdf.php"
       "CONTENT_TYPE" ""
       "HTTP_ACCEPT_LANGUAGE" "es-es,es;q=0.8,en-us;q=0.5,en;q=0.3"
       "rack.input" "java.nio.channels.Channels$ReadableByteChannelImpl@6761424d"
       "clojure.output.headers" {}
       "HTTP_ACCEPT" "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
       "HTTP_KEEP_ALIVE" "300"
       "HTTP_ACCEPT_ENCODING" "gzip,deflate"
       "SERVER_NAME" ""
       "rack.errors" "java.util.logging.Logger@22480241"
       "REQUEST_METHOD" "GET"
       "SERVER_PORT" "8880"
       "SCRIPT_NAME" ""
       "rack.multithread" true
       "REMOTE_ADDR" ""
       "rack.multiprocess" false
       "REMOTE_HOST" ""
       "HTTP_ACCEPT_CHARSET" "ISO-8859-1,utf-8;q=0.7,*;q=0.7"
       "HTTP_CONNECTION" "keep-alive"
       "HTTP_HOST" "localhost:8880"
       "REMOTE_USER" ""
       "PATH_INFO" "/dasfdasdf.php"
       "QUERY_STRING" "a=1&b=2&c=3"
       "clojure.output.stream" ""
       "rack.version" "0.1"
       "clojure.output.status" "200"
       "rack.url_scheme" "" }))

(defn test-rack-request-router-with-path
  ([path]
     { "HTTP_USER_AGENT" "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.5; es-ES; rv:1.9.0.5) Gecko/2008120121 Firefox/3.0.5 XPCOMViewer/1.0a1"
       "PATH_TRANSLATED" path
       "CONTENT_TYPE" ""
       "HTTP_ACCEPT_LANGUAGE" "es-es,es;q=0.8,en-us;q=0.5,en;q=0.3"
       "rack.input" "java.nio.channels.Channels$ReadableByteChannelImpl@6761424d"
       "clojure.output.headers" {}
       "HTTP_ACCEPT" "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
       "HTTP_KEEP_ALIVE" "300"
       "HTTP_ACCEPT_ENCODING" "gzip,deflate"
       "SERVER_NAME" ""
       "rack.errors" "java.util.logging.Logger@22480241"
       "REQUEST_METHOD" "GET"
       "SERVER_PORT" "8880"
       "SCRIPT_NAME" ""
       "rack.multithread" true
       "REMOTE_ADDR" ""
       "rack.multiprocess" false
       "REMOTE_HOST" ""
       "HTTP_ACCEPT_CHARSET" "ISO-8859-1,utf-8;q=0.7,*;q=0.7"
       "HTTP_CONNECTION" "keep-alive"
       "HTTP_HOST" "localhost:8880"
       "REMOTE_USER" ""
       "PATH_INFO" path
       "QUERY_STRING" "a=1&b=2&c=3"
       "clojure.output.stream" ""
       "rack.version" "0.1"
       "clojure.output.status" "200"
       "rack.url_scheme" "" }))

(deftest test-extract-request-method
  (is (=
       (extract-request-method {"REQUEST_METHOD" "GET"})
       :get)))

(deftest test-rehash-rac-env
  (is (=
       (rehash-rack-env (mock-environment {"HOLA" 1 "OTHER" 2 "teSt" 3}))
       { :hola 1, :other 2, :test 3 })))

(deftest test-parse-request-params-1
  (is (= (parse-request-params "a=1&b=2&c=3")
         '{:a "1" :b "2" :c "3"})))

(deftest test-parse-request-params-1
  (is (= (parse-request-params "a=1&b=2&c=3&tmp")
         {:a "1" :b "2" :c "3" :tmp "tmp"})))

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

(deftest test-parse-parameter-presence
  (is (= (parse-parameter-presence :test {:type :url-parameters :value {:test 1}} {})
         (just {}))))

(deftest test-parse-parameter-presence-nok
  (is (= (parse-parameter-presence :test {:type :url-parameters :value {:test_b 1}} {})
         (nothing))))

(deftest test-parse-parameter-value
  (is (= (parse-parameter-value :test 1 {:type :url-parameters :value {:test 1}} {})
         (just {}))))

(deftest test-parse-parameter-value-nok-1
  (is (= (parse-parameter-value :test 1 {:type :url-parameters :value {:test 2}} {})
         (nothing))))

(deftest test-parse-parameter-value-nok-2
  (is (= (parse-parameter-value :test 1 {:type :url-parameters :value {:test_b 2}} {})
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

(defn mock-request-http-tokens-with-params [method  parts params]
  (let [result (if (nil? method)
                 '()
                 (list {:type :http-method :value method}))]
    (loop [parts-left parts
           tmp result]
      (if (nil? parts-left)
        (loop [params-left params
               tmp-inner {}]
          (if (nil? params-left)
            (reverse (conj tmp {:type :url-parameters :value tmp-inner}))
            (let [p-name (first (first params-left))
                  p-value (second (first params-left))]
              (recur (rest params-left) (conj tmp-inner {p-name p-value})))))
        (recur (rest parts-left) (conj tmp {:type :url-part :value (first parts-left)}))))))

(deftest test-run-one-ok
  (is (= (run-one (c_ parse-string-eq  "test")
                  (list (mock-request-http-tokens nil "test") {}))
         (just (list nil {})))))

(deftest test-run-one-nok
  (is (= (run-one (c_ parse-string-eq  "test")
                  (list (mock-request-http-tokens nil "other") {}))
         (nothing))))

(deftest test-run-many-ok
  (is (= (run-many (c_ parse-string-eq  "test")
                   (list (mock-request-http-tokens nil "test" "test" "other") {}))
         (just (list (mock-request-http-tokens nil "other") {})))))

(deftest test-run-many-nok
  (is (= (run-many (c_ parse-string-eq  "test")
                   (list (mock-request-http-tokens :get "test" "test" "other") {}))
         (just (list (mock-request-http-tokens :get "test" "test" "other") {})))))

(deftest test-monadic-http-token-composition-ok-1
  (let [request (list (mock-request-http-tokens :get "google" "com") {:test :a})]
    (is (=
         (do->>= (c_ run-one (c_ parse-http-method :get)) (just request)
                 (c_ run-one (c_ parse-string-eq "google"))
                 (c_ run-one (c_ parse-string-eq "com")))
         (just '(nil {:test :a}))))))

(deftest test-monadic-http-token-composition-nok-1
  (let [request (list (mock-request-http-tokens :get "amazon" "com") {:test :a})]
    (is (=
         (do->>= (c_ run-one (c_ parse-http-method :get)) (just request)
                 (c_ run-one (c_ parse-string-eq "google"))
                 (c_ run-one (c_ parse-string-eq "com")))
         (nothing)))))

(deftest test-monadic-http-token-composition-ok-2
  (let [request (list (mock-request-http-tokens :get "google" "google" "google" "other" "google" "com") {:test :a})]
    (is (=
         (do->>= (c_ run-one (c_ parse-http-method :get)) (just request)
                 (c_ run-many (c_ parse-string-eq "google"))
                 (c_ run-one (c_ parse-string-eq "other"))
                 (c_ run-one (c_ parse-string-eq "google"))
                 (c_ run-one (c_ parse-string-eq "com")))
         (just '(nil {:test :a}))))))

(deftest test-monadic-composition-and-ok
  (let [request (list (mock-request-http-tokens nil "google" "test") {:test :a})]
    (is (=
         (do->>= (c_ run-and [(c_ parse-string-eq "google")
                              (c_ parse-variable :server)]) (just request)
                 (c_ run-one (c_ parse-string-eq "test")))
         (just '(nil {:test :a :server "google"}))))))

(deftest test-monadic-composition-and-nok
  (let [request (list (mock-request-http-tokens nil "google" "test-fase") {:test :a})]
    (is (=
         (do->>= (c_ run-and [(c_ parse-string-eq "google")
                              (c_ parse-variable :server)]) (just request)
                 (c_ run-one (c_ parse-string-eq "test")))
         (nothing)))))

(deftest test-monadic-composition-or-ok
  (let [request (list (mock-request-http-tokens nil "google" "test") {:test :a})]
    (is (=
         (do->>= (c_ run-or [(c_ parse-string-eq "yahoo")
                             (c_ parse-string-eq "google")]) (just request)
                 (c_ run-one (c_ parse-string-eq "test")))
         (just '(nil {:test :a}))))))

(deftest test-monadic-composition-or-nok
  (let [request (list (mock-request-http-tokens nil "msn" "test-fase") {:test :a})]
    (is (=
         (do->>= (c_ run-or [(c_ parse-string-eq "google")
                             (c_ parse-string-eq "yahoo")
                             (c_ parse-variable :server)]) (just request)
                 (c_ run-one (c_ parse-string-eq "test")))
         (nothing)))))

;; parser factory

(deftest test-parser-factory-http-method
  (is (=
       ((parser-factory 'GET) '(({:type :http-method :value :get}) {}))
       (run-one (c_ parse-http-method :get) '(({:type :http-method :value :get}) {})))))

(deftest test-parser-factory-string
  (is (=
       ((parser-factory "test") '(({:type :url-part :value "test"}) {}))
       (run-one (c_ parse-string-eq "test") '(({:type :url-part :value "test"}) {})))))

(deftest test-parser-factory-symbol
  (is (=
       ((parser-factory :test) '(({:type :url-part :value "23"}) {:a 1}))
       (run-one (c_ parse-variable :test) '(({:type :url-part :value "23"}) {:a 1})))))

(deftest test-parser-factory-regex
  (is (=
       ((parser-factory #"[a-z]+") '(({:type :url-part :value "1111"}) {}))
       (run-one (c_ parse-regex '#"[a-z]+") '(({:type :url-part :value "1111"}) {})))))

(deftest test-parser-many
  (is (=
       ((parser-factory '(* "test")) (list (mock-request-http-tokens nil "test" "test" "other") {}))
       (run-many (c_ parse-string-eq  "test")
                 (list (mock-request-http-tokens nil "test" "test" "other") {})))))

(deftest test-parser-and
  (is (=
       ((parser-factory '(&& "google" :server)) (list (mock-request-http-tokens nil "google" "test") {:test :a}))
       (run-and [(c_ parse-string-eq "google")
                 (c_ parse-variable :server)] (list (mock-request-http-tokens nil "google" "test") {:test :a})))))

(deftest test-parser-or
  (is (=
       ((parser-factory '(|| "msn" "google")) (list (mock-request-http-tokens nil "google" ) {:test :a}))
       (run-or [(c_ parse-string-eq "msn")
                (c_ parse-string-eq "google")] (list (mock-request-http-tokens nil "google") {:test :a})))))

(deftest test-parser-params-1
  (is (=
       ((parser-factory '(params :test)) (list (mock-request-http-tokens-with-params nil nil '((:test 1) (:b 2))) {:env-var :a}))
       '{:monad-type :Maybe, :monad-subtype :Just, :content (nil {:env-var :a})})))

(deftest test-parser-params-2
  (is (=
       ((parser-factory '(params :test :b)) (list (mock-request-http-tokens-with-params nil nil '((:test 1) (:b 2))) {:env-var :a}))
       '{:monad-type :Maybe, :monad-subtype :Just, :content (nil {:env-var :a})})))

(deftest test-parser-params-3
  (is (=
       ((parser-factory '(params :test (:b 2))) (list (mock-request-http-tokens-with-params nil nil '((:test 1) (:b 2))) {:env-var :a}))
       '{:monad-type :Maybe, :monad-subtype :Just, :content (nil {:env-var :a})})))

(deftest test-parser-params-4
  (is (=
       ((parser-factory '(params :test (:b 3))) (list (mock-request-http-tokens-with-params nil nil '((:test 1) (:b 2))) {:env-var :a}))
       '{:monad-type :Maybe, :monad-subtype :Nothing, :content nil})))

(deftest test-parser-params-5
  (is (=
       ((parser-factory '(params :testa (:b 2))) (list (mock-request-http-tokens-with-params nil nil '((:test 1) (:b 2))) {:env-var :a}))
       '{:monad-type :Maybe, :monad-subtype :Nothing, :content nil})))


(deftest test-check-route-1
  (is (=
       (check-route '[GET "google" "es"] (list (mock-request-http-tokens :get "google" "es") {}))
       '{:monad-type :Maybe, :monad-subtype :Just, :content (nil {})})))

(deftest test-check-route-2
  (is (=
       (check-route '[POST "google" "es"] (list (mock-request-http-tokens :get "google" "es") {}))
       '{:monad-type :Maybe, :monad-subtype :Nothing, :content nil})))

(deftest test-check-route-3
  (is (=
       (check-route '[GET "google" (&& "es" :lang)] (list (mock-request-http-tokens :get "google" "es") {}))
       '{:monad-type :Maybe, :monad-subtype :Just, :content (nil {:lang "es"})})))

(deftest test-check-route-4
  (is (=
       (check-route '[(|| GET POST) "google" (&& "es" :lang)] (list (mock-request-http-tokens :get "google" "es") {}))
       '{:monad-type :Maybe, :monad-subtype :Just, :content (nil {:lang "es"})})))

(deftest test-check-route-5
  (is (=
       (check-route '[(|| GET POST) "google" (&& "es" :lang)] (list (mock-request-http-tokens :delete "google" "es") {}))
       '{:monad-type :Maybe, :monad-subtype :Nothing, :content nil})))

(deftest test-check-route-6
  (is (=
       (check-route '[(|| GET POST) "google" (&& "es" :lang) (params :test)] (list (mock-request-http-tokens-with-params :get '("google" "es") '((:test 1) (:b 2))) {}))
       '{:monad-type :Maybe, :monad-subtype :Just, :content (nil {:lang "es"})})))

(deftest test-check-route-7
  (is (=
       (check-route '[(|| GET POST) "google" (&& "es" :lang) (params :test_b)] (list (mock-request-http-tokens-with-params :get '("google" "es") '((:test 1) (:b 2))) {}))
       '{:monad-type :Maybe, :monad-subtype :Nothing, :content nil})))

(deftest test-rehash-environment-1
  (is (=
       (rehash-rack-env (test-rack-request-router))
       {:request_method "GET", :http_host "localhost:8880", :http_accept_language "es-es,es;q=0.8,en-us;q=0.5,en;q=0.3", :rack.multithread true, :http_accept "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8", :remote_user "", :server_port "8880", :remote_addr "", :rack.url_scheme "", :remote_host "", :http_connection "keep-alive", :path_info "/dasfdasdf.php", :script_name "", :query_string "a=1&b=2&c=3", :rack.multiprocess false, :clojure.output.headers {}, :clojure.output.status "200", :http_accept_encoding "gzip,deflate", :content_type "", :rack.errors "java.util.logging.Logger@22480241", :http_user_agent "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.5; es-ES; rv:1.9.0.5) Gecko/2008120121 Firefox/3.0.5 XPCOMViewer/1.0a1", :http_keep_alive "300", :clojure.output.stream "", :path_translated "/dasfdasdf.php", :http_accept_charset "ISO-8859-1,utf-8;q=0.7,*;q=0.7", :rack.version "0.1", :rack.input "java.nio.channels.Channels$ReadableByteChannelImpl@6761424d", :server_name ""})))

(deftest test-route-1
  (is (=
       (route!
        (url-pattern GET "google" "com")
        identity)
       [{ :pattern '[GET "google" "com"] :handler identity :before-filters [] :after-filters [] }])))

(deftest test-build-http-method-token-1
  (is (= (build-http-method-tokens (test-rack-request-router))
         {:type :http-method :value :get})))

(deftest test-build-url-parts-tokens
  (is (= (build-url-parts-tokens (test-rack-request-router-with-path "/path/to/request.php"))
         [{:value "path", :type :url-part}
          {:value "to", :type :url-part}
          {:value "request", :type :url-part}
          {:value "php", :type :url-part}])))

(deftest test-build-parameters-query-tokens
  (is (= (build-parameters-query-tokens (test-rack-request-router))
         {:value {:c "3", :a "1", :b "2"}, :type :url-parameters})))

(deftest tokenize-rack-request
  (let [ result (tokenize-rack-request (test-rack-request-router)) ]
    (do
      (is (= (set result)
             (set '({:type :http-method, :value :get} {:type :url-part, :value "dasfdasdf"} {:type :url-part, :value "php"} {:type :url-parameters, :value {:b "2", :c "3",:a "1"}}))))
      (is (= (nth result 0)
             '{:type :http-method, :value :get}))
      (is (= (nth result 1)
             '{:type :url-part, :value "dasfdasdf"}))
      (is (= (nth result 2)
             '{:type :url-part, :value "php"})))))


(defn test-clear-routes-table!
  ([] (dosync
       (commute *router-table*
                (fn [table]
                  [])))))


(deftest test-check-route-for-rack-request-1
  (do (test-clear-routes-table!)
      (route! (url-pattern GET "dasfdasdf" "php") identity)
        (is (= (:parse-result (check-route-for-rack-request (test-rack-request-router) {}))
               '{:monad-type :Maybe
                 :monad-subtype :Just
                 :content (({:value {:c "3", :b "2", :a "1"}, :type :url-parameters}) {})}))))