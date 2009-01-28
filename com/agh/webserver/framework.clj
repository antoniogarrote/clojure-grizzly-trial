(comment
 "Core framework functions"
)

;;
;; @ Antonio Garrote Hernandez
;;

(ns com.agh.webserver.framework
 (:use com.agh.monads)
 (:use com.agh.monads.maybe)
 (:use com.agh.webserver.rack)
 (:use com.agh.webserver.framework.logger)
 (:use com.agh.webserver.framework.router)
 (:use com.agh.utils)
 (:use com.agh.monads.webio))

;; ahead definitions

(def *request* "")
(def *response* "")
(def *parameters* "")


;; Request headers

(defn request-header?
  "Checks if a given header is present in the HTTP request"
  ([web-request header-or-key]
    (let [header (. (if (string? header-or-key) header-or-key (keyword-to-string header-or-key)) toUpperCase)]
      (not (and (nil? (get web-request (keyword header-or-key))) (nil? (get web-request header))))))
  ([header-or-key]
     (request-header? *request* header-or-key)))

(defn request-header
  "retrieves the content of a header from the request"
    ([web-request header-or-key]
       (let [header (. (if (string? header-or-key) header-or-key (keyword-to-string header-or-key)) toUpperCase)]
         (let [resp-cased (get web-request header)]
           (if (nil? resp-cased)
             (get web-request (keyword header-or-key))
             resp-cased))))
    ([header-or-key]
       (request-header *request* header-or-key)))

(defn parameter?
  "checks if a given parameter is present in the request"
  ([parameters name-or-key]
     (not (nil? (get parameters (keyword name-or-key)))))
  ([name-or-key]
     (parameter? *parameters* name-or-key)))

(defn parameter
  "retrieves the value of the parameter if it is present in the request"
  ([parameters name-or-key]
     (get parameters (keyword name-or-key)))
  ([name-or-key]
     (parameter *parameters* name-or-key)))

(defn render-to
  "Renders a string in a Rack response setting up headers and a certain status
   code."
  ([rack-response-ref to-add status headers]
    (dosync
      (ref-set rack-response-ref (update-rack-response to-add status headers @rack-response-ref))))
  ([rack-response-ref to-add status]
     (dosync
      (ref-set rack-response-ref (update-rack-response to-add status @rack-response-ref))))
  ([rack-response-ref to-add]
     (dosync
      (ref-set rack-response-ref (update-rack-response to-add  @rack-response-ref)))))

(defn render
  "Renders using the function render-to to the locally bindind response *response*"
  ([to-add status headers]
     (render-to *response* to-add status headers))
  ([to-add status]
     (render-to *response* to-add status))
  ([to-add]
     (render-to *response* to-add)))



;; TODO ->  do-web-io -> check-route -> prepare rack request -> wrap-rack-request -> LAUNCH PROCESSING
;; (let [params# (second (:content (:parse-result parse-result)))
(defn raise
  "Wrapper around return that returns a new RequestUnfinished transformation
   of a RequestUnfinished | RequestFinished monad, preserving its information.
   Only the status and the response are modified"
  ([request status msg]
     (wrap-request request (create-rack-response status {} msg) {} :Failed)))

(defn with-web-io
  "Lifts a call to a function for a web-io-processing struct into the web-io monad"
  {:monad :WebIO}
  ([f web-io-processing]
     (try
      (let [result (binding [*request* (:environment web-io-processing)
                             *response* (:response web-io-processing)
                             *parameters* (:parameters web-io-processing)]
                     (apply f []))]
        (if (web-io-monad? result)
          result
          (wrap-request (:environment web-io-processing)
                        (:response web-io-processing)
                        (:parameters web-io-processing)
                        :Unfinished)))
      (catch Exception ex (raise (:environment web-io-processing)
                                 500
                                 (. ex getMessage))))))

(defn main-server-handler
  "Main handler for requests in the server"
  ([rack-request]
     (let [routing-result (try
                           (check-route-for-rack-request rack-request {})
                            (catch Exception ex (do
                                                  (log :error (str "EXCEPTION EX: " (. ex getMessage)))
                                                  (nothing))))]
       (if (nothing? routing-result)
         (let [clojure-output (get rack-request "clojure.output.stream")]
           (do (. clojure-output (write "Internal server error"))
               (. rack-request (put "clojure.output.status" 500))
               (. rack-request (put "clojure.output.headers" (new java.util.HashMap)))
               (. rack-request (put "clojure.output.stream" clojure-output))
               rack-request))
         (let [parameters (second (:content (:parse-result routing-result)))
               request (rehash-rack-env rack-request)
               io-monad (wrap-request request (create-rack-response) parameters)
               routing-entry (:routed routing-result)
               functions (concat (:before-filters routing-entry)
                                 (list (:handler routing-entry))
                                 (:after-filters routing-entry))
               result (loop [rest-to-lift functions
                             tmp io-monad]
                        (if (nil? rest-to-lift)
                          tmp
                          (recur (rest rest-to-lift)
                                 (>>= (c_ with-web-io (first rest-to-lift)) tmp))))]
           (if (web-io-failed? result)
             (let [clojure-output (get rack-request "clojure.output.stream")]
               (do (. clojure-output (write "Internal server error"))
                   (. rack-request (put "clojure.output.status" 500))
                   (. rack-request (put "clojure.output.headers" (new java.util.HashMap)))
                   (. rack-request (put "clojure.output.stream" clojure-output))
                   rack-request))
             (let [clojure-output (get rack-request "clojure.output.stream")
                   status (dosync (:status (deref (:response (:content result)))))
                   headers (dosync (:headers (deref (:response (:content result)))))]
               (dosync
                 (. clojure-output (append (. (:body (deref (:response (:content result)))) toString)))
                 (. rack-request (put "clojure.output.status" status))
                 (. rack-request (put "clojure.output.headers" (assoc-map-to-hash-map headers)))
                 (. rack-request (put "clojure.output.stream" clojure-output))
                 rack-request))))))))


(defn rack-invokation-point
  "Main entry point for a request to the framework"
  ([rack-request path]
     (do (log :info (str "\n\nStarting request with params: \n " rack-request " , " path))
         (let [start-time (. java.lang.System currentTimeMillis)
               result (main-server-handler rack-request)
               end-time (. java.lang.System currentTimeMillis)]
           (do (log :info (str "Request processed in " (- end-time start-time) " milliseconds"))
               result)))))

(comment
  "Tests"
)

(use 'clojure.contrib.test-is)

(defn mock-request
  ([environment]
    (struct web-request-processing
      environment
      (create-rack-response))))

(deftest test-request-header-right?
  (is (=
        (request-header?
         (:environment
          (mock-request {"TEST" :ok}))
          "TEST")
        true)))

(deftest test-request-header-right-lowcase?
  (is (=
        (request-header?
          (:environment
           (mock-request {"TEST" :ok}))
           "TEst")
        true)))

(deftest test-request-header-wrong?
  (is (=
        (request-header?
          (mock-request {"TEST" :ok})
          "TEST-erroreus")
        false)))

(deftest test-has-parameter-1
  (do
    (is (= (parameter? {:a 1} :a)
           true))
    (is (= (parameter? {:a 1} "a")
           true))))

(deftest test-parameter-1
  (do
    (is (= (parameter {:a 1} :a)
           1))
    (is (= (parameter {:a 1} "a")
           1))))

(deftest test-update-rack-response-1
  (let [response (ref (create-rack-response))
        updated-response (render-to response "test" 201 {:Content-type "text/html"})]
    (do
      (is (= (dosync (:status updated-response))
             201))
      (is (= (dosync (:headers updated-response))
             {:Content-type "text/html"}))
      (is (= (dosync (. (:body updated-response) toString))
             "test")))))

(deftest test-update-rack-response-2
  (let [response (ref (create-rack-response))
        updated-response (render-to response "test" 201)]
    (do
      (is (= (dosync (:status updated-response))
             201))
      (is (= (dosync (:headers updated-response))
             {}))
      (is (= (dosync (. (:body updated-response) toString))
             "test")))))


(deftest test-update-rack-response-3
  (let [response (ref (create-rack-response))
        updated-response (render-to response "test")]
    (do
      (is (= (dosync (:status updated-response))
             200))
      (is (= (dosync (:headers updated-response))
             {}))
      (is (= (dosync (. (:body updated-response) toString))
             "test")))))

(deftest test-with-web-io-1
  (let [result (from-monad
                (with-web-io (fn [] (do
                                      (render-to *response* "OK" 205 {:Content-type "text/html"})))
                             (from-monad (wrap-request
                                          {:test 1}
                                          (create-rack-response)
                                          {:a 1}))))]
    (dosync (let [resp (deref (:response result))]
              (do (is (= (:status resp)
                         205))
                  (is (= (:headers resp)
                         {:Content-type "text/html"}))
                  (is (= (. (:body resp) toString)
                         "OK")))))))

(deftest test-with-web-io-2
  (let [result (from-monad
                (with-web-io (fn [] (do
                                      (render "OK" 205 {:Content-type "text/html"})))
                             (from-monad (wrap-request
                                          {:test 1}
                                          (create-rack-response)
                                          {:a 1}))))]
    (dosync (let [resp (deref (:response result))]
              (do (is (= (:status resp)
                         205))
                  (is (= (:headers resp)
                         {:Content-type "text/html"}))
                  (is (= (. (:body resp) toString)
                         "OK")))))))

(deftest test-with-web-io-3
  (let [result (from-monad
                (with-web-io (fn [] (if (request-header? :test)
                                      (render (str "OK: " (request-header :test)) 201 {:Content-type "text/html"})
                                      (render "NOK" 404 {:Content-type "text/html"})))
                             (from-monad (wrap-request
                                          {:test 1}
                                          (create-rack-response)
                                          {:a 1}))))]
    (dosync (let [resp (deref (:response result))]
              (do (is (= (:status resp)
                         201))
                  (is (= (:headers resp)
                         {:Content-type "text/html"}))
                  (is (= (. (:body resp) toString)
                         "OK: 1")))))))

(deftest test-with-web-io-4
  (let [result (from-monad
                (with-web-io (fn [] (if (parameter? :a)
                                      (render (str "OK: " (parameter :a)) 201 {:Content-type "text/html"})
                                      (render "NOK" 404 {:Content-type "text/html"})))
                             (from-monad (wrap-request
                                          {:test 1}
                                          (create-rack-response)
                                          {:a 1}))))]
    (dosync (let [resp (deref (:response result))]
              (do (is (= (:status resp)
                         201))
                  (is (= (:headers resp)
                         {:Content-type "text/html"}))
                  (is (= (. (:body resp) toString)
                         "OK: 1")))))))

(defn test-rack-request-router-with-path-and-output-stream
  ([path]
     { "HTTP_USER_AGENT" "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.5; es-ES; rv:1.9.0.5) Gecko/2008120121 Firefox/3.0.5 XPCOMViewer/1.0a1"
       "PATH_TRANSLATED" path
       "CONTENT_TYPE" ""
       "HTTP_ACCEPT_LANGUAGE" "es-es,es;q=0.8,en-us;q=0.5,en;q=0.3"
       "rack.input" "java.nio.channels.Channels$ReadableByteChannelImpl@6761424d"
       "clojure.output.headers" (new java.util.HashMap)
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
       "clojure.output.stream" (new java.io.StringWriter)
       "rack.version" "0.1"
       "clojure.output.status" "200"
       "rack.url_scheme" "" }))



;;(deftest test-main-handler-1
;;  (do (test-clear-routes-table!)
;;      (route! (url-pattern GET "greetings" :name)
;;              (fn [] (do (render (str "Hello " (parameter :name)) 200))))
;;      (is (= (main-server-handler (test-rack-request-router-with-path-and-output-stream "/greetings/Helena"))
;;             "test"))))