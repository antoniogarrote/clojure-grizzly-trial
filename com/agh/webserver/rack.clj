;;
;; Clojure macros for Rack
;;

;;
;; @author Antonio Garrote Hernandez
;;

(ns com.agh.webserver.rack)

(use 'com.agh.utils)
(use 'com.agh.webserver.framework.logger)

(defstruct rack-response :status :headers :body)

(defn create-rack-response
  "Creates a new rack-response struct initialized with 200/OK status, empty
   headers and blank string as body"
  ([] (struct rack-response 200 {} (new java.lang.StringBuffer)))

  ([status headers body] (struct rack-response status headers (new java.lang.StringBuffer body))))

;;(defn to-java-rack-response [rack-response]
;;  (proxy [RackResponse] []
;;    (getStatus [] (:status rack-response))
;;    (getHeaders [] (:headers rack-response))
;;    (getBody [] (:body rack-response))
;;    (respond [grizzly-response]

;; ahead definition
;; Body

(defn update-rack-response-body
  "Appends content to the response body"
  ([content rack-response]
    (let [buffer (:body rack-response)]
      (. buffer (append content))
      (:body rack-response))))
;;

(defn update-rack-response
  "Updates the rack response with new status, headers and body. The new content
   will be appended to the previous one"
  ([body rack-response]
    (create-rack-response
      (:status rack-response)
      (:headers rack-response)
      (update-rack-response-body body rack-response)))

  ([body status rack-response]
    (create-rack-response
      status
      (:headers rack-response)
      (update-rack-response-body body rack-response)))

  ([body status headers rack-response]
    (if (and
          (= body nil)
          (= status nil)
          (= headers nil))
      rack-response
    (if (and
          (not (= body nil))
          (= status nil)
          (= headers nil))
      (create-rack-response
        (:status rack-response)
        (:headers rack-response)
        (update-rack-response-body body rack-response))
    (if (and
          (= body nil)
          (not (= status nil))
          (= headers nil))
      (create-rack-response
        status
        (:headers rack-response)
        (:body rack-response))
    (if (and
          (not (= body nil))
          (not (= status nil))
          (= headers nil))
      (create-rack-response
        status
        (:headers rack-response)
        (update-rack-response-body body rack-response))
    (if (and
          (= body nil)
          (= status nil)
          (not (= headers nil)))
      (create-rack-response
        (:status rack-response)
        (merge (:headers rack-response) headers)
        (:body rack-response))
    (if (and
          (not (= body nil))
          (= status nil)
          (not (= headers nil)))
      (create-rack-response
        (:status rack-response)
        (merge (:headers rack-response) headers)
        (update-rack-response-body body rack-response))
    (if (and
          (= body nil)
          (not (= status nil))
          (not (= headers nil)))
      (create-rack-response
        status
        (:headers rack-response)
        (update-rack-response-body body rack-response))
    (if (and
          (not (= body nil))
          (not (= status nil))
          (not (= headers nil)))
      (create-rack-response
        status
        (merge (:headers rack-response) headers)
        (update-rack-response-body body rack-response))))))))))))


;; wrapper functions


;; Status
(defn update-rack-response-status
  "Changes the status of the rack response"
  ([status rack-response]
    (update-rack-response nil status nil rack-response)))

(defn get-rack-response-status
  "Returns the status of the rack response"
  ([rack-response]
    (:status rack-response)))

;; Headers
(defn rack-response-header?
  "Checks if a given header is present in the HTTP rack response"
  ([header rack-response]
    (let [header (. header toUpperCase)
          headers (:headers rack-response)]
      (not (nil? (get headers header))))))

(defn get-rack-response-header
  "retrieves the content of a header from the rack response"
    ([header rack-response]
    (let [header (. header toUpperCase)
          headers (:headers rack-response)]
      (get headers header))))

(defn update-rack-response-header
  "Updates if present or adds a new header in the rack response"
  ([header value rack-response]
    (let [header (. header toUpperCase)]
      (update-rack-response nil nil {header value} rack-response))))

;;    (alter rack-response-ref
;;      create-rack-response
;;      status
;;      (merge (:headers @rack-response-ref) headers)
;;      (str (:body @rack-response-ref) body))))

(defn render
  "Renders a string in a Rack response setting up headers and a certain status
   code."
  ([rack-response-ref to-add status headers]
    (dosync
      (ref-set rack-response-ref (update-rack-response @rack-response-ref to-add status headers)))))


(defmacro with-rack-response
  "Embeds a call to a function with a Rack request and response arguments. The
   function should receive two arguments -> the rack request and the rack
   response. Both arguments are references so must be updated in dosync form"
  [rack-request invoked-function]
  `(let [req# (ref ~rack-request)
         res# (ref (create-rack-response))]
     (do
       ((find-var (symbol (str "cgi/" ~invoked-function))) req# res#)
       @res#)))


(defn rack-invokation-point
  "Main entry point for a request to the framework"
  ([rack-request path]
    (do (log :info (str "Starting request with params \n " rack-request " , " path))
        (log :info (str "KEYS -> \n" (keys rack-request)))
        (loop [ks (keys rack-request)]
          (if (not (nil? ks))
            (do
               (log :info (str " " (first ks) " -> " (class (get rack-request (first ks)))))
               (recur (rest ks)))))
        (+ 1 1)
        (+ 2 1)
      {:response "test"})))


(defn test-update-rack-response []
  (let [response (ref (create-rack-response))]
      (render response "test" 201 {:Content-type "text/html"})))

;;
;; tests
;;

(use 'clojure.contrib.test-is)

(defn test-serialize-rack-response
  [rack-response]
  (list (:status rack-response) (:headers rack-response) (. (:body rack-response) toString)))

(defn test-rack-request
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

;; headers wrappers
(deftest test-update-rack-response-header-func
  (is (= (test-serialize-rack-response
           (update-rack-response-header "tEsT" 12 (create-rack-response 200 {"FALSE" true} "test")))
        (test-serialize-rack-response
          (create-rack-response 200 {"FALSE" true "TEST" 12} "test")))))

(deftest test-update-rack-response-header-func-existing
  (is (= (test-serialize-rack-response
           (update-rack-response-header "false" false (create-rack-response 200 {"FALSE" true} "test")))
        (test-serialize-rack-response
          (create-rack-response 200 {"FALSE" false} "test")))))

(deftest test-rack-response-header-ask-func-neg
  (is (= (rack-response-header? "tEsT" (create-rack-response 200 {"FALSE" true} "test"))
        false)))

(deftest test-rack-response-header-ask-func
  (is (= (rack-response-header? "TeSt" (create-rack-response 200 {"TEST" true} "test"))
        true)))

(deftest test-get-rack-response-header-func
  (is (= (get-rack-response-header "tEsT" (create-rack-response 200 {"TEST" true} "test"))
        true)))


;; status wrappers
(deftest test-get-rack-response-status-func
  (is (= (get-rack-response-status (create-rack-response 200 {:test true} "test"))
        200)))

(deftest test-update-rack-response-status-func
  (is (= (test-serialize-rack-response
           (update-rack-response-status 404 (create-rack-response 200 {:test true} "test")))
         (test-serialize-rack-response
          (create-rack-response 404 {:test true} "test")))))


;; update functions
(deftest test-update-rack-response-status
  (is (= (test-serialize-rack-response
           (update-rack-response nil 404 nil (create-rack-response 200 {:test true} "test")))
        (test-serialize-rack-response
          (create-rack-response 404 {:test true} "test")))))

(deftest test-update-rack-response-body
  (is (= (test-serialize-rack-response
          (update-rack-response "hello" nil nil (create-rack-response 200 {:test true} "say ")))
         (test-serialize-rack-response
           (create-rack-response 200 {:test true} "say hello")))))

(deftest test-update-rack-response-headers
  (is (= (test-serialize-rack-response
           (update-rack-response nil nil {:test false} (create-rack-response 200 {:test true} "test")))
        (test-serialize-rack-response
          (create-rack-response 200 {:test false} "test")))))

(deftest test-update-rack-response-headers-add
  (is (= (test-serialize-rack-response
           (update-rack-response nil nil {:other true} (create-rack-response 200 {:test true} "test")))
        (test-serialize-rack-response
          (create-rack-response 200 {:test true :other true} "test")))))

(deftest test-update-rack-response-update-all
  (is (= (test-serialize-rack-response
           (update-rack-response " ok" 404 {:other true} (create-rack-response 200 {:test true} "test")))
        (test-serialize-rack-response
          (create-rack-response 404 {:test true :other true} "test ok")))))