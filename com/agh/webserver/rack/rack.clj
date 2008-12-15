;;
;; Clojure macros for Rack
;;

;;
;; @author Antonio Garrote Hernandez
;;

(ns com.agh.webserver.rack)

(use 'com.agh.utils)

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