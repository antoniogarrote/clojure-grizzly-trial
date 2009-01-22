(comment
 "Core framework functions"
)

;;
;; @ Antonio Garrote Hernandez
;;

(ns com.agh.webserver.framework
 (:use com.agh.monads)
 (:use com.agh.webserver.rack)
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
  ([web-io-processing f]
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
                (with-web-io (from-monad (wrap-request
                                          {:test 1}
                                          (create-rack-response)
                                          {:a 1}))
                             (fn [] (do
                                      (render-to *response* "OK" 205 {:Content-type "text/html"})))))]
    (dosync (let [resp (deref (:response result))]
              (do (is (= (:status resp)
                         205))
                  (is (= (:headers resp)
                         {:Content-type "text/html"}))
                  (is (= (. (:body resp) toString)
                         "OK")))))))

(deftest test-with-web-io-2
  (let [result (from-monad
                (with-web-io (from-monad (wrap-request
                                          {:test 1}
                                          (create-rack-response)
                                          {:a 1}))
                             (fn [] (do
                                      (render "OK" 205 {:Content-type "text/html"})))))]
    (dosync (let [resp (deref (:response result))]
              (do (is (= (:status resp)
                         205))
                  (is (= (:headers resp)
                         {:Content-type "text/html"}))
                  (is (= (. (:body resp) toString)
                         "OK")))))))

(deftest test-with-web-io-3
  (let [result (from-monad
                (with-web-io (from-monad (wrap-request
                                          {:test 1}
                                          (create-rack-response)
                                          {:a 1}))
                             (fn [] (if (request-header? :test)
                                      (render (str "OK: " (request-header :test)) 201 {:Content-type "text/html"})
                                      (render "NOK" 404 {:Content-type "text/html"})))))]
    (dosync (let [resp (deref (:response result))]
              (do (is (= (:status resp)
                         201))
                  (is (= (:headers resp)
                         {:Content-type "text/html"}))
                  (is (= (. (:body resp) toString)
                         "OK: 1")))))))

(deftest test-with-web-io-4
  (let [result (from-monad
                (with-web-io (from-monad (wrap-request
                                          {:test 1}
                                          (create-rack-response)
                                          {:a 1}))
                             (fn [] (if (parameter? :a)
                                      (render (str "OK: " (parameter :a)) 201 {:Content-type "text/html"})
                                      (render "NOK" 404 {:Content-type "text/html"})))))]
    (dosync (let [resp (deref (:response result))]
              (do (is (= (:status resp)
                         201))
                  (is (= (:headers resp)
                         {:Content-type "text/html"}))
                  (is (= (. (:body resp) toString)
                         "OK: 1")))))))