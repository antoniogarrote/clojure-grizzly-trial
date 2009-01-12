(comment
 "WebIO monad based HTTP request/response processing"
)

;;
;; @ Antonio Garrote Hernández
;;

(ns com.agh.monads.webio
 (:use com.agh.monads)
 (:use com.agh.webserver.rack))


(defstruct web-request-processing :environment ;; The Rack request environment
                                  :response ;; The response of the request
                                  :state) ;; Temporary state of the request

;;
;; WebIO X = WebIOUnfished X |
;;           WebIOFinished X
;;           WebIOtFailed
;;

(defn wrap-request
  "Wrapper around return that builds the new request monad initializing it
   with the values of the rack-request"
  {:monad :WebIO}
  ([rack-request]
    (return :WebIO :Unfinished
           (struct web-request-processing
                  rack-request
                  (create-rack-response)
                  {}))))


(comment

(raise [web-request status msg]
  "Wrapper around return that returns a new RequestUnfinished transformation
   of a RequestUnfinished | RequestFinished monad, preserving its information.
   Only the status and the response are modified"
  (return :WebIO :Failed
    (do (assoc))))
)


;; Request headers

(defn request-header?
  "Checks if a given header is present in the HTTP request"
  ([web-request header]
    (let [header (. header toUpperCase)
          environment (:environment web-request)]
      (not (nil? (get environment header))))))

(defn get-request-header
  "retrieves the content of a header from the request"
    ([web-request header]
    (let [header (. header toUpperCase)
          environment (get web-request :environment)]
      (header environment))))



(use 'clojure.contrib.test-is)

(defn mock-request
  ([environment]
    (struct web-request-processing
      environment
      (create-rack-response)
      {})))

(deftest test-request-header-right?
  (is (=
        (request-header?
          (mock-request {"TEST" :ok})
          "TEST")
        true)))

(deftest test-request-header-right-lowcase?
  (is (=
        (request-header?
          (mock-request {"TEST" :ok})
          "TEst")
        true)))

(deftest test-request-header-wrong?
  (is (=
        (request-header?
          (mock-request {"TEST" :ok})
          "TEST-erroreus")
        false)))

(deftest test-webio-wrapping
  (is (=
        (str (wrap-request {:test 1}))
        (str (struct Monad :WebIO :Unfinished
                  (struct web-request-processing {:test 1} (create-rack-response) {}))))))