(comment
 "WebIO monad based HTTP request/response processing"
)

;;
;; @ Antonio Garrote Hernandez
;;

(ns com.agh.monads.webio
 (:use com.agh.monads)
 (:use com.agh.webserver.rack)
 (:use com.agh.utils))


(defstruct web-request-processing :environment ;; The Rack request
                                  :response ;; The response of the request
                                  :parameters) ;; the parameters hash


;;
;; WebIO X = WebIOUnfished X |
;;           WebIOFinished X
;;           WebIOtFailed
;;

(defn wrap-request
  "Wrapper around return that builds the new request monad initializing it
   with the values of the rack-request"
  {:monad :WebIO}
  ([rack-request rack-response parameters subtype]
     (return :WebIO subtype
           (struct web-request-processing
                   rack-request
                   (if (= (class rack-response) clojure.lang.Ref)
                     rack-response
                     (ref rack-response))
                   parameters)))
  ([rack-request]
    (wrap-request rack-request (create-rack-response) {} :Unfinished))
  ([rack-request rack-response]
    (wrap-request rack-request rack-response {} :Unfinished))
  ([rack-request rack-response parameters]
    (wrap-request rack-request rack-response parameters :Unfinished)))


(defn web-io-monad?
  "Checks if the given object is a web-io monad"
  ([object] (and (monad? object)
                 (= (monad-type object)
                    :WebIO))))

(defn web-io-failed?
  ([web-io-monad]
     (= (monad-subtype web-io-monad)
        :Failed)))

(defn web-io-finished?
  ([web-io-monad]
     (= (monad-subtype web-io-monad)
        :Finished)))

(defn web-io-unfinished?
  ([web-io-monad]
     (= (monad-subtype web-io-monad)
        :Unfinished)))

(defmethod >>= :WebIO [f m]
  "Instance of the >>= function for the WebIO monad. If the computation
   result of m is WebIO Unfinished, the next function f is applied to the
   content of the m. If the result of m is WebIO Failed, next function
   f is not applied and m is returned. If the result of m is WebIO
   Finished, f is not invoked"
  (if (or (web-io-failed? m) (web-io-finished? m))
    m
    (f (:content m))))


(comment
  "Tests"
)

(use 'clojure.contrib.test-is)

(deftest test-webio-wrapping
  (let [request (wrap-request {:test 1})
        ref (:response (:content request))]
    (is (=
         (str request)
         (str "{:monad-type :WebIO, :monad-subtype :Unfinished, :content {:environment {:test 1}, :response #<Ref " ref ">, :parameters {}}}")))))

(deftest test-web-io-failed-1
  (is (=
       (web-io-failed? (wrap-request "test" "test" "test" :Failed))
       true)))

(deftest test-web-io-finished-1
  (is (=
       (web-io-finished? (wrap-request "test" "test" "test" :Finished))
       true)))

(deftest test-web-io-unfinished-1
  (is (=
       (web-io-unfinished? (wrap-request "test" "test" "test" :Unfinished))
       true)))