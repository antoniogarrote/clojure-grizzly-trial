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