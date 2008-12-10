;;
;; Clojure macros for Rack
;;

;;
;; @author Antonio Garrote Hernandez
;;

(ns com.agh.webserver.rack)

(defstruct rack-response :status :headers :body)

(defn create-rack-response
  "Creates a new rack-response struct initialized with 200/OK status, empty
   headers and blank string as body"
  ([] (struct rack-response 200 {} ""))

  ([status headers body] (struct rack-response status headers body)))

;;(defn to-java-rack-response [rack-response]
;;  (proxy [RackResponse] []
;;    (getStatus [] (:status rack-response))
;;    (getHeaders [] (:headers rack-response))
;;    (getBody [] (:body rack-response))
;;    (respond [grizzly-response]

(defn hello-inner[]
  (do
    (println "hello-inner")
    (str "y adios...")))


(defn update-rack-response
  "Updates the rack response with new status, headers and body. The new content
   will be appended to the previous one"
  ([rack-response body]
    (create-rack-response
      (:status rack-response)
      (:headers rack-response)
      (str (:body rack-response) body)))

  ([rack-response body status]
    (create-rack-response
      status
      (:headers rack-response)
      (str (:body rack-response) body)))

  ([rack-response body status headers]
    (create-rack-response
      status
      (merge (:headers rack-response) headers)
      (str (:body rack-response) body))))


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


(defn rack-invokation-point [req function]
  (with-rack-response req function))
