(comment
  "A mechanism for translating URL requests into the selection of a dispatch
   function"
)

;;
;; @author Antonio Garrote Hernández
;;

(ns com.agh.webserver.framework.router)

(defn parse-request-params [request-params]
  "Translates a HTTP request params string (k1=v1&k2=v2...) into a params hash"
  (reduce
    (fn [h1 h2] (merge h1 h2))
    (map #(hash-map (keyword (first %)) (second %))
      (map #(. % split "=")
        (. request-params split "&")))))


(defn tokenize-request-path [request-path]
  "Translates a request URL minus the params into a sequence of tokens"
  (filter #(not= % "") (. request-path split "/")))


