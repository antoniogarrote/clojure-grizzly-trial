(clojure/comment
)

(ns com.agh.webserver)

(defn reload-framework []
  (do
    (use :reload 'com.agh.monads)
    (use :reload 'com.agh.monads.maybe)
    (use :reload 'com.agh.monads.state)
    (use :reload 'com.agh.monads.webio)
    (use :reload 'com.agh.utils)
    (use :reload 'com.agh.webserver.framework.router)
    (use :reload 'com.agh.webserver.rack)))

(use :reload 'com.agh.webserver.rack)

(defn rack-invokation-point [req function]
  (with-rack-response req function))


