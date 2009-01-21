(comment
  "Main entry point for the server"
)

(ns com.agh.webserver)

(use 'clojure.contrib.test-is)
(use 'com.agh.webserver.framework.logger)

(defn reload-framework []
  (do
    (use :reload 'com.agh.monads)
    (use :reload 'com.agh.monads.maybe)
    (use :reload 'com.agh.monads.pipe)
    (use :reload 'com.agh.monads.state)
    (use :reload 'com.agh.monads.webio)
    (use :reload 'com.agh.utils)
    (use :reload 'com.agh.webserver.framework)
    (use :reload 'com.agh.webserver.framework.logger)
    (use :reload 'com.agh.webserver.rack)
    (use :reload 'com.agh.webserver.framework.router)
    (use :reload 'com.agh.webserver.framework.persistence.rdf)
    (use :reload 'com.agh.webserver.framework.persistence.rdf.vocabularies.xsd)
    (use :reload 'com.agh.webserver.framework.persistence.rdf.vocabularies.rdfs)
    (use :reload 'com.agh.webserver.framework.persistence.rdf.vocabularies.owl)
    (use :reload 'com.agh.webserver.framework.dataformats)
    (use :reload 'com.agh.webserver.framework.dataformats.json)
    (use :reload 'com.agh.webserver.rack)))


;;(defn rack-invokation-point [req function]
;;  (with-rack-response req function))


(defn run-and-log-to
  ([path]
     (do
       (reload-framework)
       (reset-logger-with-file-output :debug path)
       (run-all-tests))))