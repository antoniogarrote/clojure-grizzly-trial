(ns tests)

(use 'com.agh.utils)
(use 'com.agh.cljhaml)
(use 'com.agh.webserver.framework)
(use 'com.agh.webserver.framework.router)
(use 'com.agh.webserver.framework.logger)


(defn say-hello
  ([] (do (log :info "DENTRO")
          (if (= (parameter :name) "helena")
            (render (with-haml-template (str *framework-root* "/views/hello") :html [name "chica guapa"])
                    200
                    {:Content-type "text/html"})
            (render (with-haml-template (str *framework-root* "/views/hello") :html [name (parameter :name)])
                    200
                    {:Content-type "text/html"}))
          (log :info "ADIOS"))))