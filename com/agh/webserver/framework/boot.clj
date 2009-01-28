(def *framework-root* (. (new java.io.File ".") getCanonicalPath))

(ns com.agh.webserver.framework.boot)

(use 'com.agh.webserver)
(use 'com.agh.webserver.framework.logger)

(defn boot []
  (do
    (reload-framework)
    (reset-logger-with-file-output :debug (str *framework-root* "/log/server.out"))
    (log :info "framework loaded")

    (log :info "loading application")
    (let [ base-dir (str "file://" *framework-root*)]
      (let [ config-dir (str base-dir "/config/") ]
        (do
          (log :info (str "adding to classpath " config-dir))
          (add-classpath config-dir)))
      (let [ config-dir (str base-dir "/handlers/") ]
        (do
          (log :info (str "adding to classpath " config-dir))
          (add-classpath config-dir)))
      (let [ config-dir (str base-dir "/tbox/") ]
        (do
          (log :info (str "adding to classpath " config-dir))
          (add-classpath config-dir))))

    (log :info "Compiling routes")
    (use  'routing)

    )
)