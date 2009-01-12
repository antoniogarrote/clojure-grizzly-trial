(comment
  "A multithreaded logger"
)

;;
;; @author Antonio Garrote Hernandez
;;

(ns com.agh.webserver.framework.logger)

(use 'com.agh.utils)

(import '(java.io File FileWriter))



(def *logger* (agent {:level 1 :output *out*}))

(defn level-name-to-int
  "Returns a numeric identifier for the levels of the logger"
  ([level]
     (let [levels {:debug 0
                   :info 1
                   :warning 2
                   :error 3
                   :critical 4}]
       (let [to-return (level levels)]
         (if (nil? to-return) 6 to-return)))))

(defn reset-logger
  "Initializes the logger with a given level and output writer"
  ([level writer]
     (send *logger* (fn [a l w] {:level (level-name-to-int l) :output w}) level writer)))

(defn reset-logger-level
  "Initializes the logger with a given level"
  ([level]
     (send *logger* (fn [a l] {:level (level-name-to-int l) :output (:output a)}) level)))


(defn reset-logger-output
  "Initializes the logger with a given level"
  ([writer]
     (send *logger* (fn [a w] {:level (:level a) :output w}) writer)))


(defn log
  "logs something in the logger with the provided level of log"
  ([level to-log]
     (do
       (send *logger* (fn [a level msg]
                        (do
                          (when (<= (:level a)
                                    (level-name-to-int level))
                            (let [to-write (str (. (keyword-to-string level) toUpperCase) " "
                                                (. (new java.util.Date) toString) " => "(str msg) "\n")
                                  writer (:output a)]
                              (do
                                (. writer (write to-write 0 (. to-write length)))
                                (. writer flush))))
                          a))
             level to-log)
       to-log))
  ([level desc to-log]
     (do
       (send *logger* (fn [a level msg]
                        (do
                          (when (<= (:level a)
                                    (level-name-to-int level))
                            (let [to-write (str (. (keyword-to-string level) toUpperCase) " "
                                                (. (new java.util.Date) toString) " => " desc " " (str msg) "\n")
                                  writer (:output a)]
                              (do
                                (. writer (write to-write 0 (. to-write length)))
                                (. writer flush))))
                          a))
             level to-log)
       to-log)))


(defn reset-logger-with-file-output
  "Resets the logger setting the output to the file whose path is provided as an argument
   with the level specified"
  ([level file-path]
     (reset-logger level (new FileWriter (new File file-path) true))))