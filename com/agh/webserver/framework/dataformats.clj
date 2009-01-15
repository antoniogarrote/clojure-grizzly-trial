(comment
  "Generic functions used in the serialization of differente data formats"
)

;;
;; @author Antonio Garrote Hernandez
;;

(ns com.agh.webserver.framework.dataformats)

(use 'clojure.contrib.test-is)
(use 'com.agh.utils)

(defmulti data-format (fn [format obj dst] format))
(defmulti mime-type identity)
(defmulti provides identity)


(defmethod mime-type :default
  ([mime] (throw (Exception. (str "We don't have a registered MIME type for " mime)))))

(defmethod provides :default
  ([mime] false))

(defn dispatch-by-meta
  "Tries to dispatch the object using a function and all the meta data of the object"
  ([object function & args]
     (let [metadata (meta object)]
       (if (nil? metadata)
         false
         (loop [keys (keys metadata)]
           (if (nil? keys)
             false
             (let [ result (apply function (conj
                                           (conj args object)
                                           (get metadata (first keys))))]
               (if (= result false)
                 (recur (rest keys))
                 result))))))))

;; writing the format to a string or a writer
(defmulti write-format-to (fn [dst to-write] (class dst)))

(defmethod write-format-to java.lang.String
  ([dst to-write] (str dst to-write)))

(defmethod write-format-to java.io.Writer
  ([dst to-write] (. dst (write to-write))))


(comment
  "Tests"
)


(deftest test-write-format-to-1
  (is (= (write-format-to "" "test")
         "test")))

(deftest test-write-format-to-2
  (let [writer (new java.io.StringWriter)]
    (do (write-format-to writer "test")
        (is (= (do (. writer (flush))
                   (. writer(toString)))
               "test")))))