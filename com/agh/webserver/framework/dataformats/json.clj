(comment
  "Formatting of Clojure objects into JSON"
)

;;
;;  @author Antonio Garrote Hernandez
;;

(ns com.agh.webserver.framework.dataformats.json)

(use 'clojure.contrib.test-is)
(use 'org.danlarkin.json)
(use 'com.agh.utils)
(use 'com.agh.webserver.framework.dataformats)
(use 'com.agh.webserver.framework.persistence.rdf)
(use 'com.agh.webserver.framework.persistence.rdf.vocabularies.owl)
(use 'com.agh.webserver.framework.persistence.rdf.vocabularies.xsd)


;; we register the MIME type

(defmulti to-json (fn [tag object dst] tag))

(defmethod provides :json
  ([mime] true))

(defmethod mime-type :json
  ([mime] "application/json"))

(defmethod data-format :json
  ([format obj dst]
     (let [result (dispatch-by-meta obj to-json dst)]
       (if (false? result)
         (to-json (class obj) obj dst)
         result))))

(defmethod to-json :default
  ([class object dst]
     (write-format-to dst (encode-to-str object))))

(defmethod to-json :owl-individual
  ([class object dst]
     (write-format-to dst (encode-to-str (owl-individual-to-properties-map object)))))

;; serialization of different objects

(comment
  "Tests"
)

(deftest test-provides-json
  (is (= (provides :json)
         true)))

(deftest test-mime-type-json
  (is (= (mime-type :json)
         "application/json")))


(deftest data-format-1
  (is (= (data-format :json 1 "")
         (str 1))))

(deftest test-data-format-owl-individual-1
  (let [repo (init-memory-repository!)
        graph (describe-tbox
               (describe-owl-datatype-property "http://test.com/prop_a" (xsd-decimal))
               (describe-owl-datatype-property "http://test.com/prop_b" (xsd-string))
               (describe-owl-datatype-property "http://test.com/prop_c" (xsd-decimal))
               (describe-owl-class "http://test.com/class_a")
               (describe-owl-class "http://test.com/class_b")
               (describe-owl-class "http://test.com/class_c")
               (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_b"))]
    (do (tbox-clear!)
        (repositories-registry-clear!)
        (connections-clear!)
        (register-repository! :test repo)
        (write-graph-in-repository! graph (connection! :test))
        (tbox-register-class! :owl-thing (owl-Thing) :test)
        (tbox-register-class! :class_a "http://test.com/class_a" :test)
        (tbox-register-class! :class_b "http://test.com/class_b" :test)
        (tbox-register-class! :class_c "http://test.com/class_c" :test)
        (tbox-register-datatype-property! :prop_a "http://test.com/prop_a" :test)
        (tbox-register-datatype-property! :prop_b "http://test.com/prop_b" :test)
        (tbox-register-object-property! :prop_c "http://test.com/prop_c" :test)
        (abox-create-individual! :class_a "http://test.com/individuals#" {:prop_a 15, :prop_b "hola"} (connection! :test))
        (abox-create-individual! :class_a "http://test.com/individuals#" {:prop_a 95, :prop_b "adios"} (connection! :test))
        (abox-create-individual! :class_c "http://test.com/individuals#" {:prop_c 35} (connection! :test))
        (let [result (abox-find-individual-uris!
                      (not-instance-of-classes :class_a)
                      (connection! :test))
              the-individual (abox-build-individual-from-uri!
                                 (first result)
                                 (connection! :test))]
            (is (= (data-format :json
                                the-individual
                                "")
                   (str "{\"tag\":\"owl-individual\",\"uri\":\"" (uri-to-string (:uri the-individual))"\",\"prop_c\":\"35\"}")))))))