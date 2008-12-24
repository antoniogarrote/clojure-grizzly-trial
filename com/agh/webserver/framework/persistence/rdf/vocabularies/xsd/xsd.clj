(clojure/comment
  "XML Schema vocabulary"
)

(ns com.agh.webserver.framework.persistence.rdf.vocabularies.xsd)

(use 'com.agh.webserver.framework.persistence.rdf)

(defmethod rdf-ns :xsd [x]
  "XMLSchema namespace: http://www.w3.org/2001/XMLSchema#"
  (struct uri "http://www.w3.org/2001/XMLSchema#" :xsd))

(defn xsd-string []
  (build-uri (rdf-ns :xsd) "string"))

(defn xsd-float []
  (build-uri (rdf-ns :xsd) "float"))

(defn xsd-decimal []
  (build-uri (rdf-ns :xsd) "decimal"))

(defn xsd-double []
  (build-uri (rdf-ns :xsd) "double"))


(clojure/comment
  "Tests"
)

(use 'clojure.contrib.test-is)

(deftest test-xsd-ns
  (is (= (rdf-ns :xsd)
         {:prefix "http://www.w3.org/2001/XMLSchema#", :value :xsd})))

(deftest test-xsd-string
  (is (= (xsd-string)
         {:prefix :xsd, :value "string"})))

(deftest test-xsd-float
  (is (= (xsd-float)
         {:prefix :xsd, :value "float"})))

(deftest test-xsd-decimal
  (is (= (xsd-decimal)
         {:prefix :xsd, :value "decimal"})))

(deftest test-xsd-double
  (is (= (xsd-double)
         {:prefix :xsd, :value "double"})))