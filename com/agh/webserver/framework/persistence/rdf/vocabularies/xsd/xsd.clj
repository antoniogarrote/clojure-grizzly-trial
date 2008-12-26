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


(defn literal-string
  "Builds a new literal string"
  ([value]
     (build-literal value))
  ([value lang]
     (build-literal value lang)))

(defn literal-decimal
  "Builds a new literal decimal"
  ([value]
     (build-literal value (xsd-decimal))))

(defn literal-double
  "Builds a new literal double"
  ([value]
     (build-literal value (xsd-double))))

(defn literal-float
  "Builds a new literal float"
  ([value]
     (build-literal value (xsd-float))))


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

(deftest test-literal-string-1
  (is (= (literal-string "test")
         {:value "test" :datatype {:prefix :xsd, :value "string"} :lang ""})))

(deftest test-literal-string-2
  (is (= (literal-string "test" "en-GB")
         {:value "test" :datatype {:prefix :xsd, :value "string"} :lang "en-GB"})))

(deftest test-xsd-float
  (is (= (xsd-float)
         {:prefix :xsd, :value "float"})))

(deftest test-xsd-decimal
  (is (= (xsd-decimal)
         {:prefix :xsd, :value "decimal"})))

(deftest test-xsd-double
  (is (= (xsd-double)
         {:prefix :xsd, :value "double"})))

(deftest test-literal-decimal
  (is (= (literal-decimal 1)
         {:value 1  :lang "" :datatype {:prefix :xsd, :value "decimal"}})))

(deftest test-literal-float
  (is (= (literal-float 1.0)
         {:value 1.0  :lang "" :datatype {:prefix :xsd, :value "float"}})))

(deftest test-literal-double
  (is (= (literal-double 2)
         {:value 2  :lang "" :datatype {:prefix :xsd, :value "double"}})))
