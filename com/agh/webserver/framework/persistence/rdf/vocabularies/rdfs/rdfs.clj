(clojure/comment
  "RDF Schema vocabulary"
)

(ns com.agh.webserver.framework.persistence.rdf.vocabularies.rdfs)

(use 'com.agh.webserver.framework.persistence.rdf)

(defmethod rdf-ns :rdfs [x]
  "XMLSchema namespace: http://www.w3.org/2000/01/rdf-schema#"
  (struct uri "http://www.w3.org/2000/01/rdf-schema#" :rdfs))


(defn rdfs-subClassOf []
  (build-uri (rdf-ns :rdfs) "subClassOf"))

(defn rdfs-range []
  (build-uri (rdf-ns :rdfs) "range"))

(defn rdfs-domain []
  (build-uri (rdf-ns :rdfs) "domain"))


(clojure/comment
  "Tests"
)

(use 'clojure.contrib.test-is)

(deftest test-rdfs-ns
  (is (= (rdf-ns :rdfs)
         {:prefix "http://www.w3.org/2000/01/rdf-schema#", :value :rdfs})))

(deftest test-rdfs-subClassOf
  (is (= (rdfs-subClassOf)
         {:prefix :rdfs, :value "subClassOf"})))

(deftest test-rdfs-range
  (is (= (rdfs-range)
         {:prefix :rdfs, :value "range"})))

(deftest test-rdfs-domain
  (is (= (rdfs-domain)
         {:prefix :rdfs, :value "domain"})))