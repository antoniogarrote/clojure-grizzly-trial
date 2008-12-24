(comment
  "Functions for enabling RDF serialized persistence for data"
)

;;
;; @author Antonio Garrote Hernandez
;;

(ns com.agh.webserver.framework.persistence.rdf)

(import '(org.openrdf.repository Repository)
        '(org.openrdf.repository.sail SailRepository)
        '(org.openrdf.sail.rdbms RdbmsStore)
        '(org.slf4j LoggerFactory))

(use 'com.agh.webserver.framework.persistence.rdf.vocabularies.xsd)

;; (init-repository "com.mysql.jdbc.Driver" "jdbc:mysql://localhost:3306/clojure_sesame" "root" "root")
(defn init-repository
  "Instantiates a new repository connection and stores it in
   a clojure ref"
  ([driver url login password]
     (do (def *rdf-repository* (new SailRepository (new RdbmsStore driver url login password)))
         (. *rdf-repository* initialize))))

(clojure/comment

(defn test-rdf-repo []
  (init-repository "com.mysql.jdbc.Driver" "jdbc:mysql://localhost:3306/clojure_sesame" "root" "root"))

)

;;
;; RDF Manipulation
;;

(defstruct xml-namespace :value :prefix)
(defstruct uri :prefix :value)
(defstruct rdf-literal :value :datatype :lang)

;; A RDF-node ca be:
;; - a URI  -> :uri
;; - a Blank node identifier -> :blank
;; - a literal value -> :literal
(defstruct rdf-node :type :value :relations)
;; A RDF-relation links two nodes with a property
(defstruct rdf-relation :value :object)
;; A triplet is formed by:
;; - subject -> rdf-node
;; - predicate -> rdf-relation
;; - object -> rdf-node
(defstruct rdf-triplet :subject :predicate :object)
;; A finite list of rdf-triplets with certain context values
(defstruct rdf-graph :triplets :context)

;;
;; Looks for a certain namespace specifier
;;
(defmulti rdf-ns identity)

;; Some default namespaces
(defmethod rdf-ns :rdf [x]
  "RDF namespace: http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  (struct uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#" :rdf))



(defn build-uri
  "Builds a new URI from a string. The URI can be specified in two different
   ways as a pair (namespace, identifier) or as a single URI string value.
   If this last way of building an URI is selected the value for :prefix of
   that URI will be null"
  ([prefix value]
     (if (= (class prefix) #=clojure.lang.PersistentStructMap)
       (struct uri (:value prefix) value)
       (struct uri prefix value)))
  ([value]
     (struct uri "" value)))

(defn build-literal
  "Builds a new literal with datatype and language annotations by default, all the
   literals are marked as xsd:string datatypes"
  ([value]
     (struct rdf-literal value (xsd-string) ""))
  ([value datatype]
     (struct rdf-literal value datatype ""))
  ([value datatype lang]
     (struct rdf-literal value datatype lang)))

(defn literal-string
  "Builds a new literal string"
  ([value]
     (build-literal value))
  ([value lang]
     (build-literal value (xsd-string) lang)))

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


(defn rdf-node-uri
  "Builds a new RDF node holding an URI"
  ([value relations]
     (struct rdf-node :uri (build-uri value) relations))
  ([prefix value relations]
     (struct rdf-node :uri (build-uri prefix value) relations)))




(clojure/comment
  "Tests"
)


(use 'clojure.contrib.test-is)

(defn mock-namespace [& parts]
  (if (nil? parts)
    (struct xml-namespace "http://test.com/" :test)
    (struct xml-namespace (first parts) (second parts))))

(deftest test-build-uri-1
  (is (= (build-uri (mock-namespace) "a")
         (build-uri "http://test.com/" "a"))))

(deftest test-ns-1
  (is (= (rdf-ns :rdf)
         {:prefix "http://www.w3.org/1999/02/22-rdf-syntax-ns#", :value :rdf})))

(deftest test-literal-1
  (is (= (build-literal "a")
         {:value "a" :datatype {:prefix :xsd, :value "string"} :lang ""})))

(deftest test-literal-2
  (is (= (build-literal "a" "b")
         {:value "a" :datatype "b" :lang ""})))

(deftest test-literal-3
  (is (= (build-literal "a" "b" "c")
         {:value "a" :datatype "b" :lang "c"})))