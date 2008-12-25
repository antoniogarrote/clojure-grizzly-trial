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

;; :rdf metadata manipulation
(defn rdf-meta
  "Returns the :rdf metadata value from an object"
  ([obj] (:rdf (meta obj))))

(defn is-rdf-meta?
  "Checks if the :rdf metadata value is available for the object"
  ([obj] (not (= nil (rdf-meta obj)))))

;; A RDF-node ca be:
;; - a URI  -> :uri
;; - a Blank node identifier -> :blank
;; - a literal value -> :literal
(defstruct rdf-node :value :relations)
;; A RDF-relation links two nodes with a property
(defstruct rdf-relation :value :object)
;; A triplet is formed by:
;; - subject -> rdf-node
;; - predicate -> rdf-relation
;; - object -> rdf-node
(defstruct rdf-triplet :subject :predicate :object)
;; A finite list of rdf-triplets with certain context values
(defstruct rdf-graph :triplets :context)

(defn build-uri
  "Builds a new URI from a string. The URI can be specified in two different
   ways as a pair (namespace, identifier) or as a single URI string value.
   If this last way of building an URI is selected the value for :prefix of
   that URI will be null"
  ([prefix value]
     (if (= (class prefix) #=clojure.lang.PersistentStructMap)
       (with-meta (struct uri (:value prefix) value) {:rdf :uri})
       (with-meta (struct uri prefix value) {:rdf :uri})))
  ([value]
     (with-meta (struct uri "" value) {:rdf :uri})))

;;
;; Looks for a certain namespace specifier
;;
(defmulti rdf-ns identity)

;; Some default namespaces
(defmethod rdf-ns :rdf [x]
  "RDF namespace: http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  (build-uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#" :rdf))

;; literals
(defn build-literal
  "Builds a new literal with datatype and language annotations by default, all the
   literals are marked as xsd:string datatypes"
  ([value]
     (with-meta (struct rdf-literal value {:prefix :xsd, :value "string"} "") {:rdf :literal}))
  ([value datatype]
     (with-meta (struct rdf-literal value datatype "") {:rdf :literal}))
  ([value datatype lang]
     (with-meta (struct rdf-literal value datatype lang) {:rdf :literal})))

;; relations

(defn build-relation
  "Builds a new relation for a given URI"
  ([prefix uri related-object]
     (with-meta (struct rdf-relation (build-uri prefix uri) related-object) {:rdf :relation}))
  ([predicate related-object]
     (if (= (rdf-meta predicate) :uri)
       (with-meta (struct rdf-relation predicate related-object) {:rdf :relation})
       (with-meta (struct rdf-relation (build-uri predicate) related-object) {:rdf :relation}))))

;; nodes
(defn build-node
  "Builds a new node with the given object"
  ([subject type preds]
     (with-meta (struct rdf-node subject preds) {:rdf type})))

(defn build-uri-node
  "Builds a new node storing a URI"
  ([prefix value preds]
     (build-node (build-uri prefix value) :uri-node preds))
  ([value  preds]
     (if (= (rdf-meta value) :uri)
       (build-node value :uri-node preds)
       (build-node (build-uri value) :uri-node preds))))

(defn build-literal-node
  "Builds a new node storing a literal"
  ([value]
     (if (= (rdf-meta value) :literal)
       (build-node value :literal-node [])
       (build-node (build-literal value) :literal-node [])))
  ([value datatype]
     (build-node (build-literal value datatype) :literal-node []))
  ([value datatype lang]
     (build-node (build-literal value datatype lang) :literal-node [])))

(defn build-blank-node
  "Builds a new blank node with a given identifier"
  ([identifier preds]
     (build-node identifier :blank-node preds)))

(clojure/comment
  "Tests"
)


(use 'clojure.contrib.test-is)

(defn mock-namespace [& parts]
  (if (nil? parts)
    (struct xml-namespace "http://test.com/" :test)
    (struct xml-namespace (first parts) (second parts))))

(deftest test-rdf-meta-1
  (is (= (rdf-meta (with-meta [] {:rdf :test}))
         :test)))

(deftest test-rdf-meta-2
  (is (= (rdf-meta (with-meta [] {:rdf-other :test}))
         nil)))

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

(deftest test-relation-1
  (is (= (build-relation (rdf-ns :rdf) "test" (build-literal "testPredicate"))
         {:value {:prefix :rdf, :value "test"}, :object {:value "testPredicate", :datatype {:prefix :xsd, :value "string"}, :lang ""}})))

(deftest test-relation-meta-1
  (is (= (meta (build-relation (rdf-ns :rdf) "test" (build-literal "testPredicate")))
         {:rdf :relation})))

(deftest test-relation-2
  (is (= (build-relation (build-uri (rdf-ns :rdf) "test") (build-literal "testPredicate"))
         {:value {:prefix :rdf, :value "test"}, :object {:value "testPredicate", :datatype {:prefix :xsd, :value "string"}, :lang ""}})))

(deftest test-relation-meta-2
  (is (= (meta (build-relation (build-uri (rdf-ns :rdf) "test") (build-literal "testPredicate")))
         {:rdf :relation})))

(deftest test-relation-3
  (is (= (build-relation (build-uri "http://test.com#test_1") (build-literal "testPredicate"))
         {:value {:prefix "", :value "http://test.com#test_1"}, :object {:value "testPredicate", :datatype {:prefix :xsd, :value "string"}, :lang ""}})))

(deftest test-relation-meta-3
  (is (= (meta (build-relation (build-uri "http://teste.com#test_1") (build-literal "testPredicate")))
         {:rdf :relation})))

(deftest test-build-node-1
  (is (= (build-node "test" :test [(build-relation "test" (build-uri "test"))])
         {:value "test", :relations [{:value {:prefix "", :value "test"}, :object {:prefix "", :value "test"}}]})))

(deftest test-build-node-2
  (is (= (build-node "test" :test
                     [(build-relation "test" (build-uri "test"))
                      (build-relation "test2" (build-uri "hola"))])
         {:value "test", :relations [{:value {:prefix "", :value "test"}, :object {:prefix "", :value "test"}}
                                     {:value {:prefix "", :value "test2"}, :object {:prefix "", :value "hola"}}]})))

(deftest test-build-node-3
  (is (= (meta (build-node "test" :test [(build-relation "test" (build-uri "test"))]))
         {:rdf :test})))

(deftest test-build-node-4
  (is (= (build-uri-node :a "test" [(build-relation "test" (build-uri-node :b "test" []))])
         {:relations [{:value {:prefix "", :value "test"}
                       :object {:relations []
                                :value {:prefix :b, :value "test"}}}]
          :value {:prefix :a, :value "test"}})))

(deftest test-build-node-literal-1
  (is (= (build-literal-node "test")
       {:value {:value "test", :datatype {:prefix :xsd, :value "string"}, :lang ""}, :relations []})))

(deftest test-blank-node-1
  (is (= (build-blank-node "test" [])
         {:value "test", :relations []})))

(deftest test-blank-node-2
  (is (= (meta (build-blank-node "test" []))
         {:rdf :blank-node})))

