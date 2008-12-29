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
        '(org.openrdf.sail.memory MemoryStore)
        '(org.openrdf.repository.RepositoryConnection)
        '(org.openrdf.sail.memory.model MemURI)
        '(org.slf4j LoggerFactory))

;; (init-repository "com.mysql.jdbc.Driver" "jdbc:mysql://localhost:3306/clojure_sesame" "root" "root")
(defn init-repository
  "Instantiates a new repository connection and stores it in
   a clojure ref"
  ([driver url login password]
     (do (def *rdf-repository* (new SailRepository (new RdbmsStore driver url login password)))
         (. *rdf-repository* initialize))))

(defn init-memory-repository
  "Instantiates a new repository connection and stores it in
   a clojure ref"
  ([]
     (let [repo (new SailRepository (new MemoryStore))]
       (do (. repo initialize)
           repo))))

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
;; A finite list of rdf-nodes with certain context values
(defstruct rdf-graph :nodes :context)
;; A finite list of rdf-triplets with certain context values
(defstruct rdf-triplets-set :triplets :context)


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

(defn uri-to-string
  "Translates a URI into its string representation looking for the namespace"
  ([uri] (if (= (:prefix uri) "")
           (:value uri)
           (if (= (class (:prefix uri)) #=java.lang.String)
                  (str (:prefix uri) (:value uri))
                  (let [ns (rdf-ns (:prefix uri))
                        pref-ns (:prefix ns)]
                    (str pref-ns (:value uri)))))))

;; literals
(defn build-literal
  "Builds a new literal with datatype and language annotations by default, all the
   literals are marked as xsd:string datatypes"
  ([value]
     (with-meta (struct rdf-literal value {:prefix :xsd, :value "string"} "") {:rdf :literal}))
  ([value param]
     (if (is-rdf-meta? param)
       (with-meta (struct rdf-literal value param "") {:rdf :literal})
       (with-meta (struct rdf-literal value {:prefix :xsd, :value "string"} param) {:rdf :literal}))))

(defn literal-to-string
  "Gets a String representation of the literal compatible with SPARQL"
  ([literal]
     (let [base (str (:value literal))
           datatype (if (= "" (:datatype literal))
                      ""
                      (uri-to-string (:datatype literal)))
           lang (str (:lang literal))]
       (if (= "" lang)
         (if (= "" datatype)
           base
           (str base "^^" datatype))
         (str base "@" lang)))))

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
  ([value datatype-or-lang]
     (build-node (build-literal value datatype-or-lang) :literal-node [])))

(defn build-blank-node
  "Builds a new blank node with a given identifier"
  ([identifier preds]
     (build-node identifier :blank-node preds)))

(defn build-graph
  "Builds a new RDF graph for the provided description"
  ([nodes]
     (with-meta (struct rdf-graph nodes #{}) {:rdf :graph}))
  ([nodes context]
     (with-meta (struct rdf-graph nodes context) {:rdf :graph})))

;; Triplets set manipulation

(defn build-triplets-set
  "Builds a new triplets set"
  ([]
     (with-meta (struct rdf-triplets-set #{} #{}) {:rdf :triplets-set}))
  ([triplets]
     (with-meta (struct rdf-triplets-set (set triplets) #{}) {:rdf :triplets-set}))
  ([triplets context]
     (with-meta (struct rdf-triplets-set (set triplets) (set context)) {:rdf :triplets-set})))

(defn add-triplet
  "Appends a triplet to a triplet set"
  ([triplet triplets-set]
     (let [triplets (:triplets triplets-set)
           context (:context triplets-set)]
       (build-triplets-set (conj triplets triplet) context))))

(defn union-triplets-set
  "Computes the union of two triplets set"
  ([set-a set-b]
     (build-triplets-set (clojure.set/union (:triplets set-a) (:triplets set-b))
                         (clojure.set/union (:context set-a) (:context set-b)))))


;; Sesame translations

(defmulti sesame-translate (fn [obj factory] (rdf-meta obj)))

(defmethod sesame-translate :uri [obj factory]
  "Translates the RDF URI object into a Sesame equivalent"
  (. factory (createURI (uri-to-string obj))))

(defmethod sesame-translate :literal [obj factory]
  "Translates the RDF literal object into a Sesame equivalent"
  (if (= (:lang obj) "")
    (. factory (createLiteral (:value obj) (uri-to-string (:datatype obj))))
    (. factory (createLiteral (:value obj) (:lang obj)))))

(defmethod sesame-translate :relation [obj factory]
  "Translates the RDF relation into a Sesame equivalent"
  (with-meta (struct rdf-relation (sesame-translate (:value obj) factory)
                     (sesame-translate (:object obj) factory))
             {:rdf :relation}))

(defmethod sesame-translate :uri-node [obj factory]
  "Translates the RDF URI node into a Sesame equivalent"
  (let [value (:value obj)
        preds (:relations obj)]
    (with-meta (struct rdf-node (sesame-translate value factory)
                       (map (fn [pred] (sesame-translate pred factory)) preds))
               {:rdf :uri-node})))

(defmethod sesame-translate :literal-node [obj factory]
  "Translates the RDF literal node into a Sesame equivalent"
  (let [value (:value obj)
        preds (:relations obj)]
    (with-meta (struct rdf-node (sesame-translate value factory)
                       (map (fn [pred] (sesame-translate pred factory)) preds))
               {:rdf :literal-node})))

(defmethod sesame-translate :blank-node [obj factory]
  "Translates the RDF blank node into a Sesame equivalent"
  (let [value (:value obj)
        preds (:relations obj)]
    (with-meta (struct rdf-node
                       (. factory (createBNode value))
                       (map (fn [pred] (sesame-translate pred factory)) preds))
               {:rdf :blank-node})))

(defmethod sesame-translate :graph [obj factory]
  "Translates the RDF graph into a sesame equivalent"
  (with-meta (struct rdf-graph
                     (map (fn [node] (sesame-translate node factory)) (:nodes obj))
                     (:context obj))
             {:rdf :graph}))

(defmethod sesame-translate :triplet [obj factory]
  "Translates a RDF triplet into a sesame equivalent"
  (with-meta (struct rdf-triplet
                     (sesame-translate (:subject obj) factory)
                     (sesame-translate (:predicate obj) factory)
                     (sesame-translate (:object obj) factory))
             {:rdf :triplet}))

(defmethod sesame-translate :triplets-set [obj factory]
  "Translates a RDF triplet into a sesame equivalent"
  (build-triplets-set
   (map (fn [triplet] (sesame-translate triplet factory)) (:triplets obj))
   (:context obj)))

;; From graph to triplets
(defmulti to-triplets (fn [obj] (rdf-meta obj)))

(defmethod to-triplets :uri-node [obj]
  "Translates a RDF URI node to a triplets-set"
  (let [ col-of-sets (map (fn [predicate-triplets-translation]
                            (let [predicate-triplets-set (second predicate-triplets-translation)
                                  predicate-partial-triplet (first predicate-triplets-translation)
                                  predicate-partial (first predicate-partial-triplet)
                                  object-partial (second predicate-partial-triplet)]
                              (add-triplet (with-meta (struct rdf-triplet (:value obj) predicate-partial object-partial) {:rdf :triplet})
                                           predicate-triplets-set)))
                          (map (fn [relation] (to-triplets relation))
                               (:relations obj))) ]
    (list (:value obj) (reduce #'union-triplets-set #{} col-of-sets))))

(defmethod to-triplets :blank-node [obj]
  "Translates a RDF URI node to a triplets-set"
  (let [ col-of-sets (map (fn [predicate-triplets-translation]
                            (let [predicate-triplets-set (second predicate-triplets-translation)
                                  predicate-partial-triplet (first predicate-triplets-translation)
                                  predicate-partial (first predicate-partial-triplet)
                                  object-partial (second predicate-partial-triplet)]
                              (add-triplet (with-meta (struct rdf-triplet (:value obj) predicate-partial object-partial) {:rdf :triplet})
                                           predicate-triplets-set)))
                          (map (fn [relation] (to-triplets relation))
                               (:relations obj))) ]
    (list (:value obj) (reduce #'union-triplets-set #{} col-of-sets))))

(defmethod to-triplets :literal-node [obj]
  "Translates a RDF literal node to a triplets-set"
  (list (:value obj) (build-triplets-set)))

(defmethod to-triplets :relation [obj]
  "Translates a RDF relation node to a triplets-set"
  (let [ object-triplets-translation (to-triplets (:object obj))
         object-uri (first object-triplets-translation)
         object-triplets (second object-triplets-translation) ]
    (list (list (:value obj) object-uri) object-triplets)))

(defmethod to-triplets :graph [obj]
  "Translates a RDF graph to a set of triplets-set"
  (reduce #'union-triplets-set #{} (map
                                    (fn [node] (second (to-triplets node)))
                                    (:nodes obj))))



;; SPARQL queries

;; A SPARQL identifier
(defstruct rdf-variable :value)
;; A variable node in a graph template
(defstruct rdf-variable-node :value :relations)
;; A variable relation in a graph template
(defstruct rdf-variable-relation :value :object)
;; A SPARQL optional graph
(defstruct rdf-optional-graph :value)
;; A SPARQL filter
(defstruct rdf-sparql-filter :type :identifier :condition)
;; A variable graph template
(defstruct rdf-graph-template :nodes-filters)

(defn build-variable
  "Builds a new variable identifier"
  ([identifier]
     (with-meta (struct rdf-variable identifier) {:rdf :variable-identifier})))

(defn build-variable-relation
 "Builds a new variable relation"
 ([identifier related-object]
    (with-meta (struct rdf-variable-relation (build-variable identifier) related-object) {:rdf :variable-relation})))

(defn build-variable-node
 "Builds a new variable node"
 ([identifier preds]
    (build-node (build-variable identifier) :variable-node preds)))

(defn build-optional-graph
  "Builds an optional graph in a pattern"
  ([graph]
     (with-meta (struct rdf-optional-graph (build-graph graph)) {:rdf :optional-graph})))

(defn build-filter
  "Builds a SPARQL filter"
  ([type identifier condition]
     (with-meta (struct rdf-sparql-filter type identifier condition) {:rdf :sparql-filter})))

(defn build-<-filter
  "< than SPARQL filter"
  ([identifier value]
     (build-filter :less-than identifier value)))

(defn build-graph-template
  "A graph template for a SPARQL query"
  ([graph-filters-mappings]
     (with-meta (struct rdf-graph-template graph-filters-mapping) {:rdf :graph-template})))

;; a SPARQL query to a triplets-set
(defmethod to-triplets :variable-node [obj]
  "transforms a variable node in a set of triplets where the variable
   is identified by a SPARQL variable"
  (let [ col-of-sets (map (fn [predicate-triplets-translation]
                            (let [predicate-triplets-set (second predicate-triplets-translation)
                                  predicate-partial-triplet (first predicate-triplets-translation)
                                  predicate-partial (first predicate-partial-triplet)
                                  object-partial (second predicate-partial-triplet)]
                              (add-triplet (with-meta (struct rdf-triplet (:value obj) predicate-partial object-partial) {:rdf :triplet})
                                           predicate-triplets-set)))
                          (map (fn [relation] (to-triplets relation))
                               (:relations obj))) ]
    (list (:value obj) (reduce #'union-triplets-set #{} col-of-sets))))

(defmethod to-triplets :variable-relation [obj]
  "Translates a RDF relation node to a triplets-set"
  (let [ object-triplets-translation (to-triplets (:object obj))
         object-uri (first object-triplets-translation)
         object-triplets (second object-triplets-translation) ]
    (list (list (:value obj) object-uri) object-triplets)))

;; Translates a graph template into a SPARQL query
(defmulti to-sparql (fn [obj bindings] (rdf-meta obj)))

(defmethod to-sparql :uri-node [obj bindings]
  "translates a uri-node into a SPARQL query"
  (let [uri-string (uri-to-string (:value obj)) ]
    (list uri-string
          (if (nil? (:relations obj))
            uri-string
            (reduce  (fn [fragment-a fragment-b]  (str fragment-a fragment-b))
                     ""
                     (map (fn [relation] (str uri-string " " (to-sparql relation bindings)))
                          (:relations obj)))))))

(defmethod to-sparql :blank-node [obj bindings]
  "translates a blank-node into a SPARQL query"
  (let [uri-string (str "_:" (:value obj)) ]
    (list uri-string
          (if (nil? (:relations obj))
            uri-string
            (reduce  (fn [fragment-a fragment-b]  (str fragment-a fragment-b))
                     ""
                     (map (fn [relation] (str uri-string " " (to-sparql relation bindings)))
                          (:relations obj)))))))

(defmethod to-sparql :literal-node [obj bindings]
  "translates a literal-node into a SPARQL query"
  (let [literal-string (literal-to-string (:value obj)) ]
    (list literal-string "")))

(defmethod to-sparql :relation [obj bindings]
  "translates a relation into a SPARQL query"
  (let [ object-triplets-translation (to-sparql (:object obj) bindings)
         object-sparql (first object-triplets-translation)
         sparql-fragment (second object-triplets-translation) ]
    (str (uri-to-string (:value obj)) " " object-sparql " .\n" sparql-fragment) ))

(defn resolve-bindings
  "Returns the SPARQL representation of a variable or the binded value
   if a binding for that identifier exists in the bindings hash"
  ([identifier bindings]
     (let [kw-identifier (:value identifier)]
       (if (nil? (get bindings kw-identifier))
         (str "?" (. (str kw-identifier) (substring 1 (. (str kw-identifier) (length)))))
         (let [identifier-val (:value identifier)]
           (if (= (:literal (rdf-meta identifier-val)))
             (literal-to-string (:value identifier-val))
             (uri-to-string (:value identifier-val))))))))

(defmethod to-sparql :variable-node [obj bindings]
  "translates a uri-node into a SPARQL query"
  (let [uri-string  (resolve-bindings (:value obj) bindings)]
    (list uri-string
          (if (nil? (:relations obj))
            uri-string
            (reduce  (fn [fragment-a fragment-b]  (str fragment-a fragment-b))
                     ""
                     (map (fn [relation] (str uri-string " " (to-sparql relation bindings)))
                          (:relations obj)))))))

(defmethod to-sparql :variable-relation [obj bindings]
  "translates a relation into a SPARQL query"
  (let [ object-triplets-translation (to-sparql (:object obj) bindings)
         object-sparql (first object-triplets-translation)
         sparql-fragment (second object-triplets-translation) ]
    (str (str (resolve-bindings (:value obj) bindings)
              " " object-sparql " .\n" sparql-fragment) )))

(defmethod to-sparql :graph [obj bindings]
  "Translates a RDF graph to a set of triplets-set"
  (str "{ " (reduce (fn [x y] (str x y))
                    ""
                    (map (fn [node] (second (to-sparql node bindings)))
                         (:nodes obj)))
       " }\n"))

(defmethod to-sparql :optional-graph [obj bindings]
  "Translates a RDF graph to a set of triplets-set"
  (str "OPTIONAL " (to-sparql (:value obj) bindings)))

;;(defmethod to-sparql :graph-template [obj bindings]


;; Persisting triplets into the repository
(defn write-graph-in-repository
  "Stores the statements in a graph into the provided repository"
  ([graph connection]
     (let [translated-graph (sesame-translate graph (. connection (getValueFactory)))
           the-triplets (:triplets (to-triplets translated-graph))]
       (loop [triplets the-triplets]
         (if (not (nil? triplets))
           (let [triplet (first triplets)]
             (do
               (. connection (add (:subject triplet)
                                  (:predicate triplet)
                                  (:object triplet)
                                  (make-array org.openrdf.sail.memory.model.MemURI 0)))
               (recur (rest triplets))))
           (. connection (commit)))))))


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

(deftest translate-uri-1
  (is (= (uri-to-string (build-uri (rdf-ns :rdf) "test"))
         "http://www.w3.org/1999/02/22-rdf-syntax-ns#test")))

(deftest translate-uri-2
  (is (= (uri-to-string (build-uri "test"))
         "test")))

(deftest translate-uri-3
  (is (= (uri-to-string (build-uri "a#" "test"))
         "a#test")))

(deftest test-ns-1
  (is (= (rdf-ns :rdf)
         {:prefix "http://www.w3.org/1999/02/22-rdf-syntax-ns#", :value :rdf})))

(deftest test-literal-1
  (is (= (build-literal "a")
         {:value "a" :datatype {:prefix :xsd, :value "string"} :lang ""})))

(deftest test-literal-2
  (is (= (build-literal "a" "b")
         {:value "a" :datatype {:prefix :xsd, :value "string"} :lang "b"})))

(deftest test-literal-3
  (is (= (build-literal "a" (build-uri "test"))
         {:value "a" :datatype {:prefix "", :value "test"} :lang ""})))

(deftest test-literal-to-string-1
  (is (= (literal-to-string (build-literal "a"))
         "a^^http://www.w3.org/2001/XMLSchema#string")))

(deftest test-literal-to-string-2
  (is (= (literal-to-string (build-literal "a" "es_es"))
         "a@es_es")))

(deftest test-literal-to-string-3
  (is (= (literal-to-string (build-literal "a" (build-uri "http://test.com#datatype")))
         "a^^http://test.com#datatype")))

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

(deftest test-sesame-translate-uri
  (let [repo (init-memory-repository)
        conn (. repo (getConnection))]
    (do
      (is (= (. (sesame-translate (build-uri (rdf-ns :rdf) "test") (. conn (getValueFactory)))
                (toString))
             (uri-to-string (build-uri (rdf-ns :rdf) "test"))))
      (. conn (close)))))

(deftest test-sesame-translate-literal-1
  (let [repo (init-memory-repository)
        conn (. repo (getConnection))]
    (do
      (is (= (. (sesame-translate (build-literal "test") (. conn (getValueFactory)))
                (getLabel))
             (:value (build-literal "test"))))
      (. conn (close)))))

(deftest test-sesame-translate-literal-2
  (let [repo (init-memory-repository)
        conn (. repo (getConnection))]
    (do
      (is (= (. (sesame-translate (build-literal "test") (. conn (getValueFactory)))
                (toString))
             "\"test\"@http://www.w3.org/2001/xmlschema#string"))
      (. conn (close)))))

(deftest test-sesame-translate-relation-1
  (let [repo (init-memory-repository)
        conn (. repo (getConnection))]
    (do
      (is (= (. (sesame-translate (build-relation (rdf-ns :rdf) "test" (build-literal "testPredicate")) (. conn (getValueFactory)))
                (toString))
             "{:value #=(org.openrdf.sail.memory.model.MemURI. \"http://www.w3.org/1999/02/22-rdf-syntax-ns#test\"), :object #=(org.openrdf.sail.memory.model.MemLiteral. \"\\\"testPredicate\\\"@http://www.w3.org/2001/xmlschema#string\")}"))
      (. conn (close)))))

(deftest test-sesame-translate-relation-2
  (let [repo (init-memory-repository)
        conn (. repo (getConnection))]
    (do
      (is (= (meta (sesame-translate (build-relation (rdf-ns :rdf) "test" (build-literal "testPredicate")) (. conn (getValueFactory))))
             {:rdf :relation}))
      (. conn (close)))))

(deftest test-sesame-translate-uri-node-1
  (let [repo (init-memory-repository)
        conn (. repo (getConnection))]
    (do
      (is (= (. (sesame-translate (build-uri-node :rdf "test" [(build-relation :rdf "test" (build-uri-node :rdf "test" []))]) (. conn (getValueFactory)))
                (toString))
             "{:value #=(org.openrdf.sail.memory.model.MemURI. \"http://www.w3.org/1999/02/22-rdf-syntax-ns#test\"), :relations ({:value #=(org.openrdf.sail.memory.model.MemURI. \"http://www.w3.org/1999/02/22-rdf-syntax-ns#test\"), :object {:value #=(org.openrdf.sail.memory.model.MemURI. \"http://www.w3.org/1999/02/22-rdf-syntax-ns#test\"), :relations nil}})}"))
      (. conn (close)))))

(deftest test-sesame-translate-uri-node-2
  (let [repo (init-memory-repository)
        conn (. repo (getConnection))]
    (do
      (is (= (meta (sesame-translate (build-uri-node :rdf "test" [(build-relation :rdf "test" (build-uri-node :rdf "test" []))]) (. conn (getValueFactory))))
             {:rdf :uri-node}))
      (. conn (close)))))

(deftest test-sesame-translate-literal-node-1
  (let [repo (init-memory-repository)
        conn (. repo (getConnection))]
    (do
      (is (= (. (sesame-translate (build-literal-node "test") (. conn (getValueFactory)))
                (toString))
             "{:value #=(org.openrdf.sail.memory.model.MemLiteral. \"\\\"test\\\"@http://www.w3.org/2001/xmlschema#string\"), :relations nil}"))
      (. conn (close)))))

(deftest test-sesame-translate-literal-node-2
  (let [repo (init-memory-repository)
        conn (. repo (getConnection))]
    (do
      (is (= (meta (sesame-translate (build-literal-node "test") (. conn (getValueFactory))))
             {:rdf :literal-node}))
      (. conn (close)))))

(deftest test-sesame-translate-triplet-1
  (let [repo (init-memory-repository)
        conn (. repo (getConnection))]
    (do
      (is (= (str (sesame-translate (with-meta (struct rdf-triplet
                                                       (build-uri (rdf-ns :rdf) "subject")
                                                       (build-uri (rdf-ns :rdf) "predicate")
                                                       (build-uri (rdf-ns :rdf) "object"))
                                               {:rdf :triplet}) (. conn (getValueFactory))))
             "{:subject #=(org.openrdf.sail.memory.model.MemURI. \"http://www.w3.org/1999/02/22-rdf-syntax-ns#subject\"), :predicate #=(org.openrdf.sail.memory.model.MemURI. \"http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate\"), :object #=(org.openrdf.sail.memory.model.MemURI. \"http://www.w3.org/1999/02/22-rdf-syntax-ns#object\")}"))
      (. conn (close)))))

(deftest test-add-triplet-1
  (is (= (add-triplet (struct rdf-triplet :a :b :c) (struct rdf-triplets-set (set []) (set [])))
         {:triplets #{{:subject :a, :predicate :b, :object :c}}, :context #{}})))

(deftest test-add-triplet-12
  (is (= (add-triplet (struct rdf-triplet :a :b :c) (struct rdf-triplets-set (set [(struct rdf-triplet :a :b :c)]) (set [])))
         {:triplets #{{:subject :a, :predicate :b, :object :c}}, :context #{}})))

(deftest test-to-triplets-1
  (is (= (to-triplets (build-uri-node :rdf "test-subject"
                                      [(build-relation :rdf "test-relation"
                                                       (build-uri-node :rdf "test-object" []))]))
         '({:prefix :rdf, :value "test-subject"}
           {:triplets #{{:subject {:prefix :rdf, :value "test-subject"}
                         :predicate {:prefix :rdf, :value "test-relation"}
                         :object {:prefix :rdf, :value "test-object"}}}
            :context #{}}))))

(deftest test-to-triplets-2
  (is (= (to-triplets (build-uri-node :rdf "test-subject"
                                      [(build-relation :rdf "test-relation"
                                                       (build-uri-node :rdf "test-object" [(build-relation :rdf "test-relation2"
                                                                                                           (build-uri-node :rdf "test-object2" []))]))]))
         '({:prefix :rdf, :value "test-subject"}
           {:triplets #{{:subject {:prefix :rdf, :value "test-subject"}
                         :predicate {:prefix :rdf, :value "test-relation"}
                         :object {:prefix :rdf, :value "test-object"}}
                        {:subject {:prefix :rdf, :value "test-object"}
                         :predicate {:prefix :rdf, :value "test-relation2"}
                         :object {:prefix :rdf, :value "test-object2"}}}, :context #{}}))))

(deftest test-to-triplets-4
  (is (= (to-triplets (build-uri-node :rdf "test-subject"
                                      [(build-relation :rdf "test-relation"
                                                       (build-literal-node "test"))]))
         '({:prefix :rdf, :value "test-subject"}
           {:triplets #{{:subject {:prefix :rdf, :value "test-subject"}
                         :predicate {:prefix :rdf, :value "test-relation"}
                         :object {:value "test", :datatype {:prefix :xsd, :value "string"}, :lang ""}}}
            :context #{}}))))

(deftest test-to-triplets-5
  (is (= (to-triplets (build-uri-node :rdf "test-subject"
                                      [(build-relation :rdf "test-relation"
                                                       (build-blank-node "test" []))]))
         '({:prefix :rdf, :value "test-subject"}
           {:triplets #{{:subject {:prefix :rdf, :value "test-subject"}
                         :predicate {:prefix :rdf, :value "test-relation"}
                         :object "test"}}
            :context #{}}))))

(deftest test-to-triplets-6
  (is (= (to-triplets (build-graph
                       [(build-uri-node :rdf "test-subjecta"
                                        [(build-relation :rdf "relation-1"
                                                         (build-blank-node "testa" []))])
                        (build-uri-node :rdf "test-subjectb"
                                        [(build-relation :rdf "relation-2"
                                                         (build-blank-node "testb" []))])]))
         '{:triplets #{{:subject {:prefix :rdf, :value "test-subjecta"}
                        :predicate {:prefix :rdf, :value "relation-1"}
                        :object "testa"}
                       {:subject {:prefix :rdf, :value "test-subjectb"}
                        :predicate {:prefix :rdf, :value "relation-2"}
                        :object "testb"}}
           :context #{}})))

(deftest test-to-triplets-variable-1
  (is (= (to-triplets (build-graph
                       [(build-variable-node :x
                                        [(build-relation :rdf "relation-1"
                                                         (build-blank-node "testa" []))])
                        (build-uri-node :rdf "test-subjectb"
                                        [(build-variable-relation :y
                                                         (build-blank-node "testb" []))])]))
         '{:triplets #{{:subject {:value :x}
                        :predicate {:prefix :rdf, :value "relation-1"}
                        :object "testa"}
                       {:subject {:prefix :rdf, :value "test-subjectb"}
                        :predicate {:value :y}
                        :object "testb"}}
           :context #{}})))

(deftest write-to-repository
  (let [repo (init-memory-repository)
        conn (. repo (getConnection))
        graph (build-graph
                       [(build-uri-node :rdf "test-subjecta"
                                        [(build-relation :rdf "relation-1"
                                                         (build-blank-node "testa" []))])
                        (build-uri-node :rdf "test-subjectb"
                                        [(build-relation :rdf "relation-2"
                                                         (build-blank-node "testb" []))])])]
    (do (write-graph-in-repository graph conn)
        (is (= 2
               (. conn (size (make-array org.openrdf.sail.memory.model.MemURI 0)))))
        (. conn (close)))))

(deftest test-to-sparql-1
  (is (= (to-sparql (build-graph
                       [(build-variable-node :x
                                        [(build-relation :rdf "relation-1"
                                                         (build-uri-node :rdf "testa" []))])
                        (build-uri-node :rdf "test-subjectb"
                                        [(build-variable-relation :y
                                                         (build-uri-node :rdf "testb" []))])])
                    {})
         "{ ?x http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-1 http://www.w3.org/1999/02/22-rdf-syntax-ns#testa .\nhttp://www.w3.org/1999/02/22-rdf-syntax-ns#test-subjectb ?y http://www.w3.org/1999/02/22-rdf-syntax-ns#testb .\n }\n")))

(deftest test-to-sparql-2
  (is (= (to-sparql (build-uri-node :rdf "testb" [])
                    {})
         '("http://www.w3.org/1999/02/22-rdf-syntax-ns#testb" ""))))

(deftest test-to-sparql-3
  (is (= (to-sparql (build-relation :rdf "testb" (build-uri-node :rdf "testc" []))
                    {})
         "http://www.w3.org/1999/02/22-rdf-syntax-ns#testb http://www.w3.org/1999/02/22-rdf-syntax-ns#testc .\n")))

(deftest test-to-sparql-4
  (is (= (to-sparql (build-optional-graph
                       [(build-variable-node :x
                                        [(build-relation :rdf "relation-1"
                                                         (build-uri-node :rdf "testa" []))])
                        (build-uri-node :rdf "test-subjectb"
                                        [(build-variable-relation :y
                                                         (build-uri-node :rdf "testb" []))])])
                    {})
         "OPTIONAL { ?x http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-1 http://www.w3.org/1999/02/22-rdf-syntax-ns#testa .\nhttp://www.w3.org/1999/02/22-rdf-syntax-ns#test-subjectb ?y http://www.w3.org/1999/02/22-rdf-syntax-ns#testb .\n }\n")))

(deftest test-to-sparql-5
  (is (= (to-sparql (build-optional-graph
                       [(build-variable-node :x
                                        [(build-relation :rdf "relation-1"
                                                         (build-literal-node "test" "es_ES"))])
                        (build-uri-node :rdf "test-subjectb"
                                        [(build-variable-relation :y
                                                         (build-uri-node :rdf "testb" []))])])
                    {})
         "OPTIONAL { ?x http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-1 test@es_ES .\nhttp://www.w3.org/1999/02/22-rdf-syntax-ns#test-subjectb ?y http://www.w3.org/1999/02/22-rdf-syntax-ns#testb .\n }\n")))

(deftest test-to-sparql-6
  (is (= (to-sparql (build-optional-graph
                       [(build-variable-node :x
                                        [(build-relation :rdf "relation-1"
                                                         (build-literal-node "test" "es_ES"))])
                        (build-blank-node "a"
                                        [(build-variable-relation :y
                                                         (build-uri-node :rdf "testb" []))])])
                    {})
         "OPTIONAL { ?x http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-1 test@es_ES .\n_:a ?y http://www.w3.org/1999/02/22-rdf-syntax-ns#testb .\n }\n")))