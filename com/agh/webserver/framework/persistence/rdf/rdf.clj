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
        '(org.openrdf.query TupleQuery)
        '(org.openrdf.query TupleQueryResult)
        '(org.openrdf.query BindingSet)
        '(org.openrdf.query QueryLanguage)
        '(org.slf4j LoggerFactory))

(use 'com.agh.utils)
(use 'com.agh.webserver.framework.logger)

;; (init-repository "com.mysql.jdbc.Driver" "jdbc:mysql://localhost:3306/clojure_sesame" "root" "root")
(defn init-repository!
  "Instantiates a new repository connection and stores it in
   a clojure ref"
  ([driver url login password]
     (let [rdf-repository (new SailRepository (new RdbmsStore driver url login password))]
       (. rdf-repository initialize))))

(defn init-memory-repository!
  "Instantiates a new repository connection and stores it in
   a clojure ref"
  ([]
     (let [repo (new SailRepository (new MemoryStore))]
       (do (. repo initialize)
           repo))))

(def *repositories-registry* (ref {}))

(defn repositories-registry-clear!
  "Empties the repositories registry"
  ([]
     (dosync
      (commute *repositories-registry*
               (fn [registry] {})))))

(defn register-repository!
 "Registers a repository with the given name, if no name is given
  the repository is registered as the default repository."
 ([name repository]
    (dosync
     (commute *repositories-registry*
              (fn [registry repo]
                (merge registry {name repo}))
              repository)))
 ([repository]
    (register-repository! :default repository)))

(defn default-repository
  "Returns the default repository"
  ([]
     (dosync
      (let [maybe-default (:default @*repositories-registry*)]
        (if (nil? maybe-default)
          ((first (keys @*repositories-registry*)) @*repositories-registry*)
          maybe-default)))))

(defn repository
  "Returns the repository by name, if no name is given it returns the default repository"
  ([repository-name]
     (dosync
      (get @*repositories-registry* repository-name)))
  ([]
     (default-repository)))

(def *connections* (ref {}))

(defn connections-clear!
  "Empties the repositories registry"
  ([]
     (dosync
      (commute *connections*
               (fn [connections] {})))))

(defn connection!
  "Returns a connection for the requested repository, creating it if
   necessary"
  ([repository-name]
     (dosync
      (let [connection (get @*connections* repository-name)]
        (if (nil? connection)
          (do
            (commute *connections*
                     (fn [registry]
                       (merge registry {repository-name (. (repository repository-name) getConnection)})))
            (get @*connections* repository-name))
          connection))))
  ([] (connection! :default)))


(defn close-connection!
  "Closes the connection to a given repository or the default repository"
  ([repository-name]
     (dosync
      (let [connection (get @*connections* repository-name)]
        (if (not (nil? connection))
          (do (dissoc @*connections* repository-name)
              (. connection close))))))
  ([] (close-connection! :default)))



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
(defstruct blank-node-identifier :value)

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


(defn rdf-type []
  (build-uri (rdf-ns :rdf) "type"))

;; Basic RDF blocks
(defn uri-to-string
  "Translates a URI into its string representation looking for the namespace"
  ([uri]
     (if (= (class uri) #=java.lang.String) ;; are they sending us a string, then it must be an URI
       uri
       (if (= (:prefix uri) "") ;; then we have to build the URI string representation
         (:value uri)
         (if (= (class (:prefix uri)) #=java.lang.String)
           (str (:prefix uri) (:value uri))
           (let [ns (rdf-ns (:prefix uri))
                 pref-ns (:prefix ns)]
             (str pref-ns (:value uri))))))))

;; literals
(defn build-literal
  "Builds a new literal with datatype and language annotations by default, all the
   literals are marked as xsd:string datatypes"
  ([value]
     (with-meta (struct rdf-literal value (with-meta {:prefix :xsd, :value "string"} {:rdf :uri}) "") {:rdf :literal}))
  ([value param]
     (if (is-rdf-meta? param)
       (with-meta (struct rdf-literal value param "") {:rdf :literal})
       (with-meta (struct rdf-literal value (with-meta {:prefix :xsd, :value "string"} {:rdf :uri}) param) {:rdf :literal}))))

(defn literal-to-string
  "Gets a String representation of the literal compatible with SPARQL"
  ([literal]
     (let [base (str "\""(:value literal) "\"")
           datatype (if (= "" (:datatype literal))
                      ""
                      (uri-to-string (:datatype literal)))
           lang (str (:lang literal))]
       (if (= "" lang)
         (if (= "" datatype)
           base
           (str base "^^" datatype))
         (str base "@" lang)))))

;; blank node
(defn build-bnode-identifier
  "builds a new blank node identifier"
  ([identifier]
     (with-meta (struct blank-node-identifier identifier) {:rdf :bnode-identifier})))

(defn bnode-identifier-to-string
  "Gets a String representation of the bnode identifier"
  ([bnode-identifier]
     (:value bnode-identifier)))

;; relations
(defn build-relation
  "Builds a new relation for a given URI"
  ([prefix uri related-object]
     (let [meta-related (rdf-meta related-object)]
       (if (= meta-related :uri)
         (with-meta (struct rdf-relation (build-uri prefix uri) (build-uri-node related-object)) {:rdf :relation})
         (if (= meta-related :literal)
           (with-meta (struct rdf-relation (build-uri prefix uri) (build-literal-node related-object)) {:rdf :relation})
           (with-meta (struct rdf-relation (build-uri prefix uri) related-object) {:rdf :relation})))))
  ([predicate related-object]
     (let [meta-related (rdf-meta related-object)
           to-relate (if (= meta-related :uri)
                       (build-uri-node related-object)
                       (if (= meta-related :literal)
                         (build-literal-node related-object)
                         related-object)) ]
       (if (= (rdf-meta predicate) :uri)
         (with-meta (struct rdf-relation predicate to-relate) {:rdf :relation})
         (with-meta (struct rdf-relation (build-uri predicate) to-relate) {:rdf :relation})))))

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
     (if (= (class preds) #=java.lang.String) ;; special case where 2 args can be a tow part uri -> (:ns ref) not (uri preds)
       (build-node (build-uri value preds) :uri-node [])
       (if (= (rdf-meta value) :uri)
         (build-node value :uri-node preds)
         (build-node (build-uri value) :uri-node preds))))
  ([value]
     (if (= (rdf-meta value) :uri)
       (build-node value :uri-node [])
       (build-node (build-uri value) :uri-node []))))


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
     (build-node (build-bnode-identifier identifier) :blank-node preds)))

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

(defmulti from-sesame (fn [obj] (class obj)))


(defmethod from-sesame #=org.openrdf.sail.memory.model.MemBNode [obj]
  "Translates a Sesame BNode into a blank URI"
  (build-bnode-identifier (. obj (stringValue))))

(defmethod from-sesame #=org.openrdf.sail.memory.model.MemLiteral [obj]
  "Translates a Sesame BNode into a blank URI"
  (let [label (. obj getLabel)
        datatype (. obj (getDatatype))
        lang (.. obj (getLanguage)) ]
    (if (or (nil? datatype) (= datatype "http://www.w3.org/2001/XMLSchema#string"))
      (build-literal label lang)
      (build-literal label (build-uri (. datatype stringValue))))))

(defmethod from-sesame #=org.openrdf.sail.memory.model.MemURI [obj]
  "Translates a Sesame URI into a RDF URI"
  (build-uri (. obj stringValue)))



(defmulti sesame-translate (fn [obj factory] (rdf-meta obj)))

(defmethod sesame-translate :uri [obj factory]
  "Translates the RDF URI object into a Sesame equivalent"
  (. factory (createURI (uri-to-string obj))))

(defmethod sesame-translate :literal [obj factory]
  "Translates the RDF literal object into a Sesame equivalent"
  (if (= (:lang obj) "")
    (if (not (nil? (:datatype obj)))
      (. factory (createLiteral (str (:value obj)) (sesame-translate (:datatype obj) factory)))
      (. factory (createLiteral (:value obj))))
    (. factory (createLiteral (str (:value obj)) (:lang obj)))))

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
  (let [value (:value (:value obj))
        preds (:relations obj)]
    (with-meta (struct rdf-node
                       (build-bnode-identifier (. factory (createBNode value)))
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
  (let [col-of-sets (map (fn [predicate-triplets-translation]
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
                              (add-triplet (with-meta (struct rdf-triplet (bnode-identifier-to-string (:value obj)) predicate-partial object-partial) {:rdf :triplet})
                                           predicate-triplets-set)))
                          (map (fn [relation] (to-triplets relation))
                               (:relations obj))) ]
    (list (bnode-identifier-to-string (:value obj)) (reduce #'union-triplets-set #{} col-of-sets))))

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

(defmethod to-triplets :default [obj]
  "Translates a RDF graph to a set of triplets-set default action"
  (throw (Exception. (str "Error, unknown rdf-meta in to-triplets for object " obj " with meta " (rdf-meta obj)))))

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
(defstruct rdf-sparql-filter :identifier :value)
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
    (build-node (build-variable identifier) :variable-node preds))
 ([identifier]
    (build-node (build-variable identifier) :variable-node [])))

(defn build-optional-graph
  "Builds an optional graph in a pattern"
  ([graph]
     (with-meta (struct rdf-optional-graph (build-graph graph)) {:rdf :optional-graph})))

(defn build-filter
  "Builds a SPARQL filter"
  ([identifier condition]
     (with-meta (struct rdf-sparql-filter identifier condition) {:rdf :sparql-filter})))

(defn build-graph-template
  "A graph template for a SPARQL query"
  ([graph-filters-mappings]
     (with-meta (struct rdf-graph-template graph-filters-mappings) {:rdf :graph-template})))

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
  (let [uri-string (str "<" (uri-to-string (:value obj)) ">") ]
    (list uri-string
          (if (nil? (:relations obj))
            uri-string
            (reduce  (fn [fragment-a fragment-b]  (str fragment-a fragment-b))
                     ""
                     (map (fn [relation] (str uri-string " " (to-sparql relation bindings)))
                          (:relations obj)))))))

(defmethod to-sparql :blank-node [obj bindings]
  "translates a blank-node into a SPARQL query"
  (let [uri-string (str "_:" (bnode-identifier-to-string (:value obj))) ]
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
    (str "<" (uri-to-string (:value obj)) "> " object-sparql " .\n" sparql-fragment) ))

(defn resolve-bindings
  "Returns the SPARQL representation of a variable or the binded value
   if a binding for that identifier exists in the bindings hash"
  ([identifier bindings]
     (let [kw-identifier (:value identifier)]
       (if (nil? (get bindings kw-identifier))
         (str "?" (. (str kw-identifier) (substring 1 (. (str kw-identifier) (length)))))
         (let [identifier-val (get bindings kw-identifier)]
           (if (= :literal (rdf-meta identifier-val))
             (literal-to-string identifier-val)
             (str "<" (uri-to-string identifier-val) ">")))))))

(defmethod to-sparql :variable-node [obj bindings]
  "translates a uri-node into a SPARQL query"
  (let [uri-string (resolve-bindings (:value obj) bindings)]
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
                         (:nodes obj)))))

(defmethod to-sparql :optional-graph [obj bindings]
  "Translates a RDF graph to a SPARQL query"
  (str "OPTIONAL " (to-sparql (:value obj) bindings)))

(defmethod to-sparql :sparql-filter [obj bindings]
  "Translates a RDF filter to a SPARQL fragment"
  (if (nil? (get bindings (:identifier obj)))
    (str "FILTER (" (:value obj) ") .\n")
    ""))

(defmethod to-sparql :graph-template [obj bindings]
  (let [ fragments (map
                    (fn [mapping]
                      (let [ graph (:template mapping)
                            filters (:filters mapping)
                            graph-sparql (to-sparql graph bindings)
                            filters-txt (map (fn [x] (to-sparql x bindings)) filters)
                            filters-sparql (reduce
                                            (fn [fa fb] (str fa fb))
                                            ""
                                            filters-txt) ]
                        (str graph-sparql filters-sparql " }\n")))
                   (:nodes-filters obj)) ]
    (reduce (fn [a b] (str a b)) "" fragments)))

;; Quering the repository with a RDF template

(defn prepare-bindings
  ([bindings-list]
     (reduce
      merge
      {}
      (map (fn [x] (if (= #=clojure.lang.Keyword
                          (class x))
                     {}
                     x))
           bindings-list))))

(defn collect-vars
  ([bindings-list]
     (reduce
      (fn [col elem-or-nil]
        (if (nil? elem-or-nil) col (conj col elem-or-nil)))
      []
      (map (fn [x] (if (= #=clojure.lang.Keyword
                          (class x))
                     x
                     nil))
           bindings-list))))


(defn prepare-query-from-template
  ([args templates bindings]
     (let [sparql-args (str (reduce
                             (fn [acum elem] (str acum " ?" (. (str elem) (substring 1 (. (str elem) (length))))))
                             "SELECT"
                             args)
                            "\n WHERE ")
           sparql-graph (to-sparql (build-graph-template templates) bindings)]
              (str sparql-args sparql-graph))))

;;; JUST FOR TESTING ;;;
(defn build-query-from-template
  "Transforms a template into a SPARQL query"
  ([& args]
     (let [templates (last args)
           bindings-list (drop-last args)
           bindings (prepare-bindings bindings-list)
           args (collect-vars bindings-list)]
       (prepare-query-from-template args templates bindings))))
;;;;;;;;;;;;;;;;;;;;;;;;
(defn query-template-in-repository
  "Queries the repository with the provided template"
    ([& args]
     (let [connection (last args)
           templates-or-map (last (drop-last args))
           templates (if (is-rdf-meta? templates-or-map) (:nodes-filters templates-or-map) templates-or-map)
           bindings-list (drop-last 2 args)
           bindings (prepare-bindings bindings-list)
           args (collect-vars bindings-list)
           sparql-query (log :info "Quering repository with SPARQL query"
                             (prepare-query-from-template args
                                                          templates
                                                          bindings))
           sesame-query (. connection (prepareTupleQuery (. QueryLanguage SPARQL) sparql-query))]
       (let [result (. sesame-query (evaluate))]
         (loop [results []]
           (if (. result hasNext)
             (let [this-result (. result next)
                   new-result (reduce
                               (fn [acum var] (merge acum
                                                     {var (from-sesame (. this-result (getValue (. (str var) (substring 1 (. (str var) (length)))))))}))
                               {}
                               args)]
               (recur (conj results new-result)))
             results))))))

(defn query-transitive-closure-for-predicate
  "Computes the transitive closuer of a property of a subject"
  ([subject predicate connection]
     (let [template (build-graph-template
                     [{:template (build-graph
                                  [(build-variable-node :subject
                                                        [(build-relation predicate
                                                                         (build-variable-node :object))])])
                       :filters []}])
           initial-set (reduce
                        clojure.set/union
                        (set [])
                        (map
                         (fn [result] (set (list (:object result))))
                         (query-template-in-repository
                          {:subject subject} :object
                          template
                          connection)))]
       (if (empty? initial-set)
         initial-set
         (loop [closure (set [])
                working initial-set]
           (let [this-object (first working)
                 rest-working (rest working)
                 this-working (reduce
                               clojure.set/union
                               (set [])
                               (map
                                (fn [result] (set (list (:object result))))
                                (query-template-in-repository
                                 {:subject this-object} :object
                                 template
                                 connection)))
                 to-append (clojure.set/difference this-working closure)
                 new-working (clojure.set/union to-append rest-working)]
             (if (empty? new-working)
               (conj closure this-object)
               (recur (conj closure this-object) new-working))))))))



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

;; Translates a RDF template into a RDF graph with a set of bindings
(defmulti template-to-graph (fn [obj bindings] (rdf-meta obj)))

(defmethod template-to-graph :uri-node [obj bindings]
  "Transforms a uri-node from a template into a graph"
  (let [translated-relations (map (fn [r] (template-to-graph r bindings)) (:relations obj))]
    (build-uri-node obj translated-relations)))

(defmethod template-to-graph :literal-node [obj bindings]
  "Transforms a literal-node from a template into a graph"
  obj)

(defmethod template-to-graph :blank-node [obj bindings]
  "Transforms a blank-node from a template into a graph"
  (let [translated-relations (map (fn [r] (template-to-graph r bindings)) (:relations obj))]
    (build-blank-node obj translated-relations)))

(defmethod template-to-graph :relation [obj bindings]
  "Transforms a relation from a template into a graph"
  (let [translated-object (template-to-graph (:object obj) bindings)]
    (build-relation (:value obj) translated-object)))

(defmethod template-to-graph :variable-relation [obj bindings]
  "Transforms a relation from a template into a graph"
  (let [translated-object (template-to-graph (:object obj) bindings)
        variable-identifier (:value (:value obj))
        value-for-identifier (get bindings variable-identifier) ]
    (if (nil? value-for-identifier)
      (throw (Exception. (str "Unbounded variable for variable relation " variable-identifier)))
      (build-relation value-for-identifier translated-object))))

(defmethod template-to-graph :variable-node [obj bindings]
  "Transforms a relation from a template into a graph"
  (let [translated-relations (map (fn [r] (template-to-graph r bindings)) (:relations obj))
        variable-identifier (:value (:value obj))
        value-for-identifier (get bindings variable-identifier) ]
    (if (nil? value-for-identifier)
      (throw (Exception. (str "Unbounded variable for variable node " variable-identifier)))
      (let [tag (rdf-meta value-for-identifier)]
        (if (= tag :bnode-identifier)
          (build-blank-node (:value (value-for-identifier)) translated-relations)
        (if (= tag :uri)
          (build-uri-node value-for-identifier translated-relations)
        (if (= tag :literal)
          (build-literal-node value-for-identifier)
          (throw (Exception. (str "Unknown RDF graph metadata " tag))))))))))

(defmethod template-to-graph :graph [obj bindings]
  "Transforms a graph froma template into a graph"
  (let [translated-nodes (map (fn [n] (template-to-graph n bindings)) (:nodes obj))]
    (build-graph translated-nodes)))

(defmethod template-to-graph :optional-graph [obj bindings]
  "Transforms a optional graph template from a template into a graph"
  (try
   (template-to-graph obj bindings)
   (catch Exception _ (build-graph [])))) ;; if exception found, that means a lack of the optional binding
                                          ;; so we return a graph without nodes

(defmethod template-to-graph :graph-template [obj bindings]
  "Transforms a graph template from a template into a graph"
  (let [nodes-sets (reduce
                    (fn [acum graph-filter-struct]
                      (conj acum (:template graph-filter-struct)))
                    []
                    (:nodes-filters obj)) ]
    (build-graph (reduce
                  clojure.set/union
                  []
                  (map (fn [graph-from-template]
                           (:nodes (template-to-graph graph-from-template bindings))) nodes-sets)))))

;; Several multimethods that parses data into the propper RDF objects
;; meaning something valid with :rdf meatadata
(defmulti to-rdf class)

(defmethod to-rdf :default [something]
  (throw (Exception. (str "Error to-rdf, unkown dispatch for resource: " something " with class " (class something) ))))

(defmethod to-rdf #=clojure.lang.PersistentArrayMap [something]
  something)

(defmethod to-rdf #=clojure.lang.PersistentStructMap [something]
  something)

(defmethod to-rdf #=java.lang.String [something]
  (if (nil? (re-find '#"^http://" something))
    (let [parts (. something (split "\"\\^\\^"))]
      (if (= (count parts) 2) ;; has a datatype?
        (let [value (. (first parts) (substring 1))
              datatype (second parts)]
          (build-literal value (build-uri datatype)))
        (let [parts (. something (split "\"@"))]
          (if (= (count parts) 2) ;; has a language tag?
            (let [value (. (first parts) (substring 1))
                  lang (second parts)]
              (build-literal value lang))
            (build-literal something)))))
    (build-uri something)))


(clojure/comment
  "Tests"
)


(use 'clojure.contrib.test-is)

(defn repositories-register-restore!
  "A helper function for testing without losing the repository-register in the test"
  ([repositories-to-restore]
     (dosync
      (commute *repositories-registry*
               (fn [repositories-registry]
                 repositories-to-restore)))))

(defn connections-restore!
  "A helper function for testing without losing the connections registry in the test"
  ([connections-to-restore]
     (dosync
      (commute *connections*
               (fn [connections]
                 connections-to-restore)))))

(deftest test-register-repository
  (let [repo (init-memory-repository!)
        original @*repositories-registry*]
    (do (register-repository! :test repo)
        (is (= (repository :test)
               repo))
        (is (= (repository)
               repo))
        (repositories-register-restore! original))))

(deftest test-register-default-repository
  (let [repo (init-memory-repository!)
        original @*repositories-registry*]
    (do (register-repository! repo)
        (is (= (repository :default)
               repo))
        (is (= (repository)
               repo))
        (repositories-register-restore! original))))

(deftest test-connection
  (let [repo (init-memory-repository!)
        original @*repositories-registry*
        original-conn @*connections*]
    (do (register-repository! :test repo)
        (let [conn (connection! :test)]
          (is (not (= conn nil)))
          (is (= (class conn)
                  #=org.openrdf.repository.sail.SailRepositoryConnection)))
        (close-connection! :test)
        (repositories-register-restore! original)
        (connections-restore! original-conn))))


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

(deftest test-rdf-type
  (is (= (rdf-type)
         {:prefix :rdf, :value "type"})))

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
         "\"a\"^^http://www.w3.org/2001/XMLSchema#string")))

(deftest test-literal-to-string-2
  (is (= (literal-to-string (build-literal "a" "es_es"))
         "\"a\"@es_es")))

(deftest test-literal-to-string-3
  (is (= (literal-to-string (build-literal "a" (build-uri "http://test.com#datatype")))
         "\"a\"^^http://test.com#datatype")))

(deftest test-relation-1
  (is (= (build-relation (rdf-ns :rdf) "test" (build-literal "testPredicate"))
         {:value {:prefix :rdf, :value "test"}, :object {:value {:value "testPredicate", :datatype {:prefix :xsd, :value "string"}, :lang ""} :relations []}})))

(deftest test-relation-meta-1
  (is (= (meta (build-relation (rdf-ns :rdf) "test" (build-literal "testPredicate")))
         {:rdf :relation})))

(deftest test-relation-2
  (is (= (build-relation (build-uri (rdf-ns :rdf) "test") (build-literal "testPredicate"))
         {:value {:prefix :rdf, :value "test"}, :object {:value {:value "testPredicate", :datatype {:prefix :xsd, :value "string"}, :lang ""} :relations []}})))

(deftest test-relation-meta-2
  (is (= (meta (build-relation (build-uri (rdf-ns :rdf) "test") (build-literal "testPredicate")))
         {:rdf :relation})))

(deftest test-relation-3
  (is (= (build-relation (build-uri "http://test.com#test_1") (build-literal "testPredicate"))
         {:value {:prefix "", :value "http://test.com#test_1"}, :object {:value {:value "testPredicate", :datatype {:prefix :xsd, :value "string"}, :lang ""} :relations []}})))

(deftest test-relation-meta-3
  (is (= (meta (build-relation (build-uri "http://teste.com#test_1") (build-literal "testPredicate")))
         {:rdf :relation})))

(deftest test-build-node-1
  (is (= (build-node "test" :test [(build-relation "test" (build-uri "test"))])
         {:value "test", :relations [{:value {:prefix "", :value "test"}, :object {:value {:prefix "", :value "test"} :relations []}}]})))

(deftest test-build-node-2
  (is (= (build-node "test" :test
                     [(build-relation "test" (build-uri "test"))
                      (build-relation "test2" (build-uri "hola"))])
         {:value "test", :relations [{:value {:prefix "", :value "test"}, :object {:value {:prefix "", :value "test"} :relations []}}
                                     {:value {:prefix "", :value "test2"}, :object {:value {:prefix "", :value "hola"} :relations []}}]})))

(deftest test-build-node-3
  (is (= (meta (build-node "test" :test [(build-relation "test" (build-uri "test"))]))
         {:rdf :test})))

(deftest test-build-node-4
  (is (= (build-uri-node :a "test" [(build-relation "test" (build-uri-node :b "test" []))])
         {:relations [{:value {:prefix "", :value "test"}
                       :object {:relations []
                                :value {:prefix :b, :value "test"}}}]
          :value {:prefix :a, :value "test"}})))

(deftest test-build-uri-node-1
  (is (= (build-uri-node "http://test.com")
         {:relations []
          :value {:prefix "", :value "http://test.com"}})))

(deftest test-build-uri-node-2
  (is (= (build-uri-node (build-uri :rdf "a"))
         {:relations []
          :value {:prefix :rdf, :value "a"}})))

(deftest test-build-uri-node-3
  (is (= (build-uri-node :rdf "a")
         {:relations []
          :value {:prefix :rdf, :value "a"}})))

(deftest test-build-node-literal-1
  (is (= (build-literal-node "test")
       {:value {:value "test", :datatype {:prefix :xsd, :value "string"}, :lang ""}, :relations []})))

(deftest test-blank-node-1
  (is (= (build-blank-node "test" [])
         {:value {:value "test"}, :relations []})))

(deftest test-blank-node-2
  (is (= (meta (build-blank-node "test" []))
         {:rdf :blank-node})))

(deftest test-sesame-translate-uri
  (let [repo (init-memory-repository!)
        conn (. repo (getConnection))]
    (do
      (is (= (. (sesame-translate (build-uri (rdf-ns :rdf) "test") (. conn (getValueFactory)))
                (toString))
             (uri-to-string (build-uri (rdf-ns :rdf) "test"))))
      (. conn (close)))))

(deftest test-sesame-translate-literal-1
  (let [repo (init-memory-repository!)
        conn (. repo (getConnection))]
    (do
      (is (= (. (sesame-translate (build-literal "test") (. conn (getValueFactory)))
                (getLabel))
             (:value (build-literal "test"))))
      (. conn (close)))))

(deftest test-sesame-translate-literal-2
  (let [repo (init-memory-repository!)
        conn (. repo (getConnection))]
    (do
      (is (= (. (sesame-translate (build-literal "test") (. conn (getValueFactory)))
                (toString))
             "\"test\"^^<http://www.w3.org/2001/XMLSchema#string>"))
      (. conn (close)))))

(deftest test-sesame-translate-relation-1
  (let [repo (init-memory-repository!)
        conn (. repo (getConnection))]
    (do
      (is (= (. (sesame-translate (build-relation (rdf-ns :rdf) "test" (build-literal "testPredicate")) (. conn (getValueFactory)))
                (toString))
             "{:value #=(org.openrdf.sail.memory.model.MemURI. \"http://www.w3.org/1999/02/22-rdf-syntax-ns#test\"), :object {:value #=(org.openrdf.sail.memory.model.MemLiteral. \"\\\"testPredicate\\\"^^<http://www.w3.org/2001/XMLSchema#string>\"), :relations nil}}"))
      (. conn (close)))))

(deftest test-sesame-translate-relation-2
  (let [repo (init-memory-repository!)
        conn (. repo (getConnection))]
    (do
      (is (= (meta (sesame-translate (build-relation (rdf-ns :rdf) "test" (build-literal "testPredicate")) (. conn (getValueFactory))))
             {:rdf :relation}))
      (. conn (close)))))

(deftest test-sesame-translate-uri-node-1
  (let [repo (init-memory-repository!)
        conn (. repo (getConnection))]
    (do
      (is (= (. (sesame-translate (build-uri-node :rdf "test" [(build-relation :rdf "test" (build-uri-node :rdf "test" []))]) (. conn (getValueFactory)))
                (toString))
             "{:value #=(org.openrdf.sail.memory.model.MemURI. \"http://www.w3.org/1999/02/22-rdf-syntax-ns#test\"), :relations ({:value #=(org.openrdf.sail.memory.model.MemURI. \"http://www.w3.org/1999/02/22-rdf-syntax-ns#test\"), :object {:value #=(org.openrdf.sail.memory.model.MemURI. \"http://www.w3.org/1999/02/22-rdf-syntax-ns#test\"), :relations nil}})}"))
      (. conn (close)))))

(deftest test-sesame-translate-uri-node-2
  (let [repo (init-memory-repository!)
        conn (. repo (getConnection))]
    (do
      (is (= (meta (sesame-translate (build-uri-node :rdf "test" [(build-relation :rdf "test" (build-uri-node :rdf "test" []))]) (. conn (getValueFactory))))
             {:rdf :uri-node}))
      (. conn (close)))))

(deftest test-sesame-translate-literal-node-1
  (let [repo (init-memory-repository!)
        conn (. repo (getConnection))]
    (do
      (is (= (. (sesame-translate (build-literal-node "test") (. conn (getValueFactory)))
                (toString))
             "{:value #=(org.openrdf.sail.memory.model.MemLiteral. \"\\\"test\\\"^^<http://www.w3.org/2001/XMLSchema#string>\"), :relations nil}"))
      (. conn (close)))))

(deftest test-sesame-translate-literal-node-2
  (let [repo (init-memory-repository!)
        conn (. repo (getConnection))]
    (do
      (is (= (meta (sesame-translate (build-literal-node "test") (. conn (getValueFactory))))
             {:rdf :literal-node}))
      (. conn (close)))))

(deftest test-sesame-translate-triplet-1
  (let [repo (init-memory-repository!)
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
  (let [repo (init-memory-repository!)
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
         "{ ?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#testa> .\n<http://www.w3.org/1999/02/22-rdf-syntax-ns#test-subjectb> ?y <http://www.w3.org/1999/02/22-rdf-syntax-ns#testb> .\n")))

(deftest test-to-sparql-2
  (is (= (to-sparql (build-uri-node :rdf "testb" [])
                    {})
         '("<http://www.w3.org/1999/02/22-rdf-syntax-ns#testb>" ""))))

(deftest test-to-sparql-3
  (is (= (to-sparql (build-relation :rdf "testb" (build-uri-node :rdf "testc" []))
                    {})
         "<http://www.w3.org/1999/02/22-rdf-syntax-ns#testb> <http://www.w3.org/1999/02/22-rdf-syntax-ns#testc> .\n")))

(deftest test-to-sparql-4
  (is (= (to-sparql (build-optional-graph
                       [(build-variable-node :x
                                        [(build-relation :rdf "relation-1"
                                                         (build-uri-node :rdf "testa" []))])
                        (build-uri-node :rdf "test-subjectb"
                                        [(build-variable-relation :y
                                                         (build-uri-node :rdf "testb" []))])])
                    {})
         "OPTIONAL { ?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#testa> .\n<http://www.w3.org/1999/02/22-rdf-syntax-ns#test-subjectb> ?y <http://www.w3.org/1999/02/22-rdf-syntax-ns#testb> .\n")))

(deftest test-to-sparql-5
  (is (= (to-sparql (build-optional-graph
                       [(build-variable-node :x
                                        [(build-relation :rdf "relation-1"
                                                         (build-literal-node "test" "es_ES"))])
                        (build-uri-node :rdf "test-subjectb"
                                        [(build-variable-relation :y
                                                         (build-uri-node :rdf "testb" []))])])
                    {})
         "OPTIONAL { ?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-1> \"test\"@es_ES .\n<http://www.w3.org/1999/02/22-rdf-syntax-ns#test-subjectb> ?y <http://www.w3.org/1999/02/22-rdf-syntax-ns#testb> .\n")))

(deftest test-to-sparql-6
  (is (= (to-sparql (build-optional-graph
                       [(build-variable-node :x
                                        [(build-relation :rdf "relation-1"
                                                         (build-literal-node "test" "es_ES"))])
                        (build-blank-node "a"
                                        [(build-variable-relation :y
                                                         (build-uri-node :rdf "testb" []))])])
                    {})
         "OPTIONAL { ?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-1> \"test\"@es_ES .\n_:a ?y <http://www.w3.org/1999/02/22-rdf-syntax-ns#testb> .\n")))

(deftest test-to-sparql-7
  (is (= (to-sparql (build-graph-template
                     [{:template (build-optional-graph
                                  [(build-variable-node :x
                                                        [(build-relation :rdf "relation-1"
                                                                         (build-literal-node "test" "es_ES"))])
                                   (build-blank-node "a"
                                                     [(build-variable-relation :y
                                                                               (build-uri-node :rdf "testb" []))])])
                       :filters [(build-filter :x "?x < 25")
                                 (build-filter :y "isURI(?y)")]}])
                     {})
         "OPTIONAL { ?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-1> \"test\"@es_ES .\n_:a ?y <http://www.w3.org/1999/02/22-rdf-syntax-ns#testb> .\nFILTER (?x < 25) .\nFILTER (isURI(?y)) .\n }\n")))

(deftest test-to-sparql-8
  (is (= (to-sparql (build-graph-template
                     [{:template (build-optional-graph
                                  [(build-variable-node :x
                                                        [(build-relation :rdf "relation-1"
                                                                         (build-literal-node "test" "es_ES"))])
                                   (build-blank-node "a"
                                                     [(build-variable-relation :y
                                                                               (build-uri-node :rdf "testb" []))])])
                       :filters [(build-filter :x "?x < 25")
                                 (build-filter :y "isURI(?y)")]}])
                     {:x (build-uri :rdf "a")})
         "OPTIONAL { <http://www.w3.org/1999/02/22-rdf-syntax-ns#a> <http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-1> \"test\"@es_ES .\n_:a ?y <http://www.w3.org/1999/02/22-rdf-syntax-ns#testb> .\nFILTER (isURI(?y)) .\n }\n")))

(deftest test-to-sparql-9
  (is (= (to-sparql
          (build-graph-template
           [{:template (build-graph
                        [(build-variable-node :x
                                              [ (build-relation :rdf "test-relation"
                                                                (build-variable-node :y
                                                                                     []))])])
             :filters []}])
          {})
         "{ ?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#test-relation> ?y .\n }\n")))

(deftest test-prepare-bindings-1
  (is (= (prepare-bindings '(:x :y {:z "test"}))
         {:z "test"})))

(deftest test-prepare-bindings-2
  (is (= (prepare-bindings '(:x {:w "hola"} :y {:z "test"}))
         {:w "hola", :z "test"})))

(deftest test-prepare-bindings-3
  (is (= (prepare-bindings '(:x :y :z))
         {})))

(deftest test-collect-vars-1
  (is (= (set (collect-vars '(:x {:y 1} :z)))
         (set [:x :z]))))

(use 'com.agh.utils)
(deftest query-repository-1
  (is (= (let [repo (init-memory-repository!)
               conn (. repo (getConnection))
               graph (build-graph
                      [(build-uri-node "http://test.com/test-subjecta/whatever"
                                       [(build-relation :rdf "relation-1"
                                                        (build-blank-node "testa" []))])
                       (build-uri-node :rdf "test-subjectb"
                                       [(build-relation :rdf "relation-2"
                                                        (build-blank-node "testb" []))])])
               sparql (build-query-from-template
                       :y
                       [{:template (build-graph
                                    [(build-variable-node
                                      :x [ (build-relation
                                            :rdf "relation-1" (build-variable-node
                                                               :y []))])])
                         :filters []}])
               query (. conn (prepareTupleQuery (. QueryLanguage SPARQL) sparql))]
           (do
             (write-graph-in-repository graph conn)
             (let [ result  (. query (evaluate))
                     binded (. (. result next) (getValue "y")) ]
               (. conn (close))
               (from-sesame binded))))
         (build-bnode-identifier "testa"))))

(deftest query-repository-2
  (is (= (let [repo (init-memory-repository!)
               conn (. repo (getConnection))
               graph (build-graph
                      [(build-uri-node "http://test.com/test-subjecta/whatever"
                                       [(build-relation :rdf "relation-1"
                                                        (build-literal-node "testa"))])
                       (build-uri-node :rdf "test-subjectb"
                                       [(build-relation :rdf "relation-2"
                                                        (build-blank-node "testb" []))])])
               sparql (build-query-from-template
                       :x
                       [{:template (build-graph
                                    [(build-variable-node
                                      :x [ (build-relation
                                            :rdf "relation-1" (build-variable-node
                                                               :y []))])])
                         :filters []}])
               query (. conn (prepareTupleQuery (. QueryLanguage SPARQL) sparql))]
           (do
             (write-graph-in-repository graph conn)
             (let [ result  (. query (evaluate))
                     binded (. (. result next) (getValue "x")) ]
               (. conn (close))
               (uri-to-string (from-sesame binded)))))
         "http://test.com/test-subjecta/whatever")))

(deftest query-repository-3
  (is (= (let [repo (init-memory-repository!)
               conn (. repo (getConnection))
               graph (build-graph
                      [(build-uri-node "http://test.com/test-subjecta/whatever"
                                       [(build-relation :rdf "relation-1"
                                                        (build-literal-node "testa"))])
                       (build-uri-node :rdf "test-subjectb"
                                       [(build-relation :rdf "relation-2"
                                                        (build-blank-node "testb" []))])])
               sparql (build-query-from-template
                       :y
                       [{:template (build-graph
                                    [(build-variable-node
                                      :x [ (build-relation
                                            :rdf "relation-1" (build-variable-node
                                                               :y []))])])
                         :filters []}])
               query (. conn (prepareTupleQuery (. QueryLanguage SPARQL) sparql))]
           (do
             (write-graph-in-repository graph conn)
             (let [ result  (. query (evaluate))
                     binded (. (. result next) (getValue "y")) ]
               (. conn (close))
               (literal-to-string (from-sesame binded)))))
         (literal-to-string (build-literal "testa")))))


(deftest query-in-repository-1
  (is (= (let [repo (init-memory-repository!)
               conn (. repo (getConnection))
               graph (build-graph
                      [(build-uri-node "http://test.com/test-subjecta/whatever"
                                       [(build-relation :rdf "relation-1"
                                                        (build-literal-node "testa"))])
                       (build-uri-node :rdf "test-subjectb"
                                       [(build-relation :rdf "relation-2"
                                                        (build-blank-node "testb" []))])])]
           (do
             (write-graph-in-repository graph conn)
             (let [result (query-template-in-repository
                           :y
                           [{:template (build-graph
                                        [(build-variable-node
                                          :x [ (build-relation
                                                :rdf "relation-1" (build-variable-node
                                                                   :y []))])])
                             :filters []}]
                           conn)]
               (do
                 (. conn (close))
                 result))))
         [{:y {:value "testa", :datatype {:prefix "", :value "http://www.w3.org/2001/XMLSchema#string"}, :lang ""}}])))

(deftest query-in-repository-2
  (is (= (set (let [repo (init-memory-repository!)
                    conn (. repo (getConnection))
                    graph (build-graph
                           [(build-uri-node "http://test.com/test-subjecta/whatever"
                                            [(build-relation :rdf "relation-1"
                                                             (build-literal-node "testa1"))
                                             (build-relation :rdf "relation-2"
                                                             (build-literal-node "testa2"))])
                            (build-uri-node :rdf "test-subjectb"
                                            [(build-relation :rdf "relation-2"
                                                             (build-blank-node "testb" []))])])]
                (do
                  (write-graph-in-repository graph conn)
                  (let [result (query-template-in-repository
                                :y
                                [{:template (build-graph
                                             [(build-uri-node "http://test.com/test-subjecta/whatever"
                                                              [ (build-variable-relation
                                                                 :x (build-variable-node
                                                                     :y []))])])
                                  :filters []}]
                                conn)]
                    (do
                      (. conn (close))
                      result)))))
         (set [{:y {:value "testa2", :datatype {:prefix "", :value "http://www.w3.org/2001/XMLSchema#string"}, :lang ""}}
               {:y {:value "testa1", :datatype {:prefix "", :value "http://www.w3.org/2001/XMLSchema#string"}, :lang ""}}]))))

(deftest query-in-repository-3
  (is (= (set (let [repo (init-memory-repository!)
                    conn (. repo (getConnection))
                    graph (build-graph
                           [(build-uri-node "http://test.com/test-subjecta/whatever"
                                            [(build-relation :rdf "relation-1"
                                                             (build-literal-node "testa1"))
                                             (build-relation :rdf "relation-2"
                                                             (build-literal-node "testa2"))])
                            (build-uri-node :rdf "test-subjectb"
                                            [(build-relation :rdf "relation-2"
                                                             (build-blank-node "testb" []))])])]
                (do
                  (write-graph-in-repository graph conn)
                  (let [result (query-template-in-repository
                                :x :y
                                [{:template (build-graph
                                             [(build-uri-node "http://test.com/test-subjecta/whatever"
                                                              [ (build-variable-relation
                                                                 :x (build-variable-node
                                                                     :y []))])])
                                  :filters []}]
                                conn)]
                    (do
                      (. conn (close))
                      result)))))
         (set [{:x {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-2"}
                :y {:value "testa2", :datatype {:prefix "", :value "http://www.w3.org/2001/XMLSchema#string"}, :lang ""}}
               {:x {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-1"}
                :y {:value "testa1", :datatype {:prefix "", :value "http://www.w3.org/2001/XMLSchema#string"}, :lang ""}}]))))

(deftest query-in-repository-4
  (is (= (set (let [repo (init-memory-repository!)
                    conn (. repo (getConnection))
                    graph (build-graph
                           [(build-uri-node "http://test.com/test-subjecta/whatever"
                                            [(build-relation :rdf "relation-1"
                                                             (build-literal-node "testa1"))
                                             (build-relation :rdf "relation-2"
                                                             (build-literal-node "testa2"))])
                            (build-uri-node :rdf "test-subjectb"
                                            [(build-relation :rdf "relation-2"
                                                             (build-blank-node "testb" []))])])]
                (do
                  (write-graph-in-repository graph conn)
                  (let [result (query-template-in-repository
                                :x :y :z
                                [{:template (build-graph
                                             [(build-variable-node :x
                                                                   [ (build-variable-relation
                                                                      :y (build-variable-node
                                                                          :z []))])])
                                  :filters []}]
                                conn)]
                    (do
                      (. conn (close))
                      result)))))
         (set [{:z {:value "testb"}
                :x {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#test-subjectb"}
                :y {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-2"}}
               {:z {:value "testa2", :datatype {:prefix "", :value "http://www.w3.org/2001/XMLSchema#string"}, :lang ""}
                :x {:prefix "", :value "http://test.com/test-subjecta/whatever"}
                :y {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-2"}}
               {:z {:value "testa1", :datatype {:prefix "", :value "http://www.w3.org/2001/XMLSchema#string"}, :lang ""}
                :x {:prefix "", :value "http://test.com/test-subjecta/whatever"}
                :y {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-1"}}]))))

(deftest query-in-repository-5
  (is (= (let [repo (init-memory-repository!)
               conn (. repo (getConnection))
               graph (build-graph
                      [(build-uri-node :rdf "test-subjectb"
                                       [(build-relation :rdf "relation-2"
                                                        (build-blank-node "testb" []))])
                       (build-uri-node :rdf "test-subjectb"
                                       [(build-relation :rdf "relation-2"
                                                        (build-blank-node "testb" []))])])]
           (do
             (write-graph-in-repository graph conn)
             (let [result (query-template-in-repository
                           :x :y :z
                           [{:template (build-graph
                                        [(build-variable-node :x
                                                              [ (build-variable-relation
                                                                 :y (build-variable-node
                                                                     :z []))])])
                             :filters []}]
                           conn)]
               (do
                 (. conn (close))
                 result))))
         [{:z {:value "testb"}
           :x {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#test-subjectb"}
           :y {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-2"}}])))


(deftest test-template-to-graph
  (is (= (let [repo (init-memory-repository!)
               conn (. repo getConnection)
               graph (build-graph
                      [(build-uri-node "http://test.com/test-subjecta/whatever"
                                       [(build-relation :rdf "relation-1"
                                                        (build-literal-node "testa1"))
                                        (build-relation :rdf "relation-2"
                                                        (build-literal-node "testa2"))])
                       (build-uri-node :rdf "test-subjectb"
                                       [(build-relation :rdf "relation-2"
                                                        (build-blank-node "testb" []))])])
               template (build-graph-template [{:template (build-graph
                                                           [(build-variable-node :x
                                                                                 [ (build-variable-relation
                                                                                    :y (build-variable-node
                                                                                        :z []))])])
                                                :filters []}])]
           (do
             (write-graph-in-repository graph conn)
             (let [result (query-template-in-repository
                           :x :y :z
                           template
                           conn)]
               (do
                 (. conn close)
                 (set result)))))
         (set [{:z {:value "testa2", :datatype {:prefix "", :value "http://www.w3.org/2001/XMLSchema#string"}, :lang ""}
                :x {:prefix "", :value "http://test.com/test-subjecta/whatever"}
                :y {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-2"}}
               {:z {:value "testa1", :datatype {:prefix "", :value "http://www.w3.org/2001/XMLSchema#string"}, :lang ""}
                :x {:prefix "", :value "http://test.com/test-subjecta/whatever"}
                :y {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-1"}}
               {:z {:value "testb"}
                :x {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#test-subjectb"}
                :y {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#relation-2"}}]))))

(deftest test-query-transitive-closure-1
  (is (= (let [repo (init-memory-repository!)
               conn (. repo getConnection)
               graph (build-graph
                      [(build-uri-node "http://test.com/a"
                                       [(build-relation :rdf "relation-1"
                                                        (build-uri-node "http://test.com/b"))])
                       (build-uri-node "http://test.com/b"
                                       [(build-relation :rdf "relation-1"
                                                        (build-uri-node "http://test.com/c"))
                                        (build-relation :rdf "relation-1"
                                                        (build-uri-node "http://test.com/c"))
                                        (build-relation :rdf "relation-1"
                                                        (build-uri-node "http://test.com/d"))])]) ]
           (do
             (write-graph-in-repository graph conn)
             (let [result (query-transitive-closure-for-predicate "http://test.com/a" (build-uri :rdf "relation-1") conn)]
               (do
                 (. conn close)
                 (set result)))))
         (set [{:prefix "", :value "http://test.com/b"}
               {:prefix "", :value "http://test.com/d"}
               {:prefix "", :value "http://test.com/c"}]))))

(deftest test-query-transitive-closure-2
  (is (= (let [repo (init-memory-repository!)
               conn (. repo getConnection)
               graph (build-graph
                      [(build-uri-node "http://test.com/a"
                                       [(build-relation :rdf "relation-1"
                                                        (build-uri-node "http://test.com/b"))])
                       (build-uri-node "http://test.com/b"
                                       [(build-relation :rdf "relation-1"
                                                        (build-uri-node "http://test.com/c"))
                                        (build-relation :rdf "relation-1"
                                                        (build-uri-node "http://test.com/d"))])]) ]
           (do
             (write-graph-in-repository graph conn)
             (let [result (query-transitive-closure-for-predicate "http://test.com/d" (build-uri :rdf "relation-1") conn)]
               (do
                 (. conn close)
                 (set result)))))
         (set []))))

(deftest test-to-rdf-1
  (is (= (to-rdf {:a 1})
         {:a 1})))

(deftest test-to-rdf-2
  (is (= (to-rdf "http://test.com")
         {:prefix "", :value "http://test.com"})))

(deftest test-to-rdf-3
  (is (= (to-rdf "hooola")
         {:value "hooola", :datatype {:prefix :xsd, :value "string"}, :lang ""})))

(deftest test-to-rdf-4
  (is (= (to-rdf "\"hooola\"^^test")
         {:value "hooola", :datatype {:prefix "", :value "test"}, :lang ""})))

(deftest test-to-rdf-5
  (is (= (to-rdf "\"hooola\"@es_ES")
         {:value "hooola", :datatype {:prefix :xsd, :value "string"}, :lang "es_ES"})))