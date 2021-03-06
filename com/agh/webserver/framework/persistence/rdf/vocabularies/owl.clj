(comment
  "Functions for the description of OWL ontologies and instances"
)

;;
;; @author Antonio Garrote Hernandez
;;

(ns com.agh.webserver.framework.persistence.rdf.vocabularies.owl)

(use 'com.agh.monads)
(use 'com.agh.monads.maybe)
(use 'com.agh.webserver.framework.persistence.rdf)
(use 'com.agh.webserver.framework.persistence.rdf.vocabularies.xsd)
(use 'com.agh.webserver.framework.persistence.rdf.vocabularies.rdfs)
(use 'com.agh.webserver.framework.logger)
(use 'com.agh.utils)

;; OWL vocabulary

(defmethod rdf-ns :owl [x]
  "XMLSchema namespace: http://www.w3.org/2002/07/owl#"
  (struct uri "http://www.w3.org/2002/07/owl#" :owl))

(defn owl-Thing []
  (build-uri (rdf-ns :owl) "Thing"))

(defn owl-equivalentProperty []
  (build-uri (rdf-ns :owl) "equivalentProperty"))

(defn owl-equivalentClass []
  (build-uri (rdf-ns :owl) "equivalentClass"))

;; OWL structures

(defstruct owl-class
  :name ;; the unique name of the class
  :uri ;; the unique URI of the class
  :subclass-of ;; a list of superclasses
  :datatype-properties ;; datatype properties of this class
  :object-properties ;; object properties of this class
  :repository-name ;; name of the repository where the TBox with this class is stored
)


(defstruct owl-datatype-property
  :name ;; the unique name of the property
  :uri ;; the unique URI of the property
  :range ;; the URI of the datatype associated to this property
  :equivalent-properties ;; a list equivalent properties
  :repository-name ;; name of the repository where the TBox with this property is stored
)


(defstruct owl-object-property
  :name ;; the unique name of the property
  :uri ;; the unique URI of the property
  :equivalent-properties ;; a list of equivalent properties
  :repository-name ;; name of the repository where the TBox with this property is stored
)

;;
;; TBox description
;;

(defn describe-tbox
  "Gathers a set of tbox descriptions"
  ([ & descriptions]
     (let [nodes (map (fn [description]
                        (if (= (rdf-meta description) :graph)
                          (:nodes description)
                          (list description)))
                      descriptions)]
       (build-graph (reduce clojure.set/union
                            []
                            nodes)))))
(defn describe-owl-class
  "Generates a RDF graph describing the class"
  ([prefix value]
     (build-uri-node prefix value
                     [(build-relation (rdf-type) (build-uri-node :owl "Class" []))
                      (build-relation (rdfs-subClassOf) (build-uri-node (owl-Thing) []))]))
  ([value]
     (build-uri-node value
                     [(build-relation (rdf-type) (build-uri-node :owl "Class" []))
                      (build-relation (rdfs-subClassOf) (build-uri-node (owl-Thing) []))])))

(defn describe-owl-subclass
  "Generates a RDF graph describing that child-class is subclass of parent-class"
  ([child-prefix child-value parent-prefix parent-value]
     (build-uri-node child-prefix child-value
                     [(build-relation (rdfs-subClassOf) (build-uri-node parent-prefix parent-value []))]))
  ([value-a value-b value-c]
     (if (= (class value-a) #=clojure.lang.Keyword)
       (build-uri-node value-a value-b
                     [(build-relation (rdfs-subClassOf) (build-uri-node value-c []))])
       (build-uri-node value-a
                     [(build-relation (rdfs-subClassOf) (build-uri-node value-b value-c []))])))
  ([child-value parent-value]
     (build-uri-node child-value
                     [(build-relation (rdfs-subClassOf) (build-uri-node parent-value []))])))


(defn describe-owl-datatype-property
  "Generates a RDF graph describing a datatype property"
  ([value datatype]
     (build-uri-node value
                     [(build-relation (rdf-type) (build-uri-node :owl "DatatypeProperty" []))
                      (build-relation (rdfs-range) (build-uri-node datatype []))]))
  ([prefix value datatype]
     (build-uri-node prefix value
                     [(build-relation (rdf-type) (build-uri-node :owl "DatatypeProperty" []))
                      (build-relation (rdfs-range) (build-uri-node datatype []))])))

(defn describe-owl-object-property
  "Generates a RDF graph describing an object property"
  ([value]
     (build-uri-node value
                     [(build-relation (rdf-type) (build-uri-node :owl "ObjectProperty" []))]))
  ([prefix value]
     (build-uri-node prefix value
                     [(build-relation (rdf-type) (build-uri-node :owl "ObjectProperty" []))])))

(defn describe-owl-object-property-range
  "Generates a RDF graph describing the range of an object property"
  ([value-property value-range]
     (build-uri-node value-property
                     [(build-relation (rdfs-range) (build-uri-node value-range))]))
  ([value-a value-b value-c]
     (if (= (class value-a) #=clojure.lang.Keyword)
       (build-uri-node value-a value-b
                       [(build-relation (rdfs-range) (build-uri-node value-c))])
       (build-uri-node value-a
                       [(build-relation (rdfs-range) (build-uri-node value-b value-c))])))
  ([prefix-property value-property prefix-object value-object]
     (build-uri-node prefix-property value-property
                     [(build-relation (rdfs-range) (build-uri-node prefix-object value-object))])))

(defn describe-owl-object-property-and-range
  "Generates a RDF graph describing an object property"
  ([property range]
     (do (describe-owl-object-property property)
         (describe-owl-object-property-range property range)))
  ([value-a value-b value-c]
     (if (= (class value-a) #=clojure.lang.Keyword)
       (do (describe-owl-object-property value-a value-b)
           (describe-owl-object-property-range value-a value-b value-c))
       (do (describe-owl-object-property value-a)
           (describe-owl-object-property-range value-a value-b value-c))))
  ([property-prefix property-value range-prefix range-value]
     (do (describe-owl-object-property property-prefix property-value)
         (describe-owl-object-property-range property-prefix property-value range-prefix range-value))))


(defn describe-owl-equivalent-properties
  "Generates a RDF graph describing equivalency between properties"
  ([prefix-a value-a prefix-b value-b]
     (build-graph
      [ (build-uri-node prefix-a value-a
                        [(build-relation (owl-equivalentProperty) (build-uri-node prefix-b value-b))])
        (build-uri-node prefix-b value-b
                        [(build-relation (owl-equivalentProperty) (build-uri-node prefix-a value-a))]) ]))
  ([value-a value-b value-c]
     (if (= (class value-a) #=clojure.lang.Keyword)
       (build-graph
        [ (build-uri-node value-c
                          [(build-relation (owl-equivalentProperty) (build-uri-node value-a value-b []))])
          (build-uri-node value-a value-b
                          [(build-relation (owl-equivalentProperty) (build-uri-node value-c []))]) ])
       (build-graph
        [ (build-uri-node value-b value-c
                          [(build-relation (owl-equivalentProperty) (build-uri-node value-a []))])
          (build-uri-node value-a
                          [(build-relation (owl-equivalentProperty) (build-uri-node value-b value-c []))]) ])))
  ([value-a value-b]
     (build-graph
      [ (build-uri-node value-a
                        [(build-relation (owl-equivalentProperty) (build-uri-node value-b []))])
        (build-uri-node value-b
                        [(build-relation (owl-equivalentProperty) (build-uri-node value-a []))]) ])))


;; not semantic support for equivalent classes
(defn describe-owl-equivalent-classes
  "Generates a RDF graph describing equivalency between classes"
  ([prefix-a value-a prefix-b value-b]
     (build-graph
      [ (build-uri-node prefix-a value-a
                        [(build-relation (owl-equivalentClass) (build-uri-node prefix-b value-b))])
        (build-uri-node prefix-b value-b
                        [(build-relation (owl-equivalentClass) (build-uri-node prefix-a value-a))]) ]))
  ([value-a value-b value-c]
     (if (= (class value-a) #=clojure.lang.Keyword)
       (build-graph
        [ (build-uri-node value-c
                          [(build-relation (owl-equivalentClass) (build-uri-node value-a value-b []))])
          (build-uri-node value-a value-b
                          [(build-relation (owl-equivalentClass) (build-uri-node value-c []))]) ])
       (build-graph
        [ (build-uri-node value-b value-c
                          [(build-relation (owl-equivalentClass) (build-uri-node value-a []))])
          (build-uri-node value-a
                          [(build-relation (owl-equivalentClass) (build-uri-node value-b value-c []))]) ])))
  ([value-a value-b]
     (build-graph
      [ (build-uri-node value-a
                        [(build-relation (owl-equivalentClass) (build-uri-node value-b []))])
        (build-uri-node value-b
                        [(build-relation (owl-equivalentClass) (build-uri-node value-a []))]) ])))

(defn describe-owl-class-has-property
  "Generates a RDF graph describing that a class is the domain for a property"
  ([class-prefix class-value property-prefix property-value]
      (build-uri-node property-prefix property-value
                      [(build-relation (rdfs-domain) (build-uri-node class-prefix class-value))]))
  ([value-a value-b value-c]
     (if (= (class value-a) #=clojure.lang.Keyword)
        (build-uri-node value-c
                          [(build-relation (rdfs-domain) (build-uri-node value-a value-b []))])
        (build-uri-node value-b value-c
                          [(build-relation (rdfs-domain) (build-uri-node value-a []))])))
  ([class-value property-value]
      (build-uri-node property-value
                        [(build-relation (rdfs-domain) (build-uri-node class-value []))])))


;;
;; TBox retrieval
;;

(defn retrieve-owl-superclasses-for-class
  "Returns the list of URIs of the superclasses of a class"
  ([class-uri]
     (retrieve-owl-superclasses-for-class class-uri (connection! :default)))
  ([class-uri repository-connection]
     (set
      (filter (fn [prop] (not= (uri-to-string class-uri) (uri-to-string prop)))
              (query-transitive-closure-for-predicate class-uri
                                                      (uri-to-string (rdfs-subClassOf))
                                                      (connection! repository-connection))))))


(defn retrieve-owl-datatype-properties-for-class
  "Returns the list of URIs of the datatype properties for a class"
  ([class-uri]
     (retrieve-owl-datatype-properties-for-class class-uri (connection! :default)))
  ([class-uri repository-connection]
     (let [template (build-graph-template
                     [{:template (build-graph
                                  [(build-variable-node :property
                                                        [(build-relation (rdfs-domain)
                                                                         (build-uri-node
                                                                          class-uri []))])
                                   (build-variable-node :property
                                                        [(build-relation (rdf-type)
                                                                         (build-uri-node
                                                                          :owl "DatatypeProperty"))])])
                       :filters []}])]
           (map (fn [result] (:property result))
                (query-template-in-repository
                 :property
                 template
                 (connection! repository-connection))))))

(defn retrieve-owl-object-properties-for-class
  "Returns the list of URIs of the datatype properties for a class"
  ([class-uri]
     (retrieve-owl-object-properties-for-class class-uri (connection! :default)))
  ([class-uri repository-connection]
     (let [template (build-graph-template
                     [{:template (build-graph
                                  [(build-variable-node :property
                                                        [(build-relation (rdfs-domain)
                                                                         (build-uri-node
                                                                          class-uri []))])
                                   (build-variable-node :property
                                                        [(build-relation (rdf-type)
                                                                         (build-uri-node
                                                                          :owl "ObjectProperty"))])])
                       :filters []}])]
           (map (fn [result] (:property result))
                (query-template-in-repository
                 :property
                 template
                 (connection! repository-connection))))))

(defn retrieve-owl-range-for-property
  "Returns the datatype of the range of a datatype property"
  ([property-uri]
     (retrieve-owl-range-for-property property-uri (connection! :default)))
  ([property-uri repository-connection]
     (let [template (build-graph-template
                     [{:template (build-graph
                                  [(build-uri-node property-uri
                                                        [(build-relation (rdfs-range)
                                                                         (build-variable-node
                                                                          :datatype []))])])
                       :filters []}])]
       (:datatype (first (query-template-in-repository
                          :datatype
                          template
                          (connection! repository-connection)))))))

(defn retrieve-owl-equivalent-properties-for-property
  "Returns equivalent datatype properties of a datatype property"
  ([property-uri]
     (retrieve-owl-equivalent-properties-for-property property-uri (connection! :default)))
  ([property-uri repository-connection]
     (set
      (filter (fn [prop] (not= (uri-to-string property-uri) (uri-to-string prop)))
              (query-transitive-closure-for-predicate property-uri
                                                      (uri-to-string (owl-equivalentProperty))
                                                      (connection! repository-connection))))))

;;
;; TBox manipulation
;;

;;(defstruct tbox-data
;;  :names ;; a map from user names to uris for different tbox resources
;;  :uris ;; a map from uris to resource data
;;  :repositories ;; a map from uris to repository name for where the resource is stored
;;  :validations ;; a map of uris to a hash of validations sets for different validations
;;)

(def *tbox* (ref {:names {} :uris {} :repositories {} :validations{} :resource-kind {}}))

(defn tbox-retrieve-uri-for
  "Returns the URI for a resource name, if a string is passed as the argument
   it is assumed to be a URI and returned without modification"
  ([identifier]
     (if (keyword? identifier)
       (dosync (identifier (:names @*tbox*)))
       (if (= (rdf-meta identifier)
              :owl-class)
         (:uri identifier)
       (if (= (rdf-meta identifier)
              :owl-datatype-property)
         (:uri identifier)
       (if (= (rdf-meta identifier)
              :owl-object-property)
         (:uri identifier)
         (uri-to-string identifier)))))))

(defn tbox-retrieve-repository-for
  "Returns the repository where a resource is stored"
  ([identifier]
     (let [uri (tbox-retrieve-uri-for identifier)]
       (dosync (uri (:repositories @*tbox*))))))

(defn tbox-register-validation-on!
  "Add a validation for a modification of the ABox refering some resource of the TBox"
  ([action resource function]
     (dosync (commute *tbox*
                      (fn [tbox]
                        (let [identifier (tbox-retrieve-uri-for resource)
                              old-validations (get (:validations tbox) identifier)
                              new-validations (if (null? old-validations)
                                                {action (set [function])}
                                                (let [validations-for-action (get old-validations action)]
                                                  (if (null? validations-for-action)
                                                    (merge old-validations {action (set [function])})
                                                    (merge old-validations {action (conj validations-for-action function)}))))]
                          {:names (:names tbox)
                           :uris (:uris tbox)
                           :repositories (:repositories tbox)
                           :validations (assoc (dissoc (:repositories tbox) identifier) identifier new-validations)
                           :resource-kind (:resource-kind tbox)})))))
  ([action prefix value function]
     (dosync (commute *tbox*
                      (fn [tbox]
                        (let [identifier (uri-to-string (build-uri prefix value))
                              old-validations (get (:validations tbox) identifier)
                              new-validations (if (nil? old-validations)
                                                {action (set [function])}
                                                (let [validations-for-action (get old-validations action)]
                                                  (if (null? validations-for-action)
                                                    (merge old-validations {action (set [function])})
                                                    (merge old-validations {action (conj validations-for-action function)}))))]
                          {:names (:names tbox)
                           :uris (:uris tbox)
                           :repositories (:repositories tbox)
                           :validations (assoc (dissoc (:repositories tbox) identifier) identifier new-validations)
                           :resource-kind (:resource-kind tbox)}))))))

(defn tbox-retrieve-validations-for
  "Returns the set of validations for a given TBox resource"
  ([action resource]
     (dosync (action (get (:validations @*tbox*) (if (keyword? resource)
                                                  (get (:names @*tbox*) resource)
                                                  (uri-to-string resource))))))
  ([action prefix value]
     (dosync
      (action (get (:validations @*tbox*) (uri-to-string (build-uri prefix value)))))))

(defn tbox-find-datatype-property-by-uri!
  "Builds a datatype property and stores it in the TBox"
  ([datatype-property-uri]
     (dosync
      (commute *tbox*
               (fn [tbox datatype-property-uri]
                 (if (null? (get (:uris tbox) (uri-to-string datatype-property-uri)))
                   (let [repository-name (get (:repositories tbox) (uri-to-string datatype-property-uri))
                         connection (connection! repository-name)
                         datatype (retrieve-owl-range-for-property datatype-property-uri connection)
                         equivalent-properties (retrieve-owl-equivalent-properties-for-property datatype-property-uri connection)
                         name (key-for-value (:names tbox) (uri-to-string datatype-property-uri))
                         property (with-meta
                                   (struct owl-datatype-property
                                           (if (= name nil) nil (first name))
                                           datatype-property-uri
                                           datatype
                                           equivalent-properties
                                           repository-name)
                                   {:rdf :owl-datatype-property})]
                     {:names (:names tbox)
                      :uris (merge (:uris tbox) {(uri-to-string datatype-property-uri) property})
                      :repositories (:repositories tbox)
                      :validations (:validations tbox)
                      :resource-kind (:resource-kind tbox)})
                   tbox))
               datatype-property-uri)
      (get (:uris @*tbox*) (uri-to-string datatype-property-uri)))))

(defn tbox-find-object-property-by-uri!
  "Builds an object property and stores it in the TBox"
  ([object-property-uri]
     (dosync
      (commute *tbox*
               (fn [tbox property-uri]
                 (if (null? (get (:uris tbox) (uri-to-string property-uri)))
                   (let [repository-name (get (:repositories tbox) (uri-to-string property-uri))
                         connection (connection! repository-name)
                         equivalent-properties (retrieve-owl-equivalent-properties-for-property property-uri connection)
                         name (key-for-value (:names tbox) (uri-to-string object-property-uri))
                         property (with-meta
                                   (struct owl-object-property
                                           (if (= name nil) nil (first name))
                                           property-uri
                                           equivalent-properties
                                           repository-name)
                                   {:rdf :owl-object-property})]
                     {:names (:names tbox)
                      :uris (merge (:uris tbox) {(uri-to-string property-uri) property})
                      :repositories (:repositories tbox)
                      :validations (:validations tbox)
                      :resource-kind (:resource-kind tbox)})
                   tbox))
               object-property-uri)
      (get (:uris @*tbox*) (uri-to-string object-property-uri)))))

(defn tbox-find-class-by-uri!
  "Try to find an OWL class in a repository and stores it in the TBox"
  ([class-uri]
     (dosync
      (when (null? (get (:uris @*tbox*) (uri-to-string class-uri)))
        (let [repository-name (get (:repositories @*tbox*) (uri-to-string class-uri))
              connection (connection! repository-name)
              name (key-for-value (:names @*tbox*) (uri-to-string class-uri))
              superclasses-uri (retrieve-owl-superclasses-for-class class-uri connection)
              superclasses (set
                            (loop [sc []
                                   sc-uri superclasses-uri]
                              (if (empty? sc-uri)
                                sc
                                (recur (conj sc (tbox-find-class-by-uri! (first sc-uri))) (rest sc-uri)))))

              base-datatype-properties (set
                                        (map
                                         (fn [prop] (tbox-find-datatype-property-by-uri! prop))
                                         (loop [props (set [])
                                                classes (conj superclasses-uri class-uri)]
                                           (if (empty? classes)
                                             props
                                             (let [class-to-look (first classes)
                                                   props-found (retrieve-owl-datatype-properties-for-class  class-to-look connection)]
                                               (recur (clojure.set/union props props-found)
                                                      (rest classes)))))))
              base-object-properties (set
                                      (map
                                       (fn [prop] (tbox-find-object-property-by-uri! prop))
                                       (loop [props (set [])
                                              classes (conj superclasses-uri class-uri)]
                                         (if (empty? classes)
                                           props
                                           (let [class-to-look (first classes)
                                                 props-found (retrieve-owl-object-properties-for-class  class-to-look connection)]
                                             (recur (clojure.set/union props props-found)
                                                    (rest classes)))))))
              class (with-meta
                     (struct owl-class
                             (if (= name nil) nil (first name))
                             class-uri
                             superclasses
                             base-datatype-properties
                             base-object-properties
                             repository-name)
                     {:rdf :owl-class})]
          (commute *tbox*
                   (fn [tbox]
                     {:names (:names tbox)
                      :uris (merge (:uris tbox) {(uri-to-string class-uri) class})
                      :repositories (:repositories tbox)
                      :validations (:validations tbox)
                      :resource-kind (:resource-kind tbox)}))))
                 (get (:uris @*tbox*) (uri-to-string class-uri)))))

(defn tbox-find-datatype-property!
  "Finds a datatype property in the TBox by the name used to register it"
  ([name]
     (dosync (tbox-find-datatype-property-by-uri! (get (:names @*tbox*) name)))))

(defn tbox-find-object-property!
  "Finds an object property in the TBox by the name used to register it"
  ([name]
     (dosync (tbox-find-object-property-by-uri! (get (:names @*tbox*) name)))))

(defn tbox-find-class!
  "Finds a class in the TBox by the name used to register it"
  ([name]
     (dosync (tbox-find-class-by-uri! (get (:names @*tbox*) name)))))

(defn tbox-find-resource-by-uri!
  "Returns an object from the TBox if it is registered and has a value"
  ([uri]
     (let [uri-str (uri-to-string uri)
           existing-resource (dosync
                               (get (:uris @*tbox*) uri-str))]
       (if (null? existing-resource)
         (let [ resource-kind (dosync (get (:resource-kind @*tbox*) uri-str))]
           (if (= resource-kind :owl-class)
             (tbox-find-class-by-uri! uri-str)
           (if (= resource-kind :owl-datatype-property)
             (tbox-find-datatype-property-by-uri! uri-str)
           (if (= resource-kind :owl-object-property)
             (tbox-find-object-property-by-uri! uri-str)
             (throw (Exception. (str "Unknown resource kind for URI" uri-str)))))))
         existing-resource))))

(defn tbox-datatype-property?
  "Checks if a given resource is a datatype property"
  ([resource]
     (let [uri (if (keyword? resource)
                 (tbox-find-class-by-uri! resource)
                 resource)]
       (if (= (rdf-meta (tbox-find-resource-by-uri! uri))
              :owl-datatype-property)
         true
         false))))

(defn tbox-object-property?
  "Checks if a given resource is a object property"
  ([resource]
     (let [uri (if (keyword? resource)
                 (tbox-find-class-by-uri! resource)
                 resource)]
       (if (= (rdf-meta (tbox-find-resource-by-uri! uri))
              :owl-object-property)
         true
         false))))



(defn tbox-register-name!
  "Maps a name to an URI in the TBox"
  ([name uri kind]
     (tbox-register-name! name uri kind :default))
  ([name uri kind repository]
     (dosync
      (commute *tbox*
               (fn [tbox name uri]
                 (if (= (class uri) #=java.lang.String)
                   {:names (merge (:names tbox) {name uri})
                    :uris (:uris tbox)
                    :repositories (merge (:repositories tbox) {uri repository})
                    :validations (:validations tbox)
                    :resource-kind (merge (:resource-kind tbox) {uri kind})}
                   (let [uri-name (uri-to-string uri)]
                     {:names (merge (:names tbox) {name uri-name})
                      :uris (:uris tbox)
                      :repositories (merge (:repositories tbox) {uri-name repository})
                      :validations (:validations tbox)
                      :resource-kind (merge (:resource-kind tbox) {uri-name kind}) })))
               name
               uri))))

(defn tbox-find-name-for-uri
  "Finds a name used to register an URI in the TBox"
  ([uri]
     (let [str-uri (uri-to-string uri)]
           (dosync
            (let [names (:names @*tbox*)]
              (loop [rest-names-keys (keys names)]
                (if (null? rest-names-keys)
                  nil
                  (let [this-key (first rest-names-keys)
                        this-uri (get names this-key)]
                    (if (= this-uri str-uri)
                      this-key
                      (recur (rest rest-names-keys)))))))))))

(defn tbox-register-class!
  "register an OWL class with a given name"
  ([name uri repository]
     (tbox-register-name! name uri :owl-class repository))
  ([name uri]
     (tbox-register-class! name uri :default)))

(defn tbox-register-object-property!
  "register an OWL class with a given name"
  ([name uri repository]
     (tbox-register-name! name uri :owl-datatype-property repository))
  ([name uri]
     (tbox-register-object-property! name uri :default)))

(defn tbox-register-datatype-property!
  "register an OWL class with a given name"
  ([name uri repository]
     (tbox-register-name! name uri :owl-datatype-property repository))
  ([name uri]
     (tbox-register-datatype-property! name uri :default)))


(defn tbox-clear!
  "Empties the TBox*"
  ([]
     (dosync
      (commute *tbox*
               (fn [tbox]
                 {:names {} :uris {} :repositories {} :validations {} :resource-kind {}})))))

;; ABox creation and manipulation

(defstruct abox-individual
  :identifier ;; the unique identifier of this individual
  :uri ;; the URI identifying this individual, composed of a namespace and the UUID
  :classes ;; the owl classes this individual belongs to
  :properties-value-map ;; map with {:property-name value} maps
  :dirty-properties ;; props with value not stored in the repository
  :created ;; true if the individual has been created and has not been saved yet
)


(defn owl-individual?
  "Checks if an object is an ABox individual"
  ([object] (= (rdf-meta object) :owl-individual)))

(defn owl-individual-to-properties-map
  "Returns a map with the properties of an individual"
  ([individual]
     (let [properties (:properties-value-map individual)
           base-map (reduce-maps-list
                     (map (fn [key]
                            (let [key-name (let [to-return (tbox-find-name-for-uri key)]
                                             (if (null? to-return) key to-return))]
                              (let [the-obj (get properties key)]
                                (if (null? the-obj)
                                  {}
                                  (if (tbox-datatype-property? key)
                                    {key-name (:value the-obj)}
                                    (if (owl-individual? the-obj)
                                      {key-name (owl-individual-to-properties-map the-obj)}
                                      {key-name {:uri (uri-to-string the-obj) :tag :owl-individual}}))))))
                          (keys properties)))]
       (merge base-map {:tag :owl-individual :uri (uri-to-string (:uri individual))}))))

(defn gen-id
  "Generates an unique identifier"
  ([] (.. java.util.UUID (randomUUID) (toString))))

(defn parse-id
  "Try to retrieve an UUID from a URI string"
  ([uri]
     (let [uri-str (uri-to-string uri)]
       (if (null? (re-find '#"#" uri-str))
         (let [index (+ (. uri-str (lastIndexOf "/")) 1)]
           (. uri-str (substring index (. uri-str length))))
         (let [index (+ (. uri-str (lastIndexOf "#")) 1)]
           (. uri-str (substring index (. uri-str length))))))))

(defn apply-validations
  "Applies validations for an action and resource to certain arguments"
  ([action resource-uri args-list]
     (let [resource-validations (tbox-retrieve-validations-for action (tbox-retrieve-uri-for resource-uri))]
       (if (nothing? (loop [validations resource-validations
                            monad (just args-list)]
                       (if (null? validations)
                         monad
                         (recur (rest validations)
                                (>>= (fn [props] (if (true? (apply (first validations) props))
                                                   (just props)
                                                   (nothing)))
                                     monad)))))
         false
         true))))


(defn abox-list-properties-for
  "Returns a single map with all the properties and values for an individual"
  ([individual]
     (:properties-value-map individual)))
;;     (loop [class-uris (keys (:properties-value-map individual))
;;            acum {}]
;;            (if (nil? class-uris)
;;              acum
;;              (recur (rest class-uris)
;;                     (merge acum (get (:properties-value-map individual) (first class-uris))))))))


(defn abox-individual-to-graph
  "Translates an ABox individual to a set of RDF graph"
  ([individual]
     (let [ class-uris (loop [classes (:classes individual)
                              acum (set (map (fn [c] (:uri c)) classes)) ]
                              (if (null? classes)
                                acum
                                (recur (rest classes)
                                       (clojure.set/union acum (set (map (fn [c] (:uri c)) (:subclass-of (first classes))))))))
           properties (abox-list-properties-for individual)
           types-node (build-uri-node (:uri individual)
                                      (loop [uris-left class-uris
                                             acum []]
                                        (if (null? uris-left)
                                          acum
                                          (recur (rest uris-left)
                                                 (conj acum (build-relation (rdf-type) (to-rdf (first uris-left))))))))
           properties-node (build-uri-node (:uri individual)
                                           (map (fn [property-uri]
                                                  (let [value-for-prop-uri (get properties property-uri)]
                                                    (if (= (rdf-meta value-for-prop-uri) :literal)
                                                      (build-relation (to-rdf property-uri)
                                                                      (build-literal-node value-for-prop-uri))
                                                      (build-relation (to-rdf property-uri)
                                                                      (build-uri-node value-for-prop-uri)))))
                                                (keys properties))) ]
       (build-graph [types-node properties-node]))))


(defn abox-create-individual
  "Creates a new individual of a given class"
  ([class-name-or-uri individual-ns props-map]
     (let [uri (tbox-retrieve-uri-for class-name-or-uri)
           ns-uri (let [ns (if (keyword? individual-ns) (:prefix (rdf-ns individual-ns)) individual-ns)
                        last-ns (last ns)]
                    (if (or (= last-ns \/)
                            (= last-ns \#))
                      ns
                      (str ns "#")))
           owl-class (tbox-find-class-by-uri! uri)
           rdf-props-map (reduce
                          (fn [acum item] (merge acum item))
                          {}
                          (map
                           (fn [key] {(tbox-retrieve-uri-for key)
                                      (to-rdf (get props-map key))})
                           (keys props-map)))]
       (if (loop [classes (conj (:subclass-of owl-class) owl-class)] ;; lets check if all the validations for superclasses and the class validates
             (if (null? classes)
               true
               (if (apply-validations :create (:uri (first classes)) (list rdf-props-map))
                 (recur (rest classes))
                 false)))
         (do
           (loop [keys (keys props-map)]
             (if (not (null? keys))
               (let [the-key (first keys)
                     the-key-uri (tbox-retrieve-uri-for the-key)]
                 (if (apply-validations :create the-key-uri (list (get rdf-props-map the-key-uri)))
                   (recur (rest keys))
                   (throw (Exception. (str "Validations failed for resource "
                                           uri
                                           "on property validation for property "
                                           (tbox-retrieve-uri-for the-key)
                                           "with value "
                                           (the-key props-map))))))))
           ;; validations OK
           (let [uuid (gen-id)]
             (with-meta
              (struct abox-individual
                      uuid
                      (str ns-uri uuid)
                      (set [owl-class])
                      rdf-props-map
                      (set[(keys rdf-props-map)])
                      true)
              {:rdf :owl-individual})))
         (throw (Exception. (str "Validations failed for resource "
                                 uri
                                 " and arguments "
                                 props-map)))))))

(defn abox-create-individual!
  "Creates a new individual and stores its triplets in the repository"
  ([class-name-or-uri individual-ns props-map connection]
     (let [individual (abox-create-individual class-name-or-uri individual-ns props-map)]
       (do
         (write-graph-in-repository! (abox-individual-to-graph individual) (connection! connection))
         (with-meta
          (struct abox-individual
                  (:identifier individual)
                  (:uri individual)
                  (:classes individual)
                  (:properties-value-map individual)
                  (set [])
                  false)
          {:rdf :owl-individual}))))
  ([class-name-or-uri individual-ns props-map]
     (abox-create-individual! class-name-or-uri individual-ns props-map (connection! :default))))


;; ABox individuals finder

(defn abox-find-individual-uris!
  "Creates a SPARQL query to retrieve individuals from an ABox stored in a repository"
  ([ & conditions-connection ]
     (let [connection (connection! (last conditions-connection))
           conditions (drop-last conditions-connection)]
       (map (fn [result] (:individual result))
            (query-template-in-repository :individual conditions connection)))))


(defn abox-build-individual-from-uri!
  "Creates a representation of the ABox individual with the given URI"
  ([uri] (abox-build-individual-from-uri! uri :default))
  ([uri connection]
     (let [conn (connection! connection)
           classes (let [classes-uris (query-template-in-repository
                                       :uri
                                       (build-graph-template [{:template (build-graph [ (build-uri-node uri
                                                                                                        [(build-relation (rdf-type)
                                                                                                                         (build-variable-node :uri []))])])
                                                               :filters []}])
                                       (connection! connection))]
                     (map (fn [result] (tbox-find-class-by-uri! (:uri result)))
                          classes-uris))
           properties (let [classes-uris (query-template-in-repository
                                          :uri :obj
                                          (build-graph-template [{:template (build-graph [ (build-uri-node uri
                                                                                                           [(build-variable-relation :uri
                                                                                                                                     (build-variable-node :obj []))])])
                                                                  :filters [(build-filter :uri (str "?uri != <" (uri-to-string (rdf-type)) ">"))]}])
                                          (connection! connection))]
                        (reduce (fn [acum item] (merge acum item))
                                {}
                                (map (fn [result] {(uri-to-string (:uri result)) (:obj result)})
                                     classes-uris)))]
;;           datatype-properties (filter (fn [prop] (= (rdf-meta prop) :owl-datatype-property)) properties)
;;           object-properties (filter (fn [prop] (= (rdf-meta prop) :owl-object-property)) properties)]
       (with-meta
        (struct abox-individual
                (parse-id uri)
                uri
                (set classes)
                properties
                (set [])
                false)
       {:rdf :owl-individual}))))


;; Some functions modelling conditions to be used when building individual finders
(defn instance-of-classes
  "Returns the graph describing that an individual is instance of a set of OWL classes"
  ([ & classes ]
     (let [template (build-graph [ (build-variable-node :individual
                                                        (map (fn [class] (build-relation (rdf-type)
                                                                                         (build-uri-node (tbox-retrieve-uri-for class))))
                                                             classes)) ])]
       { :template template :filters [] })))

(defn not-instance-of-classes
  "Returns the graph describing that an individual is instance of a set of OWL classes"
  ([ & classes ]
     (let [ classes-map (loop [mapping {}
                               to-map classes
                               counter 0]
                          (if (null? to-map)
                            mapping
                            (recur (assoc mapping (keyword (str "c" counter)) (tbox-retrieve-uri-for (first classes)))
                                   (rest classes)
                                   (+ 1 counter)))) ]
       (let [template-inner (build-graph-template [
                                                   {:template (build-optional-graph
                                                               [(build-variable-node :individual
                                                                                    (map (fn [key] (build-relation (rdf-type)
                                                                                                                   (build-variable-node key)))
                                                                                         (keys classes-map)))])
                                                    :filters (map (fn [key] (build-filter key (str (keyword-to-sparql-var key) " = <" (get classes-map key) ">" )))
                                                                  (keys classes-map))}
                                                   ])]
         {:template (build-graph [(build-variable-node :individual [(build-relation (rdf-type)
                                                                                    (build-uri-node (owl-Thing) []))])
                                  template-inner])
          :filters (map (fn [key] (build-filter key (str " ! bound(" (keyword-to-sparql-var key) ")")))
                        (keys classes-map))}))))


;; TBox/ABox resource removal

(defn tbox-resource-triplets
  "Removes the resource from the TBox"
  ([prefix value] (tbox-resource-triplets (build-uri prefix value)))
  ([value]
     (build-graph-template
      [{:template (build-graph
                   [(build-variable-node :subject
                                         [(build-variable-relation :predicate
                                                                   (build-variable-node :object))])])
        :filters [(build-filter :subject (let [value-str (if (= (rdf-meta value) :owl-class)
                                                           (uri-to-string (:uri value))
                                                           (if (= (rdf-meta value) :owl-datatype-property)
                                                             (uri-to-string (:uri value))
                                                             (if (= (rdf-meta value) :owl-object-property)
                                                               (uri-to-string (:uri value))
                                                               (uri-to-string value))))]
                                           (str "?subject = <" value-str "> || ?predicate = <" value-str "> || ?object = <" value-str ">")))]}])))

(defn abox-resource-triplets
  "Removes the resource from the ABox, just a call to remove resource from TBox"
  ([value]
     (if (= (rdf-meta value) :owl-individual)
       (tbox-resource-triplets (:uri value))
       (tbox-resource-triplets value))))


;; ABox update

(defn abox-update-individual-properties
  "Updates the values for the properties of an individual"
  ([individual properties-to-update]
     (let [uris-to-update (reduce
                           (fn [acum item] (merge acum item))
                           {}
                           (map (fn [key]
                                 (let [uri (if (keyword? key)
                                             (uri-to-string (tbox-retrieve-uri-for key))
                                             (uri-to-string key))]
                                     {uri (to-rdf (get properties-to-update key))}))
                               (keys properties-to-update)))
           updated-properties (loop [ the-properties (keys uris-to-update)
                                      the-updated-props (:properties-value-map individual) ]
                                (if (null? the-properties)
                                  the-updated-props
                                  (recur (rest the-properties)
                                         (let [the-key (first the-properties)
                                               dissoced-map (dissoc uris-to-update the-key)]
                                         (assoc dissoced-map the-key (get uris-to-update the-key))))))
           updated-dirty-properties (set (clojure.set/union (:dirty-properties individual) (set (keys uris-to-update)))) ]

       (with-meta
        (struct abox-individual
                (:identifier individual)
                (:uri individual)
                (:classes individual)
                updated-properties
                updated-dirty-properties
                (:created individual))
        {:rdf :owl-individual}))))


;; ABox save
;;(comment
(defn abox-save-individual!
  "Saves the state of an individual into the repository"
  ([individual connection]
     (if (:created individual)
       (abox-create-individual! individual connection)
       (let [uri (:uri individual)
             dirty-properties (:dirty-properties individual)
             properties (:properties-value-map individual)
             properties-to-delete (filter (fn [key] (null? (get dirty-properties key))) (keys properties))
             properties-to-update (filter (fn [key] (not (null? (get dirty-properties key)))) (keys properties))]
         (do
           (loop [to-delete properties-to-delete]
             (when (not (null? to-delete))
               (let [property (first to-delete)
                     graph (build-graph
                            [ (build-uri-node uri
                                              [ (build-relation property
                                                                (if (tbox-datatype-property? property)
                                                                  (build-literal-node (to-rdf (get properties property)))
                                                                  (build-uri-node (get properties property)))) ])])]
                     ;; next I must check result, deleting it if the property is stored in the database
                 (remove-graph-from-repository! graph connection)))))))))

                 ;; afterwards, i must repeat the operation with properties to update:
                 ;; - if the property is not present create it
                 ;; - if the property is present -> delete it and recreate it with the new value
;;))))))))
;;)


(defn abox-destroy-individual-by-uri
  "Destroys a given individual provided its URI"
  ([uri-or-string connection]
     (let [uri (uri-to-string uri-or-string)
           template (build-graph-template
                     [{:template (build-graph [ (build-variable-node :subject
                                                                     [(build-variable-relation :predicate
                                                                                               (build-variable-node :object []))]) ])
                       :filters [(build-filter :subject (str "?subject = <" uri "> || ?object = <" uri ">" ))]}])]
           (remove-graph-template-from-repository! template connection)))
  ([uri-or-string]
     (abox-destroy-individual-by-uri uri-or-string (connection! :default))))


(comment
  "Tests"
)

(use 'clojure.contrib.test-is)

(defn mock-owl-individual-identifier
  "Replaces the identifier of an owl individual with a given value so it can be easily tested"
  ([individual mock-identifier]
     (with-meta
      (struct abox-individual
              mock-identifier
              (:uri individual)
              (:classes individual)
              (:properties-value-map  individual)
              (set [])
              true)
      {:rdf :owl-individual})))

(deftest test-owl-ns
  (is (= (rdf-ns :owl)
         {:prefix "http://www.w3.org/2002/07/owl#", :value :owl})))

(deftest describe-owl-class-1
  (is (= (describe-owl-class "http://test.com#Test")
         '{:value {:prefix "", :value "http://test.com#Test"}
           :relations [{:value {:prefix :rdf, :value "type"}
                        :object {:value {:prefix :owl, :value "Class"}
                                 :relations []}}
                       {:value {:prefix :rdfs, :value "subClassOf"}
                        :object {:value {:prefix :owl, :value "Thing"}
                                 :relations []}}]})))

(deftest describe-owl-subclass-1
  (is (= (describe-owl-subclass :rdf "child" :rdf "parent")
         '{:value
           {:prefix :rdf, :value "child"}
           :relations [{:value
                        {:prefix :rdfs, :value "subClassOf"}
                        :object
                        {:value {:prefix :rdf, :value "parent"}
                         :relations []}}]})))

(deftest describe-owl-subclass-2
  (is (= (describe-owl-subclass :rdf "child" "http://test.com/parent" )
         '{:value
           {:prefix :rdf, :value "child"}
           :relations [{:value
                        {:prefix :rdfs, :value "subClassOf"}
                        :object
                        {:value {:prefix "", :value "http://test.com/parent"}
                         :relations []}}]})))

(deftest describe-owl-subclass-3
  (is (= (describe-owl-subclass "http://test.com/child" "http://test.com/parent")
         '{:value
           {:prefix "", :value "http://test.com/child"}
           :relations [{:value
                        {:prefix :rdfs, :value "subClassOf"}
                        :object {:value {:prefix "", :value "http://test.com/parent"}
                                 :relations []}}]})))

(deftest describe-owl-subclass-4
  (is (= (describe-owl-subclass "http://test.com/child" :rdf "parent" )
         '{:value
           {:prefix "", :value "http://test.com/child"}
           :relations [{:value
                        {:prefix :rdfs, :value "subClassOf"}
                        :object {:value {:prefix :rdf, :value "parent"}
                                 :relations []}}]})))

(deftest describe-owl-datatype-property-1
  (is (= (describe-owl-datatype-property :rdf "testProperty" (xsd-float))
         '{:value
           {:prefix :rdf, :value "testProperty"}
           :relations [{:value
                        {:prefix :rdf, :value "type"}
                        :object {:value {:prefix :owl, :value "DatatypeProperty"}
                                 :relations []}}
                       {:value {:prefix :rdfs, :value "range"}
                        :object {:value {:prefix :xsd, :value "float"}
                                 :relations []}}]})))

(deftest describe-owl-datatype-property-2
  (is (= (describe-owl-datatype-property "http://test.com/testProperty" (xsd-float))
         '{:value
           {:prefix "", :value "http://test.com/testProperty"}
           :relations [{:value {:prefix :rdf, :value "type"}
                        :object {:value {:prefix :owl, :value "DatatypeProperty"}
                                 :relations []}}
                       {:value {:prefix :rdfs, :value "range"}
                        :object {:value {:prefix :xsd, :value "float"}
                                 :relations []}}]})))

(deftest describe-owl-object-property-1
  (is (= (describe-owl-object-property "http://test.com/testProperty")
         '{:value
           {:prefix "", :value "http://test.com/testProperty"}
           :relations [{:value {:prefix :rdf, :value "type"}
                        :object {:value {:prefix :owl, :value "ObjectProperty"}
                                 :relations []}}]})))

(deftest describe-owl-object-property-2
  (is (= (describe-owl-object-property :a "test")
         '{:value
           {:prefix :a, :value "test"}
           :relations [{:value {:prefix :rdf, :value "type"}
                        :object {:value {:prefix :owl, :value "ObjectProperty"}
                                 :relations []}}]})))

(deftest describe-owl-object-property-range-1
  (is (= (describe-owl-object-property-range :rdf "property" :rdf "object")
         {:value {:prefix :rdf, :value "property"}
          :relations [{:value {:prefix :rdfs, :value "range"}
                       :object {:value {:prefix :rdf, :value "object"}, :relations []}}]})))

(deftest describe-owl-object-property-range-2
  (is (= (describe-owl-object-property-range "http://test.com/property" "http://test.com/object")
         {:value {:prefix "", :value "http://test.com/property"}
          :relations [{:value {:prefix :rdfs, :value "range"}
                       :object {:value {:prefix "", :value "http://test.com/object"}, :relations []}}]})))

(deftest describe-owl-equivalent-properties-1
  (is (= (describe-owl-equivalent-properties :rdf "parent" :rdf "child")
         '{:nodes [{:value {:prefix :rdf, :value "parent"}, :relations [{:value {:prefix :owl, :value "equivalentProperty"}, :object {:value {:prefix :rdf, :value "child"}, :relations []}}]} {:value {:prefix :rdf, :value "child"}, :relations [{:value {:prefix :owl, :value "equivalentProperty"}, :object {:value {:prefix :rdf, :value "parent"}, :relations []}}]}], :context #{}})))

(deftest describe-owl-equivalent-properties-2
  (is (= (describe-owl-equivalent-properties "http://test.com/parent" :rdf "child")
         '{:nodes [{:value {:prefix :rdf, :value "child"}, :relations [{:value {:prefix :owl, :value "equivalentProperty"}, :object {:value {:prefix "", :value "http://test.com/parent"}, :relations []}}]} {:value {:prefix "", :value "http://test.com/parent"}, :relations [{:value {:prefix :owl, :value "equivalentProperty"}, :object {:value {:prefix :rdf, :value "child"}, :relations []}}]}], :context #{}})))

(deftest describe-owl-equivalent-properties-3
  (is (= (describe-owl-equivalent-properties "http://test.com/parent" "http://test.com/child")
         '{:nodes [{:value {:prefix "", :value "http://test.com/parent"}, :relations [{:value {:prefix :owl, :value "equivalentProperty"}, :object {:value {:prefix "", :value "http://test.com/child"}, :relations []}}]} {:value {:prefix "", :value "http://test.com/child"}, :relations [{:value {:prefix :owl, :value "equivalentProperty"}, :object {:value {:prefix "", :value "http://test.com/parent"}, :relations []}}]}], :context #{}})))

(deftest describe-owl-equivalent-properties-4
  (is (= (describe-owl-equivalent-properties :rdf "parent" "http://test.com/child")
         '{:nodes [{:value {:prefix "", :value "http://test.com/child"}, :relations [{:value {:prefix :owl, :value "equivalentProperty"}, :object {:value {:prefix :rdf, :value "parent"}, :relations []}}]} {:value {:prefix :rdf, :value "parent"}, :relations [{:value {:prefix :owl, :value "equivalentProperty"}, :object {:value {:prefix "", :value "http://test.com/child"}, :relations []}}]}], :context #{}})))

(deftest describe-owl-equivalent-classes-1
  (is (= (describe-owl-equivalent-classes :rdf "parent" :rdf "child")
         '{:nodes [{:value {:prefix :rdf, :value "parent"}, :relations [{:value {:prefix :owl, :value "equivalentClass"}, :object {:value {:prefix :rdf, :value "child"}, :relations []}}]} {:value {:prefix :rdf, :value "child"}, :relations [{:value {:prefix :owl, :value "equivalentClass"}, :object {:value {:prefix :rdf, :value "parent"}, :relations []}}]}], :context #{}})))

(deftest describe-owl-equivalent-classes-2
  (is (= (describe-owl-equivalent-classes "http://test.com/parent" :rdf "child")
         '{:nodes [{:value {:prefix :rdf, :value "child"}, :relations [{:value {:prefix :owl, :value "equivalentClass"}, :object {:value {:prefix "", :value "http://test.com/parent"}, :relations []}}]} {:value {:prefix "", :value "http://test.com/parent"}, :relations [{:value {:prefix :owl, :value "equivalentClass"}, :object {:value {:prefix :rdf, :value "child"}, :relations []}}]}], :context #{}})))

(deftest describe-owl-equivalent-classes-3
  (is (= (describe-owl-equivalent-classes "http://test.com/parent" "http://test.com/child")
         '{:nodes [{:value {:prefix "", :value "http://test.com/parent"}, :relations [{:value {:prefix :owl, :value "equivalentClass"}, :object {:value {:prefix "", :value "http://test.com/child"}, :relations []}}]} {:value {:prefix "", :value "http://test.com/child"}, :relations [{:value {:prefix :owl, :value "equivalentClass"}, :object {:value {:prefix "", :value "http://test.com/parent"}, :relations []}}]}], :context #{}})))

(deftest describe-owl-equivalent-classes-4
  (is (= (describe-owl-equivalent-classes :rdf "parent" "http://test.com/child")
         '{:nodes [{:value {:prefix "", :value "http://test.com/child"}, :relations [{:value {:prefix :owl, :value "equivalentClass"}, :object {:value {:prefix :rdf, :value "parent"}, :relations []}}]} {:value {:prefix :rdf, :value "parent"}, :relations [{:value {:prefix :owl, :value "equivalentClass"}, :object {:value {:prefix "", :value "http://test.com/child"}, :relations []}}]}], :context #{}})))

(deftest describe-tbox-1
  (is (= (describe-tbox
          (describe-owl-class "http://test.com/class_a")
          (describe-owl-class "http://test.com/class_b")
          (describe-owl-datatype-property "http://test.com/property_a1" (xsd-float))
          (describe-owl-datatype-property "http://test.com/property_a2" (xsd-float))
          (describe-owl-object-property "http://test.com/property_b")
          (describe-owl-equivalent-properties "http://test.com/property_a1" "http://test.com/property_a2"))
         '{:nodes [{:value {:prefix "", :value "http://test.com/class_a"}, :relations [{:value {:prefix :rdf, :value "type"}, :object {:value {:prefix :owl, :value "Class"}, :relations []}} {:value {:prefix :rdfs, :value "subClassOf"}, :object {:value {:prefix :owl, :value "Thing"}, :relations []}}]} {:value {:prefix "", :value "http://test.com/class_b"}, :relations [{:value {:prefix :rdf, :value "type"}, :object {:value {:prefix :owl, :value "Class"}, :relations []}} {:value {:prefix :rdfs, :value "subClassOf"}, :object {:value {:prefix :owl, :value "Thing"}, :relations []}}]} {:value {:prefix "", :value "http://test.com/property_a1"}, :relations [{:value {:prefix :rdf, :value "type"}, :object {:value {:prefix :owl, :value "DatatypeProperty"}, :relations []}} {:value {:prefix :rdfs, :value "range"}, :object {:value {:prefix :xsd, :value "float"}, :relations []}}]} {:value {:prefix "", :value "http://test.com/property_a2"}, :relations [{:value {:prefix :rdf, :value "type"}, :object {:value {:prefix :owl, :value "DatatypeProperty"}, :relations []}} {:value {:prefix :rdfs, :value "range"}, :object {:value {:prefix :xsd, :value "float"}, :relations []}}]} {:value {:prefix "", :value "http://test.com/property_b"}, :relations [{:value {:prefix :rdf, :value "type"}, :object {:value {:prefix :owl, :value "ObjectProperty"}, :relations []}}]} {:value {:prefix "", :value "http://test.com/property_a1"}, :relations [{:value {:prefix :owl, :value "equivalentProperty"}, :object {:value {:prefix "", :value "http://test.com/property_a2"}, :relations []}}]} {:value {:prefix "", :value "http://test.com/property_a2"}, :relations [{:value {:prefix :owl, :value "equivalentProperty"}, :object {:value {:prefix "", :value "http://test.com/property_a1"}, :relations []}}]}], :context #{}})))

(deftest describe-tbox-2
  (is (= (rdf-meta
          (describe-tbox
           (describe-owl-class "http://test.com/class_a")
           (describe-owl-class "http://test.com/class_b")
           (describe-owl-datatype-property "http://test.com/property_a1" (xsd-float))
           (describe-owl-datatype-property "http://test.com/property_a2" (xsd-float))
           (describe-owl-object-property "http://test.com/property_b")
           (describe-owl-equivalent-properties "http://test.com/property_a1" "http://test.com/property_a2")))
         :graph)))

(deftest describe-owl-class-has-property-1
  (is (= (describe-owl-class-has-property
          "http://test.com/class" "http://test.com/property")
         '{:value {:prefix "", :value "http://test.com/property"}, :relations [{:value {:prefix :rdfs, :value "domain"}, :object {:value {:prefix "", :value "http://test.com/class"}, :relations []}}]})))

(deftest describe-owl-class-has-property-2
  (is (= (describe-owl-class-has-property
          :rdf "class_a" "http://test.com/property")
         '{:value {:prefix "", :value "http://test.com/property"}, :relations [{:value {:prefix :rdfs, :value "domain"}, :object {:value {:prefix :rdf, :value "class_a"}, :relations []}}]})))

(deftest describe-owl-class-has-property-3
  (is (= (describe-owl-class-has-property
          "http://test.com/class_1" :rdf "property_b")
         '{:value {:prefix :rdf, :value "property_b"}, :relations [{:value {:prefix :rdfs, :value "domain"}, :object {:value {:prefix "", :value "http://test.com/class_1"}, :relations []}}]})))

(deftest describe-owl-class-has-property-4
  (is (= (describe-owl-class-has-property
          :rdf "class_a" :rdf "property_b")
         '{:value {:prefix :rdf, :value "property_b"}, :relations [{:value {:prefix :rdfs, :value "domain"}, :object {:value {:prefix :rdf, :value "class_a"}, :relations []}}]})))

(deftest test-retrieve-superclasses-1
  (is (= (let [repo (init-memory-repository!)
               conn (. repo (getConnection))
               graph (describe-tbox
                      (describe-owl-class "http://test.com/class_a")
                      (describe-owl-class "http://test.com/class_b")
                      (describe-owl-class "http://test.com/class_c")
                      (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_b")
                      (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_c")) ]
           (do
             (write-graph-in-repository! graph conn)
             (let [result (retrieve-owl-superclasses-for-class "http://test.com/class_a" conn)]
               (do
                 (. conn (close))
                 (set result)))))
         (set '({:prefix "", :value "http://test.com/class_b"}
                {:prefix "", :value "http://www.w3.org/2002/07/owl#Thing"}
                {:prefix "", :value "http://test.com/class_c"})))))


(deftest test-retrieve-datatype-properties-1
  (is (= (let [repo (init-memory-repository!)
               conn (. repo (getConnection))
               graph (describe-tbox
                      (describe-owl-class "http://test.com/class_a")
                      (describe-owl-datatype-property "http://test.com/prop_b" (xsd-float))
                      (describe-owl-datatype-property "http://test.com/prop_c" (xsd-float))
                      (describe-owl-class-has-property "http://test.com/class_a" "http://test.com/prop_b")
                      (describe-owl-class-has-property "http://test.com/class_a" "http://test.com/prop_c"))]
           (do
             (write-graph-in-repository! graph conn)
             (let [result (retrieve-owl-datatype-properties-for-class "http://test.com/class_a" conn)]
               (do
                 (. conn (close))
                 (set result)))))
         (set '({:prefix "", :value "http://test.com/prop_b"}
                {:prefix "", :value "http://test.com/prop_c"})))))

(deftest test-retrieve-owl-range-for-property
  (is (= (let [repo (init-memory-repository!)
               conn (. repo (getConnection))
               graph (describe-tbox
                      (describe-owl-datatype-property "http://test.com/prop_b" (xsd-float))
                      (describe-owl-datatype-property "http://test.com/prop_c" (xsd-float)))]
           (do
             (write-graph-in-repository! graph conn)
             (let [result (retrieve-owl-range-for-property "http://test.com/prop_b" conn)]
               (do
                 (. conn (close))
                 (uri-to-string result)))))
         (uri-to-string (xsd-float)))))

(deftest test-retrieve-owl-equivalent-properties-for-property
  (is (= (let [repo (init-memory-repository!)
               conn (. repo (getConnection))
               graph (describe-tbox
                      (describe-owl-datatype-property "http://test.com/prop_a" (xsd-float))
                      (describe-owl-datatype-property "http://test.com/prop_b" (xsd-float))
                      (describe-owl-datatype-property "http://test.com/prop_c" (xsd-float))
                      (describe-owl-equivalent-properties "http://test.com/prop_a" "http://test.com/prop_b")
                      (describe-owl-equivalent-properties "http://test.com/prop_a" "http://test.com/prop_c"))]
           (do
             (write-graph-in-repository! graph conn)
             (let [result (retrieve-owl-equivalent-properties-for-property "http://test.com/prop_a" conn)]
               (do
                 (. conn (close))
                 result))))
         (set (list {:prefix "", :value "http://test.com/prop_b"}
                    {:prefix "", :value "http://test.com/prop_c"})))))

;; TBox manipulation tests
(defn tbox-restore!
  "A helper function for testing without losing the built tbox in the test"
  ([tbox-to-restore]
     (dosync
      (commute *tbox*
               (fn [tbox]
                 tbox-to-restore)))))

(deftest test-tbox-register-name-1
  (do (tbox-clear!)
      (repositories-registry-clear!)
      (connections-clear!)
      (tbox-register-name! :test "http://test.com" :owl-class)
      (let [new-tbox @*tbox*]
        (do
          (is (= new-tbox
                 {:repositories {"http://test.com" :default}
                  :names {:test "http://test.com"}
                  :uris {}
                  :validations {}
                  :resource-kind {"http://test.com" :owl-class}}))))))

(deftest test-tbox-register-name-2
  (do (tbox-clear!)
      (repositories-registry-clear!)
      (connections-clear!)
      (tbox-register-name! :test (build-uri :rdf "a") :owl-class)
      (let [new-tbox @*tbox*]
        (do
          (is (= new-tbox
                 {:names {:test (uri-to-string (build-uri :rdf "a"))}
                  :uris {}
                  :repositories {(uri-to-string (build-uri :rdf "a")) :default}
                  :validations {}
                  :resource-kind {(uri-to-string (build-uri :rdf "a")) :owl-class}}))))))

(deftest test-tbox-register-validation-on
  (do (tbox-clear!)
      (repositories-registry-clear!)
      (connections-clear!)
      (tbox-register-name! :test (build-uri :rdf "a") :owl-class)
      (tbox-register-validation-on! :create (build-uri :rdf "a") +)
      (tbox-register-validation-on! :create (build-uri :rdf "a") (fn [x y] (+ x y)))
      (let [new-tbox @*tbox*]
        (do
          (is (= 2
                 (count (:create (get (:validations new-tbox) (uri-to-string (build-uri :rdf "a")))))))))))

(deftest test-tbox-register-validation-on-2
  (do (tbox-clear!)
      (repositories-registry-clear!)
      (connections-clear!)
      (tbox-register-name! :test (build-uri :rdf "a") :owl-class)
      (tbox-register-validation-on! :create :rdf "a" +)
      (tbox-register-validation-on! :create :rdf "a" (fn [x y] (+ x y)))
      (let [new-tbox @*tbox*]
        (do
          (is (= 2
                 (count (:create (get (:validations new-tbox) (uri-to-string (build-uri :rdf "a")))))))))))

(deftest test-tbox-retrieve-validations-for-1
  (do (tbox-clear!)
      (repositories-registry-clear!)
      (connections-clear!)
      (tbox-register-name! :test (build-uri :rdf "a") :owl-class)
      (tbox-register-validation-on! :create :rdf "a" +)
      (tbox-register-validation-on! :create :rdf "a" (fn [x y] (+ x y)))
      (let [new-tbox @*tbox*]
        (do
          (is (= true
                 (set? (tbox-retrieve-validations-for :create :rdf "a"))))
          (is (= 2
                 (count (tbox-retrieve-validations-for :create :rdf "a"))))))))

(deftest test-tbox-retrieve-validations-for-2
  (do (tbox-clear!)
      (repositories-registry-clear!)
      (connections-clear!)
      (tbox-register-class! :test (build-uri :rdf "a"))
      (tbox-register-validation-on! :create (build-uri :rdf "a") +)
      (tbox-register-validation-on! :create (build-uri :rdf "a") (fn [x y] (+ x y)))
      (let [new-tbox @*tbox*]
        (do
          (is (= true
                 (set? (tbox-retrieve-validations-for :create :rdf "a"))))
          (is (= 2
                 (count (tbox-retrieve-validations-for :create :rdf "a"))))))))

(deftest test-tbox-find-datatype-property-by-uri
  (let [repo (init-memory-repository!)
        graph (describe-tbox
               (describe-owl-datatype-property "http://test.com/prop_a" (xsd-float))
               (describe-owl-datatype-property "http://test.com/prop_b" (xsd-float))
               (describe-owl-datatype-property "http://test.com/prop_c" (xsd-float))
               (describe-owl-equivalent-properties "http://test.com/prop_a" "http://test.com/prop_c"))]
    (do (tbox-clear!)
        (repositories-registry-clear!)
        (connections-clear!)
        (register-repository! :test repo)
        (write-graph-in-repository! graph (connection! :test))
        (tbox-register-datatype-property! :prop_a "http://test.com/prop_a" :test)
        (let [property (tbox-find-datatype-property-by-uri! "http://test.com/prop_a")]
          (do (close-connection! (connection! :test))
              (is (= property
                     {:name :prop_a
                      :uri "http://test.com/prop_a"
                      :range {:prefix "", :value "http://www.w3.org/2001/XMLSchema#float"}
                      :equivalent-properties #{{:prefix "", :value "http://test.com/prop_c"}}
                      :repository-name :test})))))))

(deftest test-tbox-find-object-property-by-uri
  (let [repo (init-memory-repository!)
        graph (describe-tbox
               (describe-owl-object-property "http://test.com/prop_a" (xsd-float))
               (describe-owl-object-property "http://test.com/prop_b" (xsd-float))
               (describe-owl-object-property "http://test.com/prop_c" (xsd-float))
               (describe-owl-equivalent-properties "http://test.com/prop_a" "http://test.com/prop_c"))]
    (do (tbox-clear!)
        (repositories-registry-clear!)
        (connections-clear!)
        (register-repository! :test repo)
        (write-graph-in-repository! graph (connection! :test))
        (tbox-register-datatype-property! :prop_a "http://test.com/prop_a" :test)
        (let [property (tbox-find-object-property-by-uri! "http://test.com/prop_a")]
          (do (close-connection! (connection! :test))
              (is (= property
                     {:name :prop_a
                      :uri "http://test.com/prop_a"
                      :equivalent-properties #{{:prefix "", :value "http://test.com/prop_c"}}
                      :repository-name :test})))))))

(deftest test-find-class-by-uri-1
  (let [repo (init-memory-repository!)
        graph (describe-tbox
               (describe-owl-datatype-property "http://test.com/prop_a" (xsd-float))
               (describe-owl-datatype-property "http://test.com/prop_b" (xsd-float))
               (describe-owl-class "http://test.com/class_a")
               (describe-owl-class "http://test.com/class_b")
               (describe-owl-class "http://test.com/class_c")
               (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_b")
               (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_c")
               (describe-owl-class-has-property "http://test.com/class_a" "http://test.com/prop_a")
               (describe-owl-class-has-property "http://test.com/class_b" "http://test.com/prop_b"))
        test-template (build-graph-template
                       [{:template (build-graph
                                    [(build-variable-node :subject
                                                          [(build-variable-relation :predicate
                                                                         (build-variable-node :object))])])
                         :filters []}])]
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
        (let [class-recovered (tbox-find-class-by-uri! "http://test.com/class_a")]
          (do (is (= class-recovered
                     {:name :class_a
                      :uri "http://test.com/class_a"
                      :subclass-of #{{:name :owl-thing
                                      :uri {:prefix "", :value "http://www.w3.org/2002/07/owl#Thing"}
                                      :subclass-of #{}
                                      :datatype-properties #{}
                                      :object-properties #{}
                                      :repository-name :test}
                                     {:name :class_b
                                      :uri {:prefix "", :value "http://test.com/class_b"}
                                      :subclass-of #{{:name :owl-thing
                                                     :uri {:prefix "", :value "http://www.w3.org/2002/07/owl#Thing"}
                                                     :subclass-of #{}
                                                     :datatype-properties #{}
                                                     :object-properties #{}
                                                     :repository-name :test}}
                                      :datatype-properties #{{:name :prop_b
                                                              :uri
                                                              {:prefix "", :value "http://test.com/prop_b"}
                                                              :range {:prefix "", :value "http://www.w3.org/2001/XMLSchema#float"}
                                                              :equivalent-properties #{}
                                                              :repository-name :test}}
                                      :object-properties #{}
                                      :repository-name :test}
                                     {:name :class_c
                                      :uri {:prefix "", :value "http://test.com/class_c"}
                                      :subclass-of #{{:name :owl-thing
                                                     :uri {:prefix "", :value "http://www.w3.org/2002/07/owl#Thing"}
                                                     :subclass-of #{}
                                                     :datatype-properties #{}
                                                     :object-properties #{}
                                                     :repository-name :test}}
                                      :datatype-properties #{}
                                      :object-properties #{}
                                      :repository-name :test}}
                      :datatype-properties #{{:name :prop_b
                                              :uri {:prefix "", :value "http://test.com/prop_b"}
                                              :range {:prefix "", :value "http://www.w3.org/2001/XMLSchema#float"}
                                              :equivalent-properties #{}
                                              :repository-name :test}
                                             {:name :prop_a
                                              :uri
                                              {:prefix "", :value "http://test.com/prop_a"}
                                              :range {:prefix "", :value "http://www.w3.org/2001/XMLSchema#float"}
                                              :equivalent-properties #{}
                                              :repository-name :test}}
                      :object-properties #{}
                      :repository-name :test}
                     (close-connection! (connection! :test)))))))))

(deftest test-find-class-by-uri-2
  (let [repo (init-memory-repository!)
        graph (describe-tbox
               (describe-owl-datatype-property "http://test.com/prop_a" (xsd-float))
               (describe-owl-object-property "http://test.com/prop_b")
               (describe-owl-class "http://test.com/class_a")
               (describe-owl-class "http://test.com/class_b")
               (describe-owl-class "http://test.com/class_c")
               (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_b")
               (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_c")
               ;;               (describe-owl-object-property-and-range "http://test.com/prop_b" "http://test.com/class_c")
               (describe-owl-class-has-property "http://test.com/class_a" "http://test.com/prop_a")
               (describe-owl-class-has-property "http://test.com/class_a" "http://test.com/prop_b"))
        test-template (build-graph-template
                       [{:template (build-graph
                                    [(build-variable-node :subject
                                                          [(build-variable-relation :predicate
                                                                         (build-variable-node :object))])])
                         :filters []}])]
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
        (let [class-recovered (tbox-find-class-by-uri! "http://test.com/class_a")]
          (do (is (= (:name class-recovered)
                     :class_a))
              (is (= (:uri class-recovered)
                     "http://test.com/class_a"))
              (is (= (count (:subclass-of class-recovered))
                     3))
              (is (= (first (:datatype-properties class-recovered))
                     {:name :prop_a
                                              :uri {:prefix "", :value "http://test.com/prop_a"}
                                              :range {:prefix "", :value "http://www.w3.org/2001/XMLSchema#float"}
                                              :equivalent-properties #{}
                                              :repository-name :test}))
              (is (= (first (:object-properties class-recovered))
                     {:name :prop_b
                                            :uri {:prefix "", :value "http://test.com/prop_b"}
                                            :equivalent-properties #{}
                                            :repository-name :test}))
              (is (= (:repository-name class-recovered) :test))
              (is (= (count (keys class-recovered)) 6))
              (close-connection! (connection! :test)))))))

(deftest test-find-class-by-uri-1
  (let [repo (init-memory-repository!)
        graph (describe-tbox
               (describe-owl-datatype-property "http://test.com/prop_a" (xsd-float))
               (describe-owl-datatype-property "http://test.com/prop_b" (xsd-float))
               (describe-owl-class "http://test.com/class_a")
               (describe-owl-class "http://test.com/class_b")
               (describe-owl-class "http://test.com/class_c")
               (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_b")
               (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_c")
               (describe-owl-class-has-property "http://test.com/class_a" "http://test.com/prop_a")
               (describe-owl-class-has-property "http://test.com/class_b" "http://test.com/prop_b"))
        test-template (build-graph-template
                       [{:template (build-graph
                                    [(build-variable-node :subject
                                                          [(build-variable-relation :predicate
                                                                         (build-variable-node :object))])])
                         :filters []}])]
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
        (let [class-recovered (tbox-find-resource-by-uri! "http://test.com/class_a")
              property-recovered (tbox-find-resource-by-uri! "http://test.com/prop_a")]
          (do  (is (= (uri-to-string (:uri class-recovered))
                      "http://test.com/class_a"))
               (is (= (uri-to-string (:uri property-recovered))
                      "http://test.com/prop_a")))))))

;; ABox tests

(deftest test-generate-id-1
  (is (not (nil? (gen-id)))))

(deftest test-generate-id-2
  (is (= (class (gen-id))
         #=java.lang.String)))


(deftest test-apply-validations-1
  (do (tbox-clear!)
      (repositories-registry-clear!)
      (connections-clear!)
      (tbox-register-name! :test (build-uri :rdf "a") :owl-datatype-property)
      (tbox-register-validation-on! :create :rdf "a" (fn [ & args] (do (is (= (count args) 4)) true)))
      (tbox-register-validation-on! :create :rdf "a" (fn [ & args] (do (is (= (count args) 4)) true)))
      (let [new-tbox @*tbox*]
        (do
          (is (= true
                 (apply-validations :create (build-uri :rdf "a") (list 1 2 3 4))))))))

(deftest test-apply-validations-2
  (do (tbox-clear!)
      (repositories-registry-clear!)
      (connections-clear!)
      (tbox-register-name! :test (build-uri :rdf "a") :owl-datatype-property)
      (tbox-register-validation-on! :create :rdf "a" (fn [ & args] (do (is (= (count args) 4)) true)))
      (tbox-register-validation-on! :create :rdf "a" (fn [ & args] (do (is (= (count args) 4)) false)))
      (let [new-tbox @*tbox*]
        (do
          (is (= false
                 (apply-validations :create (build-uri :rdf "a") (list 1 2 3 4))))))))

(deftest test-abox-create-individual-1
  (let [repo (init-memory-repository!)
        graph (describe-tbox
               (describe-owl-datatype-property "http://test.com/prop_a" (xsd-decimal))
               (describe-owl-datatype-property "http://test.com/prop_b" (xsd-string))
               (describe-owl-class "http://test.com/class_a")
               (describe-owl-class "http://test.com/class_b")
               (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_b")
               (describe-owl-class-has-property "http://test.com/class_a" "http://test.com/prop_a")
               (describe-owl-class-has-property "http://test.com/class_b" "http://test.com/prop_b"))]
    (do (tbox-clear!)
        (repositories-registry-clear!)
        (connections-clear!)
        (register-repository! :test repo)
        (write-graph-in-repository! graph (connection! :test))
        (tbox-register-class! :owl-thing (owl-Thing) :test)
        (tbox-register-class! :class_a "http://test.com/class_a" :test)
        (tbox-register-class! :class_b "http://test.com/class_b" :test)
        (tbox-register-datatype-property! :prop_a "http://test.com/prop_a" :test)
        (tbox-register-datatype-property! :prop_b "http://test.com/prop_b" :test)
        (let [individual (abox-create-individual :class_a "http://test.com/individuals#" {:prop_a 1, :prop_b "hola"})]
          (do
            (is (= (set (map (fn [c] (uri-to-string (:uri c))) (:classes individual)))
                   (set ["http://test.com/class_a"])))
            (is (= (null? (:identifier individual) )
                   false))
            (is (= (:properties-value-map individual)
                   {"http://test.com/prop_a" {:value 1, :datatype {:prefix :xsd, :value "decimal"}, :lang ""}
                    "http://test.com/prop_b" {:value "hola", :datatype {:prefix :xsd, :value "string"}, :lang ""}})))))))

(deftest test-abox-create-individual-2
  (let [repo (init-memory-repository!)
        graph (describe-tbox
               (describe-owl-datatype-property "http://test.com/prop_a" (xsd-decimal))
               (describe-owl-datatype-property "http://test.com/prop_b" (xsd-string))
               (describe-owl-class "http://test.com/class_a")
               (describe-owl-class "http://test.com/class_b")
               (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_b")
               (describe-owl-class-has-property "http://test.com/class_a" "http://test.com/prop_a")
               (describe-owl-class-has-property "http://test.com/class_b" "http://test.com/prop_b"))]
    (do (tbox-clear!)
        (repositories-registry-clear!)
        (connections-clear!)
        (register-repository! :test repo)
        (write-graph-in-repository! graph (connection! :test))
        (tbox-register-class! :owl-thing (owl-Thing) :test)
        (tbox-register-class! :class_a "http://test.com/class_a" :test)
        (tbox-register-class! :class_b "http://test.com/class_b" :test)
        (tbox-register-datatype-property! :prop_a "http://test.com/prop_a" :test)
        (tbox-register-datatype-property! :prop_b "http://test.com/prop_b" :test)
        (tbox-register-validation-on! :create :prop_a (fn [value-prop]
                                                        (if (> (:value value-prop) 1)
                                                          false
                                                          true)))
        (is (= (try
                (do
                  (abox-create-individual :class_a "http://test.com/individuals#" {:prop_a 15, :prop_b "hola"})
                  true)
                (catch Exception _ false))
               false)))))

(deftest test-abox-create-individual-3
  (let [repo (init-memory-repository!)
        graph (describe-tbox
               (describe-owl-datatype-property "http://test.com/prop_a" (xsd-decimal))
               (describe-owl-datatype-property "http://test.com/prop_b" (xsd-string))
               (describe-owl-class "http://test.com/class_a")
               (describe-owl-class "http://test.com/class_b")
               (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_b")
               (describe-owl-class-has-property "http://test.com/class_a" "http://test.com/prop_a")
               (describe-owl-class-has-property "http://test.com/class_b" "http://test.com/prop_b"))]
    (do (tbox-clear!)
        (repositories-registry-clear!)
        (connections-clear!)
        (register-repository! :test repo)
        (write-graph-in-repository! graph (connection! :test))
        (tbox-register-class! :owl-thing (owl-Thing) :test)
        (tbox-register-class! :class_a "http://test.com/class_a" :test)
        (tbox-register-class! :class_b "http://test.com/class_b" :test)
        (tbox-register-datatype-property! :prop_a "http://test.com/prop_a" :test)
        (tbox-register-datatype-property! :prop_b "http://test.com/prop_b" :test)
        (let [individual (abox-create-individual :class_a "http://test.com/individuals" {:prop_a 1, :prop_b "hola"})]
          (is (= (. (:uri individual) (startsWith "http://test.com/individuals#"))
                 true))))))


(deftest test-list-properties-for-individual-1
  (let [repo (init-memory-repository!)
        graph (describe-tbox
               (describe-owl-datatype-property "http://test.com/prop_a" (xsd-decimal))
               (describe-owl-datatype-property "http://test.com/prop_b" (xsd-string))
               (describe-owl-class "http://test.com/class_a")
               (describe-owl-class "http://test.com/class_b")
               (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_b")
               (describe-owl-class-has-property "http://test.com/class_a" "http://test.com/prop_a")
               (describe-owl-class-has-property "http://test.com/class_b" "http://test.com/prop_b"))]
    (do (tbox-clear!)
        (repositories-registry-clear!)
        (connections-clear!)
        (register-repository! :test repo)
        (write-graph-in-repository! graph (connection! :test))
        (tbox-register-class! :owl-thing (owl-Thing) :test)
        (tbox-register-class! :class_a "http://test.com/class_a" :test)
        (tbox-register-class! :class_b "http://test.com/class_b" :test)
        (tbox-register-datatype-property! :prop_a "http://test.com/prop_a" :test)
        (tbox-register-datatype-property! :prop_b "http://test.com/prop_b" :test)
        (is (= (let [individual (abox-create-individual :class_a "http://test.com/individuals#" {:prop_a 15, :prop_b "hola"})]
                 (abox-list-properties-for individual))
               {"http://test.com/prop_a" {:value 15, :datatype {:prefix :xsd, :value "decimal"}, :lang ""}
                "http://test.com/prop_b" {:value "hola", :datatype {:prefix :xsd, :value "string"}, :lang ""}})))))

(deftest test-abox-individual-to-graph-1
  (let [repo (init-memory-repository!)
        graph (describe-tbox
               (describe-owl-datatype-property "http://test.com/prop_a" (xsd-decimal))
               (describe-owl-datatype-property "http://test.com/prop_b" (xsd-string))
               (describe-owl-class "http://test.com/class_a")
               (describe-owl-class "http://test.com/class_b")
               (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_b")
               (describe-owl-class-has-property "http://test.com/class_a" "http://test.com/prop_a")
               (describe-owl-class-has-property "http://test.com/class_b" "http://test.com/prop_b"))]
    (do (tbox-clear!)
        (repositories-registry-clear!)
        (connections-clear!)
        (register-repository! :test repo)
        (write-graph-in-repository! graph (connection! :test))
        (tbox-register-class! :owl-thing (owl-Thing) :test)
        (tbox-register-class! :class_a "http://test.com/class_a" :test)
        (tbox-register-class! :class_b "http://test.com/class_b" :test)
        (tbox-register-datatype-property! :prop_a "http://test.com/prop_a" :test)
        (tbox-register-datatype-property! :prop_b "http://test.com/prop_b" :test)
        (let [individual  (abox-create-individual :class_a "http://test.com/individuals#" {:prop_a 15, :prop_b "hola"})
              individual-identifier (:identifier individual)
              individual-graph (abox-individual-to-graph individual)]
          (do
            (is (= (count (:nodes individual-graph)) 2))
            (is (= (set (:relations (first (:nodes individual-graph))))
                   (set [{:value {:prefix :rdf, :value "type"}
                          :object {:value {:prefix "", :value "http://test.com/class_a"} :relations []}}
                         {:value {:prefix :rdf, :value "type"}
                          :object {:value {:prefix "", :value "http://www.w3.org/2002/07/owl#Thing"} :relations []}}
                         {:value {:prefix :rdf, :value "type"}
                          :object {:value {:prefix "", :value "http://test.com/class_b"} :relations []}}])))
            (is (= (set (:relations (second (:nodes individual-graph))))
                   (set [{:value {:prefix "", :value "http://test.com/prop_a"}
                          :object {:value {:value 15, :datatype {:prefix :xsd, :value "decimal"}, :lang ""}, :relations []}}
                         {:value {:prefix "", :value "http://test.com/prop_b"}
                          :object {:value {:value "hola", :datatype {:value "string", :prefix :xsd}, :lang ""}, :relations []}}]))))))))

(deftest test-abox-create-individual!-1
  (let [repo (init-memory-repository!)
        graph (describe-tbox
               (describe-owl-datatype-property "http://test.com/prop_a" (xsd-decimal))
               (describe-owl-datatype-property "http://test.com/prop_b" (xsd-string))
               (describe-owl-class "http://test.com/class_a")
               (describe-owl-class "http://test.com/class_b")
               (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_b")
               (describe-owl-class-has-property "http://test.com/class_a" "http://test.com/prop_a")
               (describe-owl-class-has-property "http://test.com/class_b" "http://test.com/prop_b"))
        test-template (build-graph-template
                       [{:template (build-graph
                                    [(build-variable-node :subject
                                                          [(build-variable-relation :predicate
                                                                         (build-variable-node :object))])])
                         :filters []}])]
    (do (tbox-clear!)
        (repositories-registry-clear!)
        (connections-clear!)
        (register-repository! :test repo)
        (write-graph-in-repository! graph (connection! :test))
        (tbox-register-class! :owl-thing (owl-Thing) :test)
        (tbox-register-class! :class_a "http://test.com/class_a" :test)
        (tbox-register-class! :class_b "http://test.com/class_b" :test)
        (tbox-register-datatype-property! :prop_a "http://test.com/prop_a" :test)
        (tbox-register-datatype-property! :prop_b "http://test.com/prop_b" :test)
        (let [individual (abox-create-individual! :class_a "http://test.com/individuals#" {:prop_a 15, :prop_b "hola"} (connection! :test))
              identifier (:uri individual)
              result (query-template-in-repository
                      :subject :predicate :object
                      test-template
                      (connection! :test))]
          (is (= (set result)
                 (set [{:object {:prefix "", :value "http://test.com/class_a"}
                        :predicate {:prefix "", :value "http://www.w3.org/2000/01/rdf-schema#domain"}
                        :subject {:prefix "", :value "http://test.com/prop_a"}}
                       {:object {:prefix "", :value "http://test.com/class_b"}
                        :predicate {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"}
                        :subject {:prefix "", :value identifier}}
                       {:object {:prefix "", :value "http://test.com/class_a"}
                        :predicate {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"}
                        :subject {:prefix "", :value identifier}}
                       {:object {:prefix "", :value "http://www.w3.org/2002/07/owl#Thing"}
                        :predicate {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"}
                        :subject {:prefix "", :value identifier}}
                       {:object {:prefix "", :value "http://www.w3.org/2002/07/owl#DatatypeProperty"}
                        :predicate {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"}
                        :subject {:prefix "", :value "http://test.com/prop_a"}}
                       {:object {:prefix "", :value "http://test.com/class_b"}
                        :predicate {:prefix "", :value "http://www.w3.org/2000/01/rdf-schema#domain"}
                        :subject {:prefix "", :value "http://test.com/prop_b"}}
                       {:object {:value "hola", :datatype {:prefix "", :value "http://www.w3.org/2001/XMLSchema#string"}, :lang ""}
                        :predicate {:prefix "", :value "http://test.com/prop_b"}
                        :subject {:prefix "", :value identifier}}
                       {:object {:prefix "", :value "http://www.w3.org/2002/07/owl#Class"}
                        :predicate {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"}
                        :subject {:prefix "", :value "http://test.com/class_a"}}
                       {:object {:prefix "", :value "http://www.w3.org/2001/XMLSchema#string"}
                        :predicate {:prefix "", :value "http://www.w3.org/2000/01/rdf-schema#range"}
                        :subject {:prefix "", :value "http://test.com/prop_b"}}
                       {:object {:prefix "", :value "http://www.w3.org/2002/07/owl#Class"}
                        :predicate {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"}
                        :subject {:prefix "", :value "http://test.com/class_b"}}
                       {:object {:value "15", :datatype {:prefix "", :value "http://www.w3.org/2001/XMLSchema#decimal"}, :lang ""}
                        :predicate {:prefix "", :value "http://test.com/prop_a"}
                        :subject {:prefix "", :value identifier}}
                       {:object {:prefix "", :value "http://www.w3.org/2002/07/owl#DatatypeProperty"}
                        :predicate {:prefix "", :value "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"}
                        :subject {:prefix "", :value "http://test.com/prop_b"}}
                       {:object {:prefix "", :value "http://test.com/class_b"}
                        :predicate {:prefix "", :value "http://www.w3.org/2000/01/rdf-schema#subClassOf"}
                        :subject {:prefix "", :value "http://test.com/class_a"}}
                       {:object {:prefix "", :value "http://www.w3.org/2002/07/owl#Thing"}
                        :predicate {:prefix "", :value "http://www.w3.org/2000/01/rdf-schema#subClassOf"}
                        :subject {:prefix "", :value "http://test.com/class_a"}}
                       {:object {:prefix "", :value "http://www.w3.org/2002/07/owl#Thing"}
                        :predicate {:prefix "", :value "http://www.w3.org/2000/01/rdf-schema#subClassOf"}
                        :subject {:prefix "", :value "http://test.com/class_b"}}
                       {:object {:prefix "", :value "http://www.w3.org/2001/XMLSchema#decimal"}
                        :predicate {:prefix "", :value "http://www.w3.org/2000/01/rdf-schema#range"}
                        :subject {:prefix "", :value "http://test.com/prop_a"}}])))))))


(deftest test-instance-of-classes-1
  (let [repo (init-memory-repository!)
        graph (describe-tbox
               (describe-owl-class "http://test.com/class_a")
               (describe-owl-class "http://test.com/class_b")
               (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_b"))]
    (do (tbox-clear!)
        (repositories-registry-clear!)
        (connections-clear!)
        (register-repository! :test repo)
        (write-graph-in-repository! graph (connection! :test))
        (tbox-register-class! :owl-thing (owl-Thing) :test)
        (tbox-register-class! :class_a "http://test.com/class_a" :test)
        (tbox-register-class! :class_b "http://test.com/class_b" :test)
        (tbox-register-datatype-property! :prop_a "http://test.com/prop_a" :test)
        (tbox-register-datatype-property! :prop_b "http://test.com/prop_b" :test)
        (is (= (instance-of-classes :class_a :class_b)
               '{:template {:nodes [{:value {:value :individual}
                                     :relations ({:value {:prefix :rdf, :value "type"}
                                                  :object {:value {:prefix "", :value "http://test.com/class_a"}
                                                           :relations []}}
                                                 {:value {:prefix :rdf, :value "type"}
                                                  :object {:value {:prefix "", :value "http://test.com/class_b"}
                                                           :relations []}})}]
                            :context #{}}
                 :filters []})))))


(deftest test-instance-of-classes-2
  (let [repo (init-memory-repository!)
        graph (describe-tbox
               (describe-owl-class "http://test.com/class_a")
               (describe-owl-class "http://test.com/class_b")
               (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_b"))]
    (do (tbox-clear!)
        (repositories-registry-clear!)
        (connections-clear!)
        (register-repository! :test repo)
        (write-graph-in-repository! graph (connection! :test))
        (tbox-register-class! :owl-thing (owl-Thing) :test)
        (tbox-register-class! :class_a "http://test.com/class_a" :test)
        (tbox-register-class! :class_b "http://test.com/class_b" :test)
        (tbox-register-datatype-property! :prop_a "http://test.com/prop_a" :test)
        (tbox-register-datatype-property! :prop_b "http://test.com/prop_b" :test)
        (let [class_a (tbox-find-class! :class_a)]
          (is (= (instance-of-classes class_a :class_b)
                 '{:template {:nodes [{:value {:value :individual}
                                       :relations ({:value {:prefix :rdf, :value "type"}
                                                    :object {:value {:prefix "", :value "http://test.com/class_a"}
                                                             :relations []}}
                                                   {:value {:prefix :rdf, :value "type"}
                                                    :object {:value {:prefix "", :value "http://test.com/class_b"}
                                                             :relations []}})}]
                              :context #{}}
                   :filters []}))))))


(deftest test-not-instance-of-classes-1
  (let [repo (init-memory-repository!)
        graph (describe-tbox
               (describe-owl-class "http://test.com/class_a")
               (describe-owl-class "http://test.com/class_b")
               (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_b"))]
    (do (tbox-clear!)
        (repositories-registry-clear!)
        (connections-clear!)
        (register-repository! :test repo)
        (write-graph-in-repository! graph (connection! :test))
        (tbox-register-class! :owl-thing (owl-Thing) :test)
        (tbox-register-class! :class_a "http://test.com/class_a" :test)
        (tbox-register-class! :class_b "http://test.com/class_b" :test)
        (tbox-register-datatype-property! :prop_a "http://test.com/prop_a" :test)
        (tbox-register-datatype-property! :prop_b "http://test.com/prop_b" :test)
        (is (= (not-instance-of-classes :class_a)
               '{:template {:nodes [{:value {:value :individual}
                                     :relations [{:value {:prefix :rdf, :value "type"}
                                                  :object {:value {:prefix :owl, :value "Thing"}
                                                           :relations []}}]}
                                    {:nodes-filters [{:template
                                                      {:value {:nodes [{:value {:value :individual}
                                                                        :relations ({:value {:prefix :rdf, :value "type"}
                                                                                     :object {:value {:value :c0}
                                                                                              :relations []}})}]
                                                               :context #{}}}
                                                      :filters ({:identifier :c0, :value "?c0 = <http://test.com/class_a>"})}]}]
                            :context #{}}
                 :filters ({:identifier :c0, :value " ! bound(?c0)"})})))))

(deftest test-abox-find-individual-uris!-1
  (let [repo (init-memory-repository!)
        graph (describe-tbox
               (describe-owl-datatype-property "http://test.com/prop_a" (xsd-decimal))
               (describe-owl-datatype-property "http://test.com/prop_b" (xsd-string))
               (describe-owl-class "http://test.com/class_a")
               (describe-owl-class "http://test.com/class_b")
               (describe-owl-subclass "http://test.com/class_a" "http://test.com/class_b"))]
    (do (tbox-clear!)
        (repositories-registry-clear!)
        (connections-clear!)
        (register-repository! :test repo)
        (write-graph-in-repository! graph (connection! :test))
        (tbox-register-class! :owl-thing (owl-Thing) :test)
        (tbox-register-class! :class_a "http://test.com/class_a" :test)
        (tbox-register-class! :class_b "http://test.com/class_b" :test)
        (tbox-register-datatype-property! :prop_a "http://test.com/prop_a" :test)
        (tbox-register-datatype-property! :prop_b "http://test.com/prop_b" :test)
        (abox-create-individual! :class_a "http://test.com/individuals#" {:prop_a 15, :prop_b "hola"} (connection! :test))
        (abox-create-individual! :class_a "http://test.com/individuals#" {:prop_a 95, :prop_b "adios"} (connection! :test))
        (is (= (count
                (abox-find-individual-uris!
                 (instance-of-classes :class_a)
                 (connection! :test)))
               2)))))


(deftest test-abox-find-individual-uris!-2
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
                      (connection! :test))]
          (is (= (count result)
                 1))))))

(deftest test-build-individual-from-uri!-1
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
        (let [result-uri (abox-find-individual-uris!
                          (not-instance-of-classes :class_a)
                          (connection! :test))
              result (abox-build-individual-from-uri! (first result-uri) :test)]
          (is (= result
                 (let [identifier (:identifier result)
                       uri (:uri result)]
                 {:identifier identifier
                  :uri uri
                  :classes (set [{:name :class_c, :uri "http://test.com/class_c"
                                  :subclass-of #{
                                                 {:name :owl-thing
                                                  :uri {:prefix "", :value "http://www.w3.org/2002/07/owl#Thing"}
                                                  :subclass-of #{}
                                                  :datatype-properties #{}
                                                  :object-properties #{}
                                                  :repository-name :test}}
                                  :datatype-properties #{}
                                  :object-properties #{}
                                  :repository-name :test}
                                 {:name :owl-thing, :uri {:prefix "", :value "http://www.w3.org/2002/07/owl#Thing"}
                                  :subclass-of #{}, :datatype-properties #{}, :object-properties #{}, :repository-name :test}])
                  :properties-value-map {"http://test.com/prop_c" {:value "35", :datatype {:prefix "", :value "http://www.w3.org/2001/XMLSchema#decimal"}, :lang ""}}
                  :dirty-properties #{}
                  :created false})))))))

(deftest test-abox-remove-resource-triplets-1
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
                      (connection! :test))]
          (do (remove-graph-template-from-repository! (abox-resource-triplets (first result)) (connection! :test))
              (is (= (count
                      (abox-find-individual-uris!
                       (not-instance-of-classes :class_a)
                       (connection! :test)))
                     0)))))))

(deftest test-abox-update-properties-1
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
              updated-result (abox-update-individual-properties
                              result
                              {:prop_c 46})]
          (do
            (is (= (:properties-value-map updated-result)
                   {"http://test.com/prop_c" {:value 46, :datatype {:prefix :xsd, :value "decimal"}, :lang ""}}))
            (is (= (:dirty-properties updated-result)
                   (set ["http://test.com/prop_c"]))))))))


(deftest test-abox-destroy-1
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
        (let [result (abox-find-individual-uris!
                      (instance-of-classes :class_a)
                      (connection! :test))]
          (do
            (abox-destroy-individual-by-uri (first result) (connection! :test))
            (is (= nil
                   (abox-find-individual-uris! (not-instance-of-classes :class_a) (connection! :test)))))))))