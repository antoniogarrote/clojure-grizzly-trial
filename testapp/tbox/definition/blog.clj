(use 'com.agh.webserver.framework.persistence.rdf.vocabularies.owl)
;;(register-repository! (init-memory-repository!))

(describe-tbox
 ;; classes
 (describe-owl-class "http://test.com/Blog")
 (describe-owl-class "http://test.com/Post")
 (describe-owl-class "http://test.com/User")
 (describe-owl-class "http://test.com/Comment")
 ;; datatype properties
 (describe-owl-datatype-property "http://test.com/name" (xsd-string))
 (describe-owl-datatype-property "http://test.com/content" (xsd-string))
 (describe-owl-datatype-property "http://test.com/email" (xsd-string))
 ;; class-datatype properties
 (describe-owl-class-has-property "http://test.com/Blog" "http://test.com/name")
 (describe-owl-class-has-property "http://test.com/Post" "http://test.com/name")
 (describe-owl-class-has-property "http://test.com/Post" "http://test.com/content")
 (describe-owl-class-has-property "http://test.com/User" "http://test.com/name")
 (describe-owl-class-has-property "http://test.com/User" "http://test.com/email"))
