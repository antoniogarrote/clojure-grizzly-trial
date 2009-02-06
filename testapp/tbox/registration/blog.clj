(tbox-register-class! :owl-thing (owl-Thing))

(tbox-register-class! :test-blog "http://test.com/Blog")
(tbox-register-class! :test-post "http://test.com/Post")
(tbox-register-class! :test-user "http://test.com/User")
(tbox-register-class! :test-comment "http://test.com/Comment")

(tbox-register-datatype-property! :name "http://test.com/name")
(tbox-register-datatype-property! :content "http://test.com/content")
(tbox-register-datatype-property! :email "http://test.com/email")