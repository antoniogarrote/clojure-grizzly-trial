(ns routing)

(use 'com.agh.webserver.framework.router)

(use 'tests)

(route! (url-pattern GET "greetings" :name) say-hello)