package com.agh.webserver;

import clojure.lang.RT;
import clojure.lang.Var;

/**
 *
 * @author Antonio Garrote Hernandez
 */
public class ClojureRTWrapper {

    public static void loadFramework() throws Exception {
        try {
            System.out.println("Starting clojure resources");
            RT.loadResourceScript("com/agh/webserver/framework/boot.clj");
            Var form = RT.var("com.agh.webserver.framework.boot", "boot");
            form.invoke();
        } catch (Exception ex) {
            System.err.println("Error booting framework: "+ex.getMessage());
            throw ex;
        }
    }
    
}
