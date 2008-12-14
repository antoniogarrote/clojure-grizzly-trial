package com.agh.webserver;
        
import clojure.lang.RT;
import com.sun.grizzly.tcp.StaticResourcesAdapter;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Main {

    private static final String MAINCLJ = "com/agh/webserver/webserver.clj";
    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {   
/*
        try {
            RT.loadResourceScript(MAINCLJ);
            Object result = RT.var("com.agh.webserver", "main").invoke(args);
            System.out.println("Fin");
        } catch(Exception e) {
            e.printStackTrace();
        }
*/

        ClojureSelectorThread selectorThread = new ClojureSelectorThread();
        selectorThread.setClojureRoot("/Users/antonio/Desktop/clj-web-app/");
        selectorThread.setPort(8080);
        selectorThread.setAdapter(new ClojureAdapter("/", 3, 1, 5, true, new ClojureRuntimeAsyncFilter()));
        try {
            selectorThread.initEndpoint();
            selectorThread.startEndpoint();
        } catch (IOException ex) {
            System.out.println(ex.getMessage());
        } catch (InstantiationException ex) {
            System.out.println(ex.getMessage());
        }

    }
} 
