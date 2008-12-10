package com.agh.webserver.rack;

/**
 *
 * @author Antonio Garrote Hernandez
 */
import clojure.lang.RT;
import com.agh.webserver.ClojureAdapter;
import java.util.logging.Level;

public class MultiThreadedRackAdapter implements RackAdapter {
    private RackApplication theApp;
    private final RackApplicationFactory myFactory;
    private final ClojureAdapter adapter;

    // Starts this adapter up
    public MultiThreadedRackAdapter(RackApplicationFactory f, ClojureAdapter adapter) {
        this.myFactory = f;
        this.adapter = adapter;

        //setup GrizzlyContext
        try {
            theApp = myFactory.newApplication(new RT());
        } catch(RackInitializationException e) {
            adapter.getLogger().log(Level.SEVERE, e.getMessage(), e);
            theApp = myFactory.getErrorApplication(new RT());
        }
    }

    // This method should be removed when glassfish/java.util.logger actually implements this
    private String getEffectiveLogLevel() {
        Level myLevel;
        java.util.logging.Logger pLog = adapter.getLogger();
        myLevel = pLog.getLevel();
        while (myLevel == null) {
            pLog = pLog.getParent();
            myLevel = pLog.getLevel();
        }
        return myLevel.getName();
    }

    // Shuts the adapter down
    public void shutdown() {
        // Nothing by default
    }// Returns an application suitable for processing a request
    public RackApplication getApp() {
        return theApp;
    }// Indicates that the app is done processing the request
    public void returnApp(RackApplication returned) {
        // Nothing by default
    }
    public RackApplicationFactory getFactory() {
        return myFactory;
    }
}
