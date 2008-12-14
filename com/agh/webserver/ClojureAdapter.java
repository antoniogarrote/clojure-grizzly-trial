/**
 * 
 * Port of com.sun.grizzly.jruby.RailsAdapter
 *
 */

package com.agh.webserver;

import com.agh.webserver.rack.DefaultRackApplicationFactory;
import com.agh.webserver.rack.MultiThreadedRackAdapter;
import com.agh.webserver.rack.RackApplication;
import com.agh.webserver.rack.RackApplicationFactory;
import com.sun.grizzly.tcp.Request;
import com.sun.grizzly.tcp.Response;
import com.sun.grizzly.tcp.StaticResourcesAdapter;
import com.sun.grizzly.tcp.http11.GrizzlyAdapter;
import com.sun.grizzly.tcp.http11.GrizzlyRequest;
import com.sun.grizzly.tcp.http11.GrizzlyResponse;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Antonio Garrote Hernandez
 */
public class ClojureAdapter extends GrizzlyAdapter{

    private final String contextRoot;
    private final String appRoot;
    private final int runtimes;
    private final int minRuntimes;
    private final int maxRuntimes;
    private final boolean asyncExecution;
    private final int numThreads;

    private Logger logger;

    private MultiThreadedRackAdapter handler;
    private ClojureRuntimeAsyncFilter asyncFilter;
    private RackApplicationFactory factory;
    private StaticResourcesAdapter staticAdapter;


    public ClojureAdapter(String contextRoot, String clojureRoot, int numberOfRuntime, int minRt, int maxRt, boolean asyncExecution, ClojureRuntimeAsyncFilter asyncFilter) {
        super(clojureRoot);

        this.asyncFilter = asyncFilter;
        this.logger = Logger.getLogger("Clojure::RackAdapter::Logger");

        this.staticAdapter = new StaticResourcesAdapter(rootFolder+"public");
        this.setRootFolder(rootFolder+"public");
        this.contextRoot = contextRoot;
        this.appRoot = clojureRoot;
        this.runtimes = numberOfRuntime;
        this.maxRuntimes = maxRt;
        this.minRuntimes = minRt;
        this.asyncExecution = asyncExecution;

        numThreads = Math.min(Runtime.getRuntime().availableProcessors(), numberOfRuntime);
        handler = new MultiThreadedRackAdapter(new DefaultRackApplicationFactory(this), this);
        factory = handler.getFactory();
    }

    ClojureAdapter(String clojureRoot, int numberOfRuntime, int minRt, int maxRt, boolean asyncExecution, ClojureRuntimeAsyncFilter asyncFilter) {
        this("/",clojureRoot,numberOfRuntime,minRt,maxRt,asyncExecution,asyncFilter);
    }

    public int getRuntimes() {
        return runtimes;
    }

    public int getMinRuntimes() {
        return minRuntimes;
    }

    public int getMaxRuntimes() {
        return maxRuntimes;
    }

    public boolean async() {
        return asyncExecution;
    }

    public int getNumThreads() {
        return numThreads;
    }

    public String getContextRoot(){
        return contextRoot;
    }

    public String getAppRoot() {
        return appRoot;
    }

    public Logger getLogger() {
        return logger;
    }

    @Override
    public void service(GrizzlyRequest req, GrizzlyResponse res) {
        System.out.println("clojureAdapter::service");
         RackApplication serviceApp = null;
        try {
            // Borrow a Runtime
            serviceApp = handler.getApp();

            if (serviceApp == null) {
                throw new IllegalStateException();
            }
            // Leave all processing to the runtime
            dispatchClojureRequest(serviceApp, req, res);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
/*
            if (serviceApp != null) {
                handler.returnApp(serviceApp);
                if (asyncFilter != null) {
                    asyncFilter.resume();

                }
            }
 */
        }
    }

    private void dispatchClojureRequest(final RackApplication app, GrizzlyRequest req, GrizzlyResponse res) throws IOException {
        System.out.println("clojureAdapter::dispatchRailsRequest");
        try {
            app.call(req).respond(res);
        } catch (Exception e) {
            res.setError();
            e.printStackTrace();
            if (res.isCommitted()) {
                logger.log(Level.WARNING, "Error: Couldn't handle error: response committed", e);
                return;
            }
            res.reset();

            try {
                RackApplication errorApp = factory.getErrorApplication(app.getRuntime());
                req.setAttribute("rack.exception", e);
                logger.log(Level.WARNING, e.getMessage(), e);
                errorApp.call(req).respond(res);
            } catch (Exception ex) {
                logger.log(Level.WARNING, "Error: Couldn't handle error", ex);
                res.sendError(500);
            }
        }
    }

}
