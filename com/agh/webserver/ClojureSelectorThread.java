package com.agh.webserver;

import com.sun.grizzly.arp.AsyncFilter;
import com.sun.grizzly.arp.DefaultAsyncHandler;
import com.sun.grizzly.http.SelectorThread;
import com.sun.grizzly.standalone.StaticStreamAlgorithm;
import java.io.IOException;
import java.util.logging.Level;

/**
 *
 * @author antonio
 */
public class ClojureSelectorThread extends SelectorThread {

    private final static String NUMBER_OF_RUNTIME =
            "clojure.numberOfRuntime";

    protected String clojureRoot;
    protected String clojurePublicPath;

    private int numberOfRuntime = 5;
    private int maxRt = -1; //defaults to 2
    private int minRt = -1; //defaults to 1

    public String getClojureRoot() {
        return clojureRoot;
    }

    public void setClojurePublicPath(String path) {
        this.clojurePublicPath = path;
    }

    public String getClojurePublicPath() {
        return this.clojurePublicPath;
    }
    
    @Override
    public void initEndpoint() throws IOException, InstantiationException {
        System.out.println("ClojureSelectorThread::initEndpoint");
        setupSystemProperties();
        asyncExecution = true;
        ClojureRuntimeAsyncFilter asyncFilter = new ClojureRuntimeAsyncFilter();
        adapter = new ClojureAdapter(clojureRoot, numberOfRuntime, minRt, maxRt, asyncExecution, asyncFilter);

        setWebAppRootPath(getClojurePublicPath());
        setBufferResponse(false);

        DefaultAsyncHandler asyncHandler = new DefaultAsyncHandler();
        setAsyncHandler(asyncHandler);
        asyncHandler.addAsyncFilter(asyncFilter);

        algorithmClassName = StaticStreamAlgorithm.class.getName();
        super.initEndpoint();
    }

    public void setNumberOfRuntime(int numberOfRuntime) {
        this.numberOfRuntime = numberOfRuntime;
    }

    public void setClojureRoot(String clojureRoot) {
        this.clojureRoot = clojureRoot;
    }

    @Override
    public synchronized void stopEndpoint() {
        //((ClojureAdapter) adapter).stopRubyRuntimePool();
        super.stopEndpoint();
    }

    protected void setupSystemProperties() {

        if (System.getProperty(NUMBER_OF_RUNTIME) != null) {
            try {
                numberOfRuntime = Integer.parseInt(
                        System.getProperty(NUMBER_OF_RUNTIME));
            } catch (NumberFormatException ex) {
                SelectorThread.logger().log(Level.WARNING,
                        "Invalid number of Runtime: " + System.getProperty(NUMBER_OF_RUNTIME));
            }
        }

        //TODO: provide CLI options for max/min runtimes
        if (System.getProperty("clojure.runtime.max") != null) {
            try {
                maxRt = Integer.parseInt(System.getProperty("jruby.runtime.max"));
            } catch (NumberFormatException ex) {
                SelectorThread.logger().log(Level.WARNING,
                        "Invalid number of max runtime: " + System.getProperty("jruby.runtime.max"));
            }
        }
        if (System.getProperty("clojure.runtime.min") != null) {
             try {
                 minRt = Integer.parseInt(System.getProperty("jruby.runtime.min"));
             } catch (NumberFormatException ex) {
                 SelectorThread.logger().log(Level.WARNING,
                         "Invalid number of min runtime: " + System.getProperty("jruby.runtime.min"));
             }
         }
    }
}
