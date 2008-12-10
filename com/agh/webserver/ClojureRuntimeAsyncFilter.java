package com.agh.webserver;

import com.sun.grizzly.arp.AsyncExecutor;
import com.sun.grizzly.arp.AsyncFilter;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author antonio
 */
public class ClojureRuntimeAsyncFilter implements AsyncFilter {

    public boolean doFilter(AsyncExecutor executor) {
        System.out.println("ClojureRuntimeAsyncFilter::doFilter");
        try {
            executor.execute();
            executor.postExecute();
        } catch (Exception ex) {
            System.out.println("ClojureRuntimeAsyncFilter::doFilter EXCEPTION"+ex.getMessage());
        }
        return true;
    }

}
