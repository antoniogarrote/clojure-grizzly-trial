package com.agh.webserver.rack;

import clojure.lang.RT;
import com.sun.grizzly.tcp.http11.GrizzlyRequest;

/**
 *
 * @author Antonio Garrote Hernandez
 */

public interface RackApplication {
    RackResponse call(GrizzlyRequest request);
    void destroy();
    RT getRuntime();
}
