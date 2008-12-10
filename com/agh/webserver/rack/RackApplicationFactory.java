package com.agh.webserver.rack;

import clojure.lang.RT;

/**
 * @author Antonio Garrote Hernández
 */
public interface RackApplicationFactory {
    /** Create a new application */
    RackApplication newApplication(RT runtime) throws RackInitializationException;

    /** Get the designated error application. The error application is expected
     to be a singleton and should not be returned to the factory. */
    RackApplication getErrorApplication(RT runtime);

    /** Destroy the factory. */
    void destroy();

}
