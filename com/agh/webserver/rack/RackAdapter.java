package com.agh.webserver.rack;

/**
 *
 * @author Antonio Garrote Hernandez
 */
public interface RackAdapter {
    // Shuts the adapter down
    public void shutdown();
    // Returns an application suitable for processing a request
    public RackApplication getApp();
    // Indicates that the app is done processing the request
    public void returnApp(RackApplication returned);
    // get the factory involved
    public RackApplicationFactory getFactory();

}
