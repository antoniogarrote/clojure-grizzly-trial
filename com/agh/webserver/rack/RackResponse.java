package com.agh.webserver.rack;

import com.sun.grizzly.tcp.http11.GrizzlyResponse;

import java.util.Map;

/**
 * @author Antonio Garrote Hernandez
 */

public interface RackResponse {
    /** Return the response status. */
    int getStatus();

    /** Return the response headers. */
    Map getHeaders();

    /** Return the response body */
    String getBody();

    /** Write the status, headers, and body to the response. */
    void respond(GrizzlyResponse response);
}