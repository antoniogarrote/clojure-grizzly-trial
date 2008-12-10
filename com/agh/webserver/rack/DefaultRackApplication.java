package com.agh.webserver.rack;

import clojure.lang.RT;
import com.agh.webserver.ClojureAdapter;
import com.sun.grizzly.tcp.ActionCode;
import com.sun.grizzly.tcp.Request;
import com.sun.grizzly.tcp.http11.GrizzlyRequest;
import com.sun.grizzly.util.buf.ByteChunk;
import com.sun.grizzly.util.buf.MessageBytes;
import com.sun.grizzly.util.http.MimeHeaders;


import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.channels.Channels;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Vivek Pandey
 */
public abstract class DefaultRackApplication implements RackApplication {
    private final Logger logger;
    private final String rack_version;
    private final String rack_multithread;
    private final String rack_multiprocess;
    private final String rack_run_once;
    private final HashMap base;
    private final String rack_input;
    private final String rack_errors;
    private final String rack_url_scheme;


    //Request methods
    private final String req_method;
    private final String script_name;
    private final String request_uri;
    private final String path_info;
    private final String query_string;
    private final String path_translated;
    private final String server_name;
    private final String remote_host;
    private final String remote_addr;
    private final String remote_user;
    private final String server_port;
    private final String content_type;
    private final String content_length;

    private final String output_string;
    private final String output_headers;
    private final String output_status;
    
    private final ClojureAdapter adapter;

    public DefaultRackApplication(ClojureAdapter adapter) {
        this.logger = adapter.getLogger();
        this.adapter = adapter;

        base = new HashMap();

        rack_version = new String("rack.version");
        rack_multithread = new String("rack.multithread");
        rack_multiprocess = new String("rack.multiprocess");
        rack_run_once = new String("rack.multiprocess");
        rack_input = new String("rack.input");
        rack_errors = new String("rack.errors");
        rack_url_scheme = new String("rack.url_scheme");

        base.put(rack_version, RackInfo.VERSION);
        base.put(rack_multithread, true);
        base.put(rack_multiprocess, true);
        base.put(rack_run_once, false);
        base.put(rack_errors, Logger.getLogger("Clojure::Rack::GrizzlyLog"));

        req_method = new String("REQUEST_METHOD");
        script_name = new String("SCRIPT_NAME");
        request_uri = new String("REQUEST_URI");
        path_info = new String("PATH_INFO");
        query_string = new String("QUERY_STRING");
        path_translated = new String("PATH_TRANSLATED");
        server_name = new String("SERVER_NAME");
        remote_host = new String("REMOTE_HOST");
        remote_addr = new String("REMOTE_ADDR");
        remote_user = new String("REMOTE_USER");
        server_port = new String("SERVER_PORT");
        content_type = new String("CONTENT_TYPE");
        content_length = new String("CONTENT_LENGTH");

        output_string = new String("clojure.output.stream");
        output_headers = new String("clojure.output.headers");
        output_status = new String("clojure.output.status");
        
        //base.put(output_string, new PrintWriter(new StringWriter()));
        base.put(output_string, "");
        base.put(output_status, "200");
        base.put(output_headers,new HashMap());
    }

    public RackResponse call(final GrizzlyRequest grizzlyRequest) {
        HashMap rackEnv = new HashMap(base);

        Request request = grizzlyRequest.getRequest();

        addReqInfo(grizzlyRequest, rackEnv);
        try {
            populateFromMimeHeaders(rackEnv, request.getMimeHeaders());
        } catch (IOException ex) {
            this.logger.severe("Error populating mime headers:"+ex.getMessage());
        }
        populateFromMap(rackEnv, request.getAttributes());
        try {
            rackEnv.put(rack_input, Channels.newChannel(grizzlyRequest.getInputStream()));
        } catch (IOException ex) {
            this.logger.severe("Error retrieving rack_input:"+ex.getMessage());
        }
        rackEnv.put(rack_url_scheme, newStringFromMessageBytes(request.scheme()));

        return  this.callMethod("call", rackEnv);
    }

    private void addReqInfo(GrizzlyRequest grequest, HashMap hash){
        Request request = grequest.getRequest();

        String reqUri;

        //skip context path
        if(adapter.getContextRoot().length() > 1){
            byte[] bytes = request.requestURI().getByteChunk().getBuffer();
            byte[] contextpath = adapter.getContextRoot().getBytes();

            boolean hasContextRoot = true;
            int offset=request.requestURI().getByteChunk().getOffset();

            for (int i = 0; i < contextpath.length; i++) {
                if (bytes[offset] != contextpath[i]) {
                    hasContextRoot = false;
                    break;
                }

                if (offset < bytes.length) {
                    offset++;
                }
            }
            if(hasContextRoot){
                int lenoffset = (offset > request.requestURI().getByteChunk().getOffset())?offset-request.requestURI().getByteChunk().getOffset():offset;
                
                reqUri = new String(
                bytes,
                offset,
                request.requestURI().getByteChunk().getLength() - lenoffset);
            }else{
                reqUri = newStringFromMessageBytes(request.requestURI());
            }
           hash.put(request_uri, reqUri);
        }else{
            reqUri = newStringFromMessageBytes(request.requestURI());
        }

        hash.put(path_info, reqUri);
        hash.put(path_translated, reqUri);

        hash.put(req_method, newStringFromMessageBytes(request.method()));
        hash.put(query_string, newStringFromMessageBytes(request.queryString()));

         //grizzly reads these headers lazily, so need to send actions.
        request.action(ActionCode.ACTION_REQ_LOCAL_NAME_ATTRIBUTE, null);
        hash.put(server_name, newStringFromMessageBytes(request.serverName()));
        request.action(ActionCode.ACTION_REQ_HOST_ATTRIBUTE, null);
        hash.put(remote_host, newStringFromMessageBytes(request.remoteHost()));
        request.action(ActionCode.ACTION_REQ_HOST_ADDR_ATTRIBUTE, null);
        hash.put(remote_addr, newStringFromMessageBytes(request.remoteAddr()));
        request.action(ActionCode.ACTION_REQ_LOCALPORT_ATTRIBUTE, null);
        hash.put(server_port, Integer.toString(request.getLocalPort()));

        //There is no action for remote user from grizzly. Hopefully it will be available.
        //TODO: Need to test
        hash.put(remote_user, newStringFromMessageBytes(request.getRemoteUser()));

        hash.put(script_name, new String(""));

        if(request.getContentLength() > 0){
            hash.put(content_length, new String(Integer.toString(request.getContentLength())));
        }

        hash.put(content_type, newStringFromMessageBytes(request.contentType()));
    }

    public abstract RackResponse callMethod(String cmd, HashMap rackEnv);

    private void populateFromMap(HashMap env, Map source) {
        for (Object obj : source.entrySet()) {
            Map.Entry entry = (Map.Entry) obj;
            env.put(
                    newStringFromMessageBytes((MessageBytes) entry.getKey()),
                    newStringFromMessageBytes((MessageBytes) entry.getValue()));
        }
    }

    private void populateFromMimeHeaders(HashMap env, MimeHeaders source) throws IOException {
        for (int i = 0; i < source.size(); i++) {
            MessageBytes key = source.getName(i);

            //Rack spec disallows adding content-type and content-length with HTTP_ prefix
            //Need to find a way to ignore it.
            if(key.startsWithIgnoreCase("content-type", 0) ||
                    key.startsWithIgnoreCase("content-length", 0))
                continue;

            MessageBytes mb = MessageBytes.newInstance();

            byte[] httpKey = {'H', 'T', 'T', 'P','_'};
            mb.getByteChunk().append(httpKey, 0, httpKey.length);
            byte[] bytes = key.getByteChunk().getBuffer();
            for (int k = 0; k < key.getByteChunk().getLength(); k++) {
                bytes[key.getByteChunk().getOffset() + k] = (byte) Character.toUpperCase(bytes[key.getByteChunk().getOffset() + k]);
                if (bytes[key.getByteChunk().getOffset() + k] == '-')
                    bytes[key.getByteChunk().getOffset() + k] = '_';
            }
            mb.getByteChunk().append(bytes, key.getByteChunk().getOffset(), key.getByteChunk().getLength());
  
            MessageBytes value = source.getValue(i);
            env.put(
                    newStringFromMessageBytes(mb),
                    newStringFromMessageBytes(value));
        }
    }

    private String newStringFromMessageBytes(MessageBytes messageBytes) {
        if (messageBytes == null) {
            return new String("");
        } else {
            ByteChunk chunk = messageBytes.getByteChunk();
            byte[] bytes = chunk.getBuffer();
            if (bytes == null) {
                return new String("");
            } else {
                return new String(
                        bytes,
                        chunk.getStart(),
                        chunk.getLength());
            }
        }
    }

    public void destroy() {
        //this destroy does not mean much. The runtime here might not be available. For now
        //check if runtime is non-null then destroy
        //if(runtime != null)
            //JavaEmbedUtils.terminate(runtime);
    }

    public RT getRuntime() {
        return new RT();
    }
}

