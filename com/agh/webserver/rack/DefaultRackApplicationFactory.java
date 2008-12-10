package com.agh.webserver.rack;

/**
 *
 * @author Antonio Garrote Hernandez
 */
import clojure.lang.AMapEntry;
import clojure.lang.Keyword;
import clojure.lang.LazyCons;
import clojure.lang.MapEntry;
import clojure.lang.PersistentHashMap;
import clojure.lang.PersistentStructMap;
import clojure.lang.RT;
import clojure.lang.Var;
import com.agh.webserver.ClojureAdapter;



import com.sun.grizzly.tcp.Response;
import com.sun.grizzly.tcp.http11.GrizzlyOutputBuffer;
import com.sun.grizzly.tcp.http11.GrizzlyResponse;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Vivek Pandey
 */
public class DefaultRackApplicationFactory implements RackApplicationFactory{

    private static final String CLOJURE_RACK = "com/agh/webserver/rack/rack.clj";

    private RackApplication errorApplication;
    protected final Logger logger;
    final String appRoot;
    private final ClojureAdapter adapter;


    public DefaultRackApplicationFactory(ClojureAdapter adapter) {
        this.logger = adapter.getLogger();
        this.appRoot = adapter.getAppRoot();
        this.adapter = adapter;
    }

    public RackApplication newApplication(final RT runtime) throws RackInitializationException {
        return new DefaultRackApplication(adapter) {
            public RT runtime() {
                return runtime;
            }

            @Override
            public RackResponse callMethod(String cmd, final HashMap rackEnv) {

                RackResponse resp = null;

                String[] path = ((String) rackEnv.get("PATH_TRANSLATED")).split("/");
                path = Arrays.copyOfRange(path,1,path.length);
                String[] subpath = Arrays.copyOfRange(path, 0, (path.length-1));
                String filePath = adapter.getAppRoot();
                boolean isFirst = true;
                String suffix = "";
                for(String file : subpath) {
                    if(isFirst) {
                        filePath += file;
                        suffix += file;
                        isFirst = false;
                    } else {
                        filePath += File.separator + file;
                        suffix += File.separator + file;
                    }
                }

                File script = new File(filePath+".clj");

                if(script.exists()) {
                    try {
                        Properties props = System.getProperties();
                        RT.loadResourceScript(suffix+".clj");
                        RT.loadResourceScript(CLOJURE_RACK);
                        //Object[] args = new Object[2];
                        //args[0] = rackEnv;
                        //args[1] = path[path.length-1];
                        //Var form = RT.var("com.agh.webserver.rack", "with-rack-response");
                        Var form = RT.var("com.agh.webserver.rack", "rack-invokation-point");
                        //PersistentStructMap result = (PersistentStructMap) form.invoke(rackEnv, "hello-complex");
                        PersistentStructMap result = (PersistentStructMap) form.invoke(rackEnv,path[path.length-1]);

                        Integer theStatus = null;
                        String theBody = null;
                        HashMap theHeaders = null;

                        Object[] results = result.toArray();
                        for(Object entry : results) {
                            MapEntry e = (MapEntry) entry;
                            Keyword key = (Keyword) e.getKey();
                            Object val = e.getValue();
                            String keyString = key.getName();
                            System.out.println(keyString);
                            if(keyString == "status") {
                                theStatus = (Integer) val;
                            }else if(keyString == "headers") {
                                PersistentHashMap headersStruct = (PersistentHashMap) val;
                                Object[] everyHeader = headersStruct.toArray();
                                Iterator it = headersStruct.iterator();
                                while(it.hasNext()) {
                                    AMapEntry he = (AMapEntry) it.next();
                                    System.out.println(".");
                                }
                                HashMap convertedHeaders = new HashMap();
                                for(Object h : everyHeader) {
                                    AMapEntry he = (AMapEntry) h;
                                    convertedHeaders.put(((Keyword)he.getKey()).getName(), he.getValue());
                                }
                                theHeaders = convertedHeaders;
                            } else if(keyString == "body") {
                                theBody = (String) val;
                            }
                        }

                        final Integer theFinalInteger = theStatus;
                        final String theFinalBody = theBody;
                        final HashMap theFinalHeaders = theHeaders;

                        System.out.println("hola");
                        resp = new RackResponse() {

                            public int getStatus() {
                                return theFinalInteger.intValue();
                            }

                            public Map getHeaders() {
                                return theFinalHeaders;
                            }

                            public String getBody() {
                                String txt = ((String) rackEnv.get("clojure.output.stream"));
                                System.out.println("BODY->"+txt);
                                return theFinalBody;
                            }

                            public void respond(GrizzlyResponse response) {
                                response.setStatus(getStatus());
                                Map headers = getHeaders();
                                Set keys = headers.keySet();
                                for(Object k : keys) {
                                    response.setHeader((String)k, (String)headers.get(k));
                                }
                                try {
                                    response.getWriter().print(getBody());
                                } catch (IOException ex) {
                                    logger.severe("Unable to write response to Grizzly response");
                                }
                            }
                        };
                    } catch (final Exception e) {
                        resp = new RackResponse() {

                            public int getStatus() {
                                return 500;
                            }

                            public Map getHeaders() {
                                return new HashMap();
                            }

                            public String getBody() {
                                return new String("<html><body><h1>500/Internal Server Error</h1><br/><p>"+e.getMessage()+"</p></body><html>");
                            }

                            public void respond(GrizzlyResponse response) {
                                response.setStatus(getStatus());
                                try {
                                    response.getWriter().print(getBody());
                                } catch (IOException ex) {
                                    logger.severe("Unable to write response to Grizzly response");
                                }
                            }
                        };
                    }                    

                } else {

                    resp = new RackResponse() {

                        public int getStatus() {
                            return 500;
                        }

                        public Map getHeaders() {
                            return new HashMap();
                        }

                        public String getBody() {
                            return new String("<html><body><h1>500/Internal Server Error</h1><br/><p>Requested script not found</p></body></html>");
                        }

                        public void respond(GrizzlyResponse response) {
                            response.setStatus(getStatus());
                            try {
                                response.getWriter().print(getBody());
                            } catch (IOException ex) {
                                logger.severe("Unable to write response to Grizzly response");
                            }
                        }
                    };
                }

                return resp;
            }
        };
    }

    public void finishedWithApplication(RackApplication app) {
        app.destroy();
    }

    public RackApplication getErrorApplication(final RT runtime) {
        if(errorApplication == null){
            return new DefaultRackApplication(adapter) {
                public RT runtime() {
                    return runtime;
                }

                @Override
                public RackResponse callMethod(String cmd, HashMap rackEnv) {
                    throw new UnsupportedOperationException("Not supported yet.");
                }
            };
        }
        return errorApplication;
    }

    public void destroy() {
        if(errorApplication != null)
            errorApplication.destroy();
        errorApplication = null;
    }


    
}
