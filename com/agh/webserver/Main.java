package com.agh.webserver;
        
import clojure.lang.RT;
import com.sun.grizzly.tcp.StaticResourcesAdapter;
import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.Parser;
import org.apache.commons.cli.PosixParser;
/*
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.sail.rdbms.RdbmsStore;
import org.slf4j.LoggerFactory;
*/

public class Main {

    private static final String MAINCLJ = "com/agh/webserver/webserver.clj";
    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {

/*
        SailRepository srp = new SailRepository(new RdbmsStore("com.mysql.jdbc.Driver",
                "jdbc:mysql://localhost:3306/clojure_sesame",
                "root",
                "root"));
        try {
            srp.initialize();
        } catch (RepositoryException ex) {
            Logger.getLogger(Main.class.getName()).log(Level.SEVERE, null, ex);
        }
 */
/*
        try {
            RT.loadResourceScript(MAINCLJ);
            Object result = RT.var("com.agh.webserver", "main").invoke(args);
            System.out.println("Fin");
        } catch(Exception e) {
            e.printStackTrace();
        }
*/
        try {
            CommandLine cmd = commandLineOptions(args);
            
            ClojureRTWrapper.loadFramework();
            
            ClojureSelectorThread selectorThread = new ClojureSelectorThread();
            selectorThread.setClojurePublicPath(serverRoot(cmd));
            selectorThread.setClojureRoot(new File(".").getCanonicalPath());
            selectorThread.setPort(serverPort(cmd));
            selectorThread.setAdapter(new ClojureAdapter("/", 3, 1, 5, true, new ClojureRuntimeAsyncFilter()));
            selectorThread.initEndpoint();
            selectorThread.startEndpoint();
            
        } catch (IOException ex) {
            System.out.println(ex.getMessage());
        } catch (InstantiationException ex) {
            System.out.println(ex.getMessage());
        } catch (ParseException ex) {
            System.out.println(ex.getMessage());
        } catch (Exception ex) {
            System.out.println("ABORTING!!!");
        }

    }


    private static final CommandLine commandLineOptions(String[] args) throws ParseException {
        Options opts = new Options();
        opts.addOption("p", false, "server port");
        opts.addOption("P", "public", false, "public path");

        return new PosixParser().parse(opts, args);
    }

    private static int serverPort(CommandLine cmd) {
        if(cmd.hasOption("p")) {
            return Integer.parseInt(cmd.getOptionValue("p"));
        } else {
            return 8880;
        }
    }

    private static String serverRoot(CommandLine cmd) throws IOException {
        if(cmd.hasOption("P")) {
            return cmd.getOptionValue("p");
        } else {
            File current = new File("./public");
            return current.getCanonicalPath();
        }
    }
} 
