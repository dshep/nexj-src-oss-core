// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.tools;

import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Locale;
import java.util.Properties;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Queue;
import javax.jms.QueueBrowser;
import javax.jms.Session;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NameNotFoundException;
import javax.security.auth.Subject;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.login.LoginContext;

import nexj.core.meta.Repository;
import nexj.core.meta.integration.channel.jms.MessageQueue;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.jms.JMSReceiver;
import nexj.core.rpc.jms.JMSSender;
import nexj.core.rpc.xml.XMLMarshaller;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.IOUtil;
import nexj.core.util.Logger;
import nexj.core.util.StringUtil;
import nexj.core.util.XMLUtil;

/**
 * Tool for JMS queue message extraction.
 */
public class JMSBrowserTool extends GenericTool
{
   // attributes

   /**
    * Boolean to indicate whether the message headers are to be printed
    */
   protected boolean m_bPrintHeaders;

   /**
    * True to outpur the messages in XML format
    */
   protected boolean m_bOutputXML;

   // associations

   /**
    * A variable that will contain server-specific environment settings (initial
    * context, etc.)
    */
   protected PlatformJMSClientStrategy m_platformStrategy;

   /**
    * The connection object
    */
   protected Connection m_connection;

   /**
    * The session object
    */
   protected Session m_session;

   /**
    * The queue browser object - a special object that allows to retrieve the
    * messages from the queue without permanently removing them
    */
   protected QueueBrowser m_qBrowser;

   /**
    * The print writer object
    */
   protected PrintWriter m_writer;

   /**
    * The file object to represent the file where the messages will be dumped
    */
   protected File m_file;

   /**
    * The queue object to represent the queue from which the messages will be
    * read
    */
   protected Queue m_queue;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JMSBrowserTool.class);

   // operations

   /**
    * @see nexj.core.tools.GenericTool#begin()
    */
   protected void begin() throws Exception
   {
      String sServerType = getRequiredProperty("type");

      if ("JBoss".equals(sServerType))
      {
         m_platformStrategy = new JBossJMSClientStrategy();
      }
      else if ("WebSphere".equals(sServerType))
      {
         m_platformStrategy = new WASJMSClientStrategy();
      }
      else
      {
         throw new RuntimeException("Unsupported server type: " + sServerType);
      }

      m_bPrintHeaders = StringUtil.parseBoolean(getProperty("headers", "true"));
      m_bOutputXML = StringUtil.parseBoolean(getProperty("XML", "true"));

      InitialContext ctx = m_platformStrategy.getInitialContext();
      String sFileName = getProperty("directory");

      m_file = new File(sFileName);

      if (!m_file.exists())
      {
         // The directory does not exist.
         new File(sFileName).mkdirs();
      }

      SimpleDateFormat formatter = new SimpleDateFormat("-yyyy-MM-dd-hh-mm-ss", Locale.ENGLISH);
      String sQueueName = getRequiredProperty("queue");

      m_file = new File(sFileName, m_platformStrategy.getSimpleQueueName(sQueueName) +
         formatter.format(new Date()) + ".txt");
      m_writer = new PrintWriter(IOUtil.openBufferedWriter(m_file, IOUtil.ENCODING));

      ConnectionFactory connFactory = null;

      try
      {
         connFactory = (ConnectionFactory)ctx.lookup(m_platformStrategy.getFullJNDICFName(getRequiredProperty("factory")));
      }
      catch (NameNotFoundException e)
      {
         s_logger.info(e);
      }

      m_connection = connFactory.createConnection(getProperty("user", "nexjsa"), getProperty("password", "nexj"));
      m_session = m_connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

      m_connection.start();

      try
      {
         m_queue = (Queue)ctx.lookup(m_platformStrategy.getFullJNDIQueueName(sQueueName));
      }
      catch (NameNotFoundException e)
      {
         s_logger.info(e);
      }

      m_qBrowser = m_session.createBrowser(m_queue);
   }

   /**
    * @see nexj.core.tools.GenericTool#dispose()
    */
   protected void dispose()
   {
      if (m_qBrowser != null)
      {
         try
         {
            m_qBrowser.close();
         }
         catch (JMSException e)
         {
            s_logger.error("Cannot close queue browser", e);
         }
      }

      if (m_session != null)
      {
         try
         {
            m_session.close();
         }
         catch (JMSException e)
         {
            s_logger.error("Cannot close session", e);
         }
      }

      if (m_connection != null)
      {
         try
         {
            m_connection.close();
         }
         catch (JMSException e)
         {
            s_logger.error("Cannot close connection", e);
         }
      }

      if (m_writer != null)
      {
         m_writer.close();
      }
   }

   /**
    * @see nexj.core.tools.GenericTool#execute(java.lang.String)
    */
   protected void execute(String sCommand) throws Exception
   {
      int nMessageCount = 0;
      Enumeration msgsEnum = m_qBrowser.getEnumeration();

      JMSReceiver jmsReceiver = new JMSReceiver();
      jmsReceiver.setChannel(new MessageQueue(m_queue.getQueueName()));

      XMLMarshaller xmlMarshaller = new XMLMarshaller(new InvocationContext(Repository.getMetadata()));

      if (!msgsEnum.hasMoreElements())
      {
         m_writer.println("No messages in queue ");
         m_writer.println(m_queue.getQueueName());
      }
      else
      {
         m_writer.print("Below is the list of the messages in the queue ");
         m_writer.println(m_queue.getQueueName());
         m_writer.println();

         while (msgsEnum.hasMoreElements())
         {
            m_writer.print("Message #");
            m_writer.println(++nMessageCount);

            Message tempMsg = (Message)msgsEnum.nextElement();
            TransferObject tobj = jmsReceiver.createMessage(m_platformStrategy.getMessage(tempMsg));
            Object obj = tobj;

            if (!m_bPrintHeaders)
            {
               obj = tobj.findValue(JMSSender.BODY);
            }

            if (m_bOutputXML)
            {
               StringWriter writer = new StringWriter(1024);

               xmlMarshaller.serialize(obj, writer);
               m_writer.write(XMLUtil.formatXML(writer.toString()));
            }
            else
            {
               m_writer.println(String.valueOf(obj));
            }

            m_writer.println();
         }
      }

      if (s_logger.isInfoEnabled())
      {
         s_logger.info("Dumping completed successfully");
      }
   }

   /**
    * @see nexj.core.tools.GenericTool#getOptionUsage()
    */
   protected String[] getOptionUsage()
   {
      return new String[]
      {
         "-Durl=<host>:<port>",
         "-Dfactory=<connection factory>",
         "-Dqueue=<queue>",
         "-Duser=<user>",
         "-Dpassword=<password>",
      };
   }

   /**
    * @see nexj.core.tools.GenericTool#getCommandUsage()
    */
   protected String[] getCommandUsage()
   {
      return null;
   }

   public static void main(String[] args)
   {
      new JMSBrowserTool().run(args);
   }

   // inner classes

   /**
    * General interface to describe server-specific environment settings
    * (initial context, etc.)
    */
   protected interface PlatformJMSClientStrategy
   {
      public InitialContext getInitialContext() throws Exception;

      public String getDefaultQueueConnectionFactoryName();

      public String getDefaultQueueJNDIName();

      public String getFullJNDIQueueName(String s);

      public String getSimpleQueueName(String s);

      public String getFullJNDICFName(String s);

      public String getSimpleCFName(String s);

      public String getURL();
      
      public Message getMessage (Message message);
   }

   /**
    * JMS test client strategy for JBOSS Application Server
    */
   protected class JBossJMSClientStrategy implements PlatformJMSClientStrategy
   {
      protected static final String DEFAULT_CONNECTION_FACTORY_NAME = "UIL2ConnectionFactory";

      protected static final String DEFAULT_QUEUE_JNDI_NAME = "queue/nexj/NEXJ_ERROR";

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getInitialContext()
       */
      public InitialContext getInitialContext() throws Exception
      {
         Properties env = new Properties();

         env.setProperty(Context.INITIAL_CONTEXT_FACTORY, "org.jnp.interfaces.NamingContextFactory");
         env.setProperty(Context.PROVIDER_URL, getURL());
         env.setProperty(Context.URL_PKG_PREFIXES, "org.jboss.naming:org.jnp.interfaces");

         return new InitialContext(env);
      }

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getDefaultQueueConnectionFactoryName()
       */
      public String getDefaultQueueConnectionFactoryName()
      {
         return DEFAULT_CONNECTION_FACTORY_NAME;
      }

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getDefaultQueueJNDIName()
       */
      public String getDefaultQueueJNDIName()
      {
         return DEFAULT_QUEUE_JNDI_NAME;
      }

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getFullJNDICFName(java.lang.String)
       */
      public String getFullJNDICFName(String sProvidedJNDIName)
      {
         if (sProvidedJNDIName.contains("/"))
         {
            return sProvidedJNDIName;
         }

         return sProvidedJNDIName;
      }

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getFullJNDIQueueName(java.lang.String)
       */
      public String getFullJNDIQueueName(String sProvidedJNDIName)
      {
         if (sProvidedJNDIName.contains("/"))
         {
            return sProvidedJNDIName;
         }

         return "queue/nexj/" + sProvidedJNDIName;
      }

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getSimpleCFName(java.lang.String)
       */
      public String getSimpleCFName(String sProvidedJNDIName)
      {
         if (!sProvidedJNDIName.contains("/"))
         {
            return sProvidedJNDIName;
         }

         return sProvidedJNDIName.substring(sProvidedJNDIName.lastIndexOf("/") + 1, sProvidedJNDIName.length());
      }

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getSimpleQueueName(java.lang.String)
       */
      public String getSimpleQueueName(String sProvidedJNDIName)
      {
         if (!sProvidedJNDIName.contains("/"))
         {
            return sProvidedJNDIName;
         }

         return sProvidedJNDIName.substring(sProvidedJNDIName.lastIndexOf("/") + 1, sProvidedJNDIName.length());
      }

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getURL()
       */
      public String getURL()
      {
         String sURL = getProperty("url", "localhost");

         if (sURL.indexOf(':') < 0)
         {
            sURL += ":1099";
         }

         return sURL;
      }

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getMessage(javax.jms.Message)
       */
      public Message getMessage(Message message)
      {
         try
         {
            Class cls = Class.forName("org.jboss.jms.message.JBossMessage");

            if (cls.isInstance(message))
            {
               return (Message)Class.forName("org.jboss.jms.message.MessageProxy").getConstructor(new Class[]{cls}).newInstance(new Object[]{message});
            }            
         }
         catch (Throwable t)
         {
            s_logger.debug(t);
         }

         return message;
      }
   }

   /**
    * JMS test client strategy for Websphere Application Server
    *
    * In order for this test client to work with WebSphere all of the following
    * steps must be performed:
    *
    * 1. Setup Provider Endpoints - using the websphere console navigate to
    * Resources --> JMS --> Queue connection factories - select the connection
    * factory of the queue you will be connecting to - in the Provider endpoints
    * text box enter localhost:<port>:BootstrapSecureMessaging where <port> is
    * the SIB_ENDPOINT_SECURE_ADDRESS of the application server (in the cluster
    * that hosts the SI bus) - apply the changes and synchronize all nodes if it
    * is not done automatically
    *
    * *** IMPORTANT: Websphere has a bug where it creates the Provider Enpoints
    * property in the config files using the wrong case. This must be manually
    * fixed as follows: - find the resources.xml file for each profile
    * (deployment manager and server) located in:
    * <WAS_ROOT>\profiles\<profile>\config
    * \cells\<cell>\clusters\<cluster-containing-conn-factory> - find the
    * J2CConnectionFactory of the queue connection factory in question - find
    * the J2EEResourceProperty with the name ProviderEndPoints for the
    * connection factory in question - change the name to ProviderEndpoints
    * (lowercase 'p' in Endpoints) - restart the server(s)
    *
    * 2. Setup the runtime JVM arguments in the launcher - the following JVM
    * properties must be used when running this client:
    * -Dcom.ibm.CORBA.ConfigURL
    * =file:<WAS_ROOT>/../AppClient/properties/sas.client.props
    * -Djava.security.auth
    * .login.config=<WAS_ROOT>/../AppClient/properties/wsjaas_client.conf
    * -Dcom.ibm
    * .SSL.ConfigURL=file:<WAS_ROOT>/../AppClient/properties/ssl.client.props
    *
    * 3. Setup the runtime classpath in the launcher - IBM JRE must be used
    * instead of Sun's - if there are any websphere jars on the build classpath,
    * they must be of the same fix pack level as the runtime jars - the
    * following jars must be in the runtime classpath (versions may differ)
    * <WAS_ROOT>/..AppClient/plugins/com.ibm.ws_emf_2.1.0.jar
    * <WAS_ROOT>/..AppClient/plugins/com.ibm.ws.runtime_6.1.0.jar
    * <WAS_ROOT>/..AppClient/plugins/com.ibm.ws.sib.client_2.0.0.jar
    * <WAS_ROOT>/..AppClient/plugins/com.ibm.ws.sib.utils_2.0.0.jar
    *
    * *** IMPORTANT: the jars must be listed in the classpath before/above the
    * core project
    *
    * 4. Use the correct JNDI format - if passing the JNDI names of the
    * connection factory and/or queue, they must be in the following format:
    * "cell/clusters/<cluster-containing-resource>/<jndi-name>" If you don't
    * have clusters, use the JNDI names from Websphere Console --> Resources -->
    * JMS
    */
   protected class WASJMSClientStrategy implements PlatformJMSClientStrategy
   {
      protected static final String DEFAULT_CONNECTION_FACTORY_NAME = "";

      protected static final String DEFAULT_QUEUE_JNDI_NAME = "";

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getInitialContext()
       */
      public InitialContext getInitialContext() throws Exception
      {
         Hashtable env = new Hashtable();

         env.put(Context.INITIAL_CONTEXT_FACTORY, "com.ibm.websphere.naming.WsnInitialContextFactory");
         env.put(Context.PROVIDER_URL, "corbaloc:iiop:" + getURL());

         InitialContext ctx = new InitialContext(env);

         ctx.lookup("");

         CallbackHandler cbHandler = (CallbackHandler)(Class.forName(
            "com.ibm.websphere.security.auth.callback.WSCallbackHandlerImpl").getConstructor(new Class[]
         {
            String.class,
            String.class
         }).newInstance(new Object[]
         {
            getProperty("user", "nexjsa"),
            getProperty("password", "nexj")
         }));

         LoginContext lctx = new LoginContext("WSLogin", cbHandler);
         lctx.login();

         Class.forName("com.ibm.websphere.security.auth.WSSubject").getMethod("setRunAsSubject", new Class[]
         {
            Subject.class
         }).invoke(null, new Object[]
         {
            lctx.getSubject()
         });

         return ctx;
      }

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getDefaultQueueConnectionFactoryName()
       */
      public String getDefaultQueueConnectionFactoryName()
      {
         return DEFAULT_CONNECTION_FACTORY_NAME;
      }

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getDefaultQueueJNDIName()
       */
      public String getDefaultQueueJNDIName()
      {
         return DEFAULT_QUEUE_JNDI_NAME;
      }

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getFullJNDICFName(java.lang.String)
       */
      public String getFullJNDICFName(String sProvidedJNDIName)
      {
         if (sProvidedJNDIName.contains("/"))
         {
            return sProvidedJNDIName;
         }

         return "nexj/jms/cf/" + sProvidedJNDIName;
      }

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getFullJNDIQueueName(java.lang.String)
       */
      public String getFullJNDIQueueName(String sProvidedJNDIName)
      {
         if (sProvidedJNDIName.contains("/"))
         {
            return sProvidedJNDIName;
         }

         return "nexj/jms/queue/" + sProvidedJNDIName;
      }

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getSimpleCFName(java.lang.String)
       */
      public String getSimpleCFName(String sProvidedJNDIName)
      {
         if (!sProvidedJNDIName.contains("/"))
         {
            return sProvidedJNDIName;
         }

         return sProvidedJNDIName.substring(sProvidedJNDIName.lastIndexOf('/') + 1, sProvidedJNDIName.length());
      }

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getSimpleQueueName(java.lang.String)
       */
      public String getSimpleQueueName(String sProvidedJNDIName)
      {
         if (!sProvidedJNDIName.contains("/"))
         {
            return sProvidedJNDIName;
         }

         return sProvidedJNDIName.substring(sProvidedJNDIName.lastIndexOf('/') + 1, sProvidedJNDIName.length());
      }

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getURL()
       */
      public String getURL()
      {
         String sURL = getProperty("url", "localhost");

         if (sURL.indexOf(':') < 0)
         {
            sURL += ":2809";
         }

         return sURL;
      }

      /**
       * @see nexj.core.tools.JMSBrowserTool.PlatformJMSClientStrategy#getMessage(javax.jms.Message)
       */
      public Message getMessage(Message message)
      {
         return message;
      }
   }
}
