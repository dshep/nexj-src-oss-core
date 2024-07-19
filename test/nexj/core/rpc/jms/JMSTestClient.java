// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.Properties;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.Queue;
import javax.jms.QueueConnection;
import javax.jms.QueueConnectionFactory;
import javax.jms.QueueReceiver;
import javax.jms.QueueSender;
import javax.jms.QueueSession;
import javax.jms.TextMessage;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.security.auth.Subject;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.login.LoginContext;

import nexj.core.tools.GenericTool;
import nexj.core.util.IOUtil;
import nexj.core.util.Logger;

/**
 * JMS test client
 */
public class JMSTestClient extends GenericTool
{
   // associations
   
   protected PlatformJMSClientStrategy m_platformStrategy;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JMSTestClient.class);

   private Socket m_clientSocket = null;
   
   // constructors
   
   public JMSTestClient()
   {
      String serverType = getRequiredProperty("type");
      
      if ("JBoss".equals(serverType))
      {
         m_platformStrategy = new JBossJMSClientStrategy();
      }
      else if ("WebSphere".equals(serverType))
      {
         m_platformStrategy = new WASJMSClientStrategy();
      }
      else
      {
         throw new RuntimeException("Unsupported server type: " + serverType); 
      }
   }
   
   // operations

   /**
    * @see nexj.core.tools.GenericTool#execute(java.lang.String)
    */
   protected void execute(String sCommand) throws Exception
   {
      InitialContext ctx = m_platformStrategy.getInitialContext();

      QueueConnection qcon = null;
      QueueSession qsession = null;

      try
      {
         QueueConnectionFactory qcf = (QueueConnectionFactory) ctx.lookup(getProperty("factory", m_platformStrategy.getDefaultQueueConnectionFactoryName()));
         qcon = qcf.createQueueConnection(getProperty("user", "nexjsa"), getProperty("password", "nexj"));
         qsession = qcon.createQueueSession(false, QueueSession.AUTO_ACKNOWLEDGE);
         qcon.start();

         Queue queue = (Queue)ctx.lookup(getProperty("queue", m_platformStrategy.getDefaultQueueJNDIName()));
         QueueSender snd = qsession.createSender(queue);

         processCommand(sCommand, ctx, qsession, snd);
      }
      finally
      {
         if (qcon != null)
         {
            if (qsession != null)
            {
               qsession.close();
            }

            qcon.close();
         }
      }
   }

   protected void processCommand(String sCommand, InitialContext initialContext, QueueSession queueSession,
      QueueSender queueSender) throws JMSException, IOException, NamingException, UnsupportedEncodingException
   {
      if ("send".equals(sCommand))
      {
         String sMsgDir = getProperty("msg.dir");
         int nReqCount = Integer.parseInt(getProperty("req.count", "1"));

         if (sMsgDir == null)
         {
            TextMessage msg = queueSession.createTextMessage("<hello>Hello, world!</hello>");
            //msg.setLongProperty("jmsDeliveryTime", System.currentTimeMillis() + 5000);
            //msg.setLongProperty("JMS_JBOSS_SCHEDULED_DELIVERY", System.currentTimeMillis() + 10000);
            //msg.setLongProperty("JMS_JBOSS_REDELIVERY_LIMIT", 5);
            //msg.setLongProperty("JMS_JBOSS_REDELIVERY_COUNT", 2);
            //msg.setLongProperty("JMS_JBOSS_REDELIVERY_DELAY", 5000);

            //msg.setLongProperty(JMS.BACKOFF_DELAY, 500);
            //msg.setLongProperty(JMS.MAX_BACKOFF_DELAY, 1500);
            //msg.setIntProperty(JMS.MAX_ERROR_COUNT, 1);

            for (int nReq = 0; nReq < nReqCount; ++nReq)
            {
               queueSender.send(msg);
            }
         }
         else
         {
            File[] fileArray = new File(sMsgDir).listFiles();

            if (fileArray == null)
            {
               throw new IOException("Invalid message directory \"" + sMsgDir + "\"");
            }

            Arrays.sort(fileArray);

            for (int nReq = 0; nReq < nReqCount; ++nReq)
            {
               for (int i = 0; i < fileArray.length; ++i)
               {
                  File file = fileArray[i];
                  StringBuffer buf = new StringBuffer((int)file.length());
                  Reader reader = null;
  
                  try
                  {
                     reader = new InputStreamReader(new BufferedInputStream(new FileInputStream(file)), IOUtil.ENCODING);
  
                     int ch;
  
                     while ((ch = reader.read()) != -1)
                     {
                        buf.append((char) ch);
                     }
                  }
                  catch (IOException e)
                  {
                     s_logger.error("I/O error", e);
                  }
                  finally
                  {
                     IOUtil.close(reader);
                  }
  
                  TextMessage msg = queueSession.createTextMessage(buf.toString());
  
                  //msg.setLongProperty(JMSSender.BACKOFF_DELAY, 5000);
                  //msg.setLongProperty(JMSSender.MAX_BACKOFF_DELAY, 15000);
                  //msg.setIntProperty(JMSSender.MAX_ERROR_COUNT, 5);
  
                  queueSender.send(msg);
               }
            }

            nReqCount *= fileArray.length;
         }

         if (s_logger.isInfoEnabled())
         {
            s_logger.info("Sent " + nReqCount + " message(s)");
         }
      }
      else if ("tcp".equals(sCommand))
      {
         String sTCPPort = getProperty("tcp.port", "8889");
         int port = Integer.parseInt(sTCPPort);
         
         ServerSocket serverSocket = null;

         Queue rcvQueue = (Queue)initialContext.lookup(getProperty("queue", "queue/HL7_OUTPUT"));
         QueueReceiver rcv = queueSession.createReceiver(rcvQueue);
         rcv.setMessageListener(new MessageListener()
         {
            public void onMessage(Message msg)
            {
               TextMessage tm = (TextMessage) msg;
               
               try
               {
                  if (null != m_clientSocket)
                  {
                     s_logger.debug("Sending HL7 Reply");

                     BufferedWriter wr = new BufferedWriter(
                        new OutputStreamWriter(
                           m_clientSocket.getOutputStream(),
                           IOUtil.ENCODING));
                     
                     new MLLP().writeMessage(wr, tm.getText());
                     wr.flush();
                  }

                  tm.getText();
                  
               }
               catch(JMSException e)
               {

               }
               catch(IOException ioe)
               {
               }
            }
         });

         try
         {
             serverSocket = new ServerSocket(port);
         }
         catch (IOException e)
         {
             s_logger.error("Could not listen on port: " + port, e);
             System.exit(1);
         }
         
         try
         {
             m_clientSocket = serverSocket.accept();
         }
         catch (IOException e)
         {
            s_logger.error("Accept failed", e);
            System.exit(1);
         }

         BufferedReader in = new BufferedReader(
            new InputStreamReader(m_clientSocket.getInputStream(), IOUtil.ENCODING));
         
         boolean bEOF = false;

         while(!bEOF)
         {
            String sMessage = new MLLP().readMessage(in);
            
            if (null == sMessage)
            {
               bEOF = true;
            }
            else
            {
               s_logger.debug("Received HL7 Message, sending to queue");
               s_logger.dump(sMessage);

               queueSender.send(queueSession.createTextMessage(sMessage));
            }
         }
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
         "-Dmsg.dir=<message file directory>",
         "-Dreq.count=<duplicate request count>",
         "-Dtcp.port=<HL7 TCP port>"
      };
   }

   /**
    * @see nexj.core.tools.GenericTool#getCommandUsage()
    */
   protected String[] getCommandUsage()
   {
      return new String[]
      {
         "send - send messages",
         "tcp - listen on a tcp port for HL7 messages",
      };
   }

   public static void main(String[] args)
   {
      new JMSTestClient().run(args);
   }

   // inner classes

   /**
    * Helper class for HL7's Minimal Lower Level Protocol (MLLP or LLP).
    * 
    * From the 2.3 implementation guide:
    * 
    * HL7 messages are enclosed by special characters to form a block. The format is as follows:
    * <SB>dddd<EB><CR>
    * <SB> = Start Block character (1 byte)
    *   ASCII <VT>, i.e., <0x0B>. This should not be confused with the ASCII characters
    *   SOH or STX.
    * dddd = Data (variable number of bytes)
    *   This is the HL7 data content of the block. The data can contain any displayable ASCII
    *   characters and the carriage return character, <CR>.
    * <EB> = End Block character (1 byte)
    *   ASCII <FS>, i.e., <0x1C>. This should not be confused with the ASCII characters
    *   ETX or EOT.
    * <CR> = Carriage Return (1 byte)
    *   The ASCII carriage return character, i.e., <0x0D>.
    */
   public static class MLLP
   {
      public final static Logger s_logger = Logger.getLogger(MLLP.class);
      
      public final static int START_BLOCK = '\u000b';
      public final static int END_BLOCK = '\u001c';
      public final static int CR = '\r';
      public final static int DATA_BLOCK = 'D';
      public final static int NAK_BLOCK = 'N';
      
      public String readMessage(Reader in) throws IOException
      {
         StringBuffer buf = new StringBuffer();
         int input = in.read();

         if (input != START_BLOCK)
         {
            if (input < 0 )
            {
               //end of file.  just return null;
               return null;
            }
            else
            {
               //invalid hl7.  should throw something??
               logErrorChar("First byte must be START_BLOCK, recieved: ",input);
               return null;
            }
         }
         
         //data block:
         input = in.read();
         
         while (input != END_BLOCK)
         {
            if (input < 0 )
            {
               //unexpected EOF
               return null;
            }

            buf.append((char)input);
            input = in.read();
         }

         input = in.read();

         if (input != CR)
         {
            //expected CR;
            logErrorChar("CR expected after end block.  Received: ",input);
            return null;
         }
         
         return buf.toString();
      }
      
      public void writeMessage(Writer out,String msg) throws IOException
      {
         s_logger.debug("writing message");
         s_logger.debug(msg);
         out.write(START_BLOCK);
         out.write(msg);
         out.write(END_BLOCK);
         out.write(CR);
      }

      private void logErrorChar(String msg, int ch)
      {
         s_logger.error(msg + Integer.toHexString(ch) + "(" + (char)ch + ")");
      }
   }
   
   protected interface PlatformJMSClientStrategy
   {
      public InitialContext getInitialContext() throws Exception;
      
      public String getDefaultQueueConnectionFactoryName();
      
      public String getDefaultQueueJNDIName();
   }
   
   protected class JBossJMSClientStrategy implements PlatformJMSClientStrategy
   {
      protected static final String DEFAULT_CONNECTION_FACTORY_NAME = "UIL2ConnectionFactory";
      //protected static final String DEFAULT_CONNECTION_FACTORY_NAME = "XAConnectionFactory";
      protected static final String DEFAULT_QUEUE_JNDI_NAME = "queue/nexj/NEXJ_INPUT";

      /**
       * @see nexj.core.rpc.jms.JMSTestClient.PlatformJMSClientStrategy#getInitialContext()
       */
      public InitialContext getInitialContext() throws Exception
      {
         Properties env = new Properties();

         env.setProperty(Context.INITIAL_CONTEXT_FACTORY, "org.jnp.interfaces.NamingContextFactory");
         env.setProperty(Context.PROVIDER_URL, getProperty("url", "localhost:1099"));
         env.setProperty(Context.URL_PKG_PREFIXES, "org.jboss.naming:org.jnp.interfaces");

         return new InitialContext(env);
      }

      /**
       * @see nexj.core.rpc.jms.JMSTestClient.PlatformJMSClientStrategy#getDefaultQueueConnectionFactoryName()
       */
      public String getDefaultQueueConnectionFactoryName()
      {
         return DEFAULT_CONNECTION_FACTORY_NAME;
      }

      /**
       * @see nexj.core.rpc.jms.JMSTestClient.PlatformJMSClientStrategy#getDefaultQueueJNDIName()
       */
      public String getDefaultQueueJNDIName()
      {
         return DEFAULT_QUEUE_JNDI_NAME;
      }
   }
   
   /**
    * JMS test client strategy for Websphere Application Server
    * 
    * In order for this test client to work with WebSphere all of the following steps must be performed:
    * 
    * 1. Setup Provider Endpoints  
    *  - using the websphere console navigate to Resources --> JMS --> Queue connection factories
    *  - select the connection factory of the queue you will be connecting to
    *  - in the Provider endpoints text box enter localhost:<port>:BootstrapSecureMessaging
    *    where <port> is the SIB_ENDPOINT_SECURE_ADDRESS of the application server (in the cluster that hosts the SI bus)
    *  - apply the changes and synchronize all nodes if it is not done automatically
    * 
    *    *** IMPORTANT: Websphere has a bug where it creates the Provider Enpoints property in the 
    *                   config files using the wrong case. This must be manually fixed as follows:
    *                   - find the resources.xml file for each profile (deployment manager and server) located in:
    *                     <WAS_ROOT>\profiles\<profile>\config\cells\<cell>\clusters\<cluster-containing-conn-factory>
    *                   - find the J2CConnectionFactory of the queue connection factory in question
    *                   - find the J2EEResourceProperty with the name ProviderEndPoints for the connection factory in question
    *                   - change the name to ProviderEndpoints (lowercase 'p' in Endpoints)
    *                   - restart the server(s)
    *                 
    * 2. Setup the runtime JVM arguments in the launcher 
    *  - the following JVM properties must be used when running this client:
    *    -Dcom.ibm.CORBA.ConfigURL=file:<WAS_ROOT>/../AppClient/properties/sas.client.props
    *    -Djava.security.auth.login.config=<WAS_ROOT>/../AppClient/properties/wsjaas_client.conf
    *    -Dcom.ibm.SSL.ConfigURL=file:<WAS_ROOT>/../AppClient/properties/ssl.client.props
    *   
    * 3. Setup the runtime classpath in the launcher
    *  - IBM JRE must be used instead of Sun's
    *  - if there are any websphere jars on the build classpath, they must be of the same fix pack level as the runtime jars
    *  - the following jars must be in the runtime classpath (versions may differ)
    *    <WAS_ROOT>/..AppClient/plugins/com.ibm.ws_emf_2.1.0.jar
    *    <WAS_ROOT>/..AppClient/plugins/com.ibm.ws.runtime_6.1.0.jar
    *    <WAS_ROOT>/..AppClient/plugins/com.ibm.ws.sib.client_2.0.0.jar
    *    <WAS_ROOT>/..AppClient/plugins/com.ibm.ws.sib.utils_2.0.0.jar
    *    
    *    *** IMPORTANT: the jars must be listed in the classpath before/above the core project
    *  
    *  4. Use the correct JNDI format 
    *   - if passing the JNDI names of the connection factory and/or queue, they must be in the following format:
    *     "cell/clusters/<cluster-containing-resource>/<jndi-name>"
    */
   protected class WASJMSClientStrategy implements PlatformJMSClientStrategy
   {
      protected static final String DEFAULT_CONNECTION_FACTORY_NAME = "cell/clusters/nexj/nexj/jms/cf/NEXJ_INPUT";
      protected static final String DEFAULT_QUEUE_JNDI_NAME = "cell/clusters/nexj/nexj/jms/queue/NEXJ_INPUT";
      
      /**
       * @see nexj.core.rpc.jms.JMSTestClient.PlatformJMSClientStrategy#getInitialContext()
       */
      public InitialContext getInitialContext() throws Exception
      {
         Hashtable env = new Hashtable();
         env.put(Context.INITIAL_CONTEXT_FACTORY, "com.ibm.websphere.naming.WsnInitialContextFactory");
         env.put(Context.PROVIDER_URL, "corbaloc:iiop:" + getProperty("url", "localhost:2811"));
         
         InitialContext ctx = new InitialContext(env);
         ctx.lookup("");
         
         CallbackHandler cbHandler = (CallbackHandler)
         (Class.forName("com.ibm.websphere.security.auth.callback.WSCallbackHandlerImpl")
            .getConstructor(new Class[]{String.class, String.class})
            .newInstance(new Object[]{getProperty("user", "nexjsa"), getProperty("password", "nexj")}));
         
         LoginContext lctx = new LoginContext("WSLogin", cbHandler);
         lctx.login();

         Class.forName("com.ibm.websphere.security.auth.WSSubject")
            .getMethod("setRunAsSubject", new Class[]{Subject.class})
            .invoke(null, new Object[]{lctx.getSubject()});
         
         return ctx;
      }

      /**
       * @see nexj.core.rpc.jms.JMSTestClient.PlatformJMSClientStrategy#getDefaultQueueConnectionFactoryName()
       */
      public String getDefaultQueueConnectionFactoryName()
      {
         return DEFAULT_CONNECTION_FACTORY_NAME;
      }

      /**
       * @see nexj.core.rpc.jms.JMSTestClient.PlatformJMSClientStrategy#getDefaultQueueJNDIName()
       */
      public String getDefaultQueueJNDIName()
      {
         return DEFAULT_QUEUE_JNDI_NAME;
      }
   }
}