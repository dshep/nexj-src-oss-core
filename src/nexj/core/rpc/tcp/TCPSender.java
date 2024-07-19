// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.Inet6Address;
import java.net.InetSocketAddress;
import java.security.AccessController;
import java.security.KeyStore;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.security.cert.Certificate;
import java.util.Collection;
import java.util.Iterator;
import java.util.Locale;

import javax.naming.InitialContext;
import javax.resource.ResourceException;
import javax.resource.spi.ManagedConnectionFactory;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageInputStream;
import nexj.core.integration.MessageOutputStream;
import nexj.core.integration.Responder;
import nexj.core.integration.Sender;
import nexj.core.integration.io.ObjectOutput;
import nexj.core.integration.io.ReaderInput;
import nexj.core.integration.io.StreamInput;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.channel.tcp.TCPChannel;
import nexj.core.monitoring.ThreadLocalCounter;
import nexj.core.monitoring.jmx.StatUtil;
import nexj.core.rpc.RPCException;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.tcp.ra.TCPConnectionRequestInfo;
import nexj.core.rpc.tcp.ra.TCPResourceAdapter;
import nexj.core.runtime.Initializable;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.util.Binary;
import nexj.core.util.IOUtil;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Logger;
import nexj.core.util.NetUtil;
import nexj.core.util.NoCloseInputStream;
import nexj.core.util.ObjUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.UTF8BOMIgnoreInputStream;
import nexj.core.util.XMLUtil;

/**
 * TCP Sender.
 */
public class TCPSender implements Sender, Responder, Initializable
{
   // constants

   /**
    * The message host property: String.
    */
   public final static String REMOTE_HOST = "host";

   /**
    * The message port property: int.
    */
   public final static String REMOTE_PORT = "port";

   /**
    * The message local host property: String.
    */
   public final static String LOCAL_HOST = "localHost";

   /**
    * The message local port property: int.
    */
   public final static String LOCAL_PORT = "localPort";

   /**
    * The client's key store for SSL client certificate authentication: java.security.KeyStore.
    * Can be null if not required.
    */
   public final static String CERTIFICATE = "certificate";

   /**
    * The password for the client's key store: String.
    */
   public final static String PASSWORD = "password";

   /**
    * The client's trusted certificate: java.security.cert.Certificate.
    * Can be null to trust certificates in the default trust store.
    */
   public final static String TRUST = "trust";

   /**
    * The client certificate chain: java.security.cert.Certificate[].
    * Only applicable to incoming secure connections with client
    * certificate authentication. Can be null.
    */
   public final static String CLIENT_CERTIFICATES = "certificates";

   /**
    * The connection timeout property: int.
    * Measured in milliseconds. Zero means infinite.
    */
   public final static String CONNECTION_TIMEOUT = "connectionTimeout";

   /**
    * The message tos property: int.
    */
   public final static String TOS = "tos";

   // attributes

   /**
    * True if running inside a J2EE container.
    */
   protected boolean m_bContained = J2EEUtil.isContained();

   // associations

   /**
    * Counter of messages sent since the creation of this component
    */
   protected ThreadLocalCounter m_sentCounter = new ThreadLocalCounter();

   /**
    * The channel metadata object.
    */
   protected TCPChannel m_channel;

   /**
    * The TCP connection factory.
    */
   protected TCPConnectionFactory m_factory;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(TCPSender.class);

   // operations

   /**
    * Sets the channel metadata object.
    * @param channel The TCPChannel metadata object.
    */
   public void setChannel(TCPChannel channel)
   {
      m_channel = channel;
   }

   /**
    * @return The channel metadata object.
    */
   public TCPChannel getChannel()
   {
      return m_channel;
   }

   /**
    * @see nexj.core.integration.Sender#createOutput()
    */
   public ObjectOutput createOutput()
   {
      return new ObjectOutput();
   }

   /**
    * @see nexj.core.integration.Sender#getSentCount()
    */
   public long getSentCount()
   {
      return m_sentCounter.get();
   }

   /**
    * @see nexj.core.integration.Sender#prepare(nexj.core.rpc.TransferObject, nexj.core.rpc.TransferObject, nexj.core.meta.integration.Message)
    */
   public void prepare(TransferObject raw, TransferObject tobj, Message message) throws IntegrationException
   {
   }

   /**
    * Create an InetSocketAddress object from the host/port
    * information specified in the Transfer Object
    * @param tobj The transfer object containing the local host/port fields.
    * @param remoteAddr The remote address.
    * @return A newly created InetSocketAddress or null.
    * @throws Throwable if an error occurs.
    */
   private InetSocketAddress getLocalAddress(TransferObject tobj, InetSocketAddress remoteAddr) throws Throwable
   {
      String s = (String)tobj.findValue(LOCAL_HOST, m_channel.getLocalHost());

      if (s == null)
      {
         if (remoteAddr.getAddress() instanceof Inet6Address)
         {
            s = TCPConnectionRequestInfo.DEFAULT_IP6_BIND;
         }
         else
         {
            s = TCPConnectionRequestInfo.DEFAULT_IP4_BIND;
         }
      }

      final String sHost = s;
      Integer port = (Integer)tobj.findValue(LOCAL_PORT);
      final int nPort = (port == null) ? m_channel.getLocalPort() : port.intValue();

      try
      {
         return (InetSocketAddress)AccessController.doPrivileged(new PrivilegedExceptionAction()
         {
            public Object run() throws Exception
            {
               return new InetSocketAddress(sHost, NetUtil.getBindPort(nPort));
            }
         });
      }
      catch (PrivilegedActionException e)
      {
         throw e.getCause();
      }
   }

   /**
    * Create an InetSocketAddress object from the host/port
    * information specified in the Transfer Object
    * @param tobj The transfer object containing the remote host/port fields.
    * @return A newly created InetSocketAddress.
    * @throws Throwable if an error occurs.
    */
   private InetSocketAddress getRemoteAddress(TransferObject tobj) throws Throwable
   {
      final String sHost = (String)tobj.findValue(REMOTE_HOST, m_channel.getRemoteHost());
      Integer port = (Integer)tobj.findValue(REMOTE_PORT);
      final int nPort = (port == null) ? m_channel.getRemotePort() : port.intValue();

      if (sHost == null)
      {
         throw new RPCException("err.rpc.tcp.unspecifiedRemoteHost");
      }

      try
      {
         return (InetSocketAddress)AccessController.doPrivileged(new PrivilegedExceptionAction()
         {
            public Object run() throws Exception
            {
               return new InetSocketAddress(sHost, nPort);
            }
         });
      }
      catch (PrivilegedActionException e)
      {
         throw e.getCause();
      }
   }

   /**
    * Obtain the certificate (KeyStore) from the transfer object.
    * @param tobj The transfer object.
    * @return The certificate for the outgoing SSL connection. Can be null.
    */
   private KeyStore getCertificate(TransferObject tobj)
   {
      if (!m_channel.isSecure())
      {
         return null;
      }

      return (KeyStore)tobj.findValue(CERTIFICATE, m_channel.getCertificateStore());
   }

   /**
    * Obtain the certificate password from the transfer object.
    * @param tobj The transfer object.
    * @return The certificate password for the outgoing SSL connection. Can be null.
    */
   private String getPassword(TransferObject tobj)
   {
      if (!m_channel.isSecure())
      {
         return null;
      }

      return (String)tobj.findValue(PASSWORD, m_channel.getPassword());
   }

   /**
    * Obtain the trusted certificate from the transfer object.
    * @param tobj The transfer object.
    * @return The trusted certificate for the outgoing SSL connection. Can be null.
    */
   private Certificate getTrust(TransferObject tobj)
   {
      if (!m_channel.isSecure())
      {
         return null;
      }

      return (Certificate)tobj.findValue(TRUST, m_channel.getTrustedCertificate());
   }

   /**
    * Obtain the connection timeout from the transfer object.
    * @param tobj The transfer object.
    * @return The connection timeout in milliseconds (0 means infinite).
    */
   private int getConnectionTimeout(TransferObject tobj)
   {
      Integer n = (Integer)tobj.findValue(CONNECTION_TIMEOUT);

      return (n == null) ? m_channel.getConnectionTimeout() : n.intValue();
   }

   /**
    * Obtain the traffic class from the transfer object.
    * @param tobj The transfer object.
    * @return The RFC 1349 type-of-service value (sum of lowCost=2, reliability=4, throughput=8, lowDelay=16).
    */
   private int getTOS(TransferObject tobj)
   {
      Integer tos = (Integer)tobj.findValue(TOS);

      return (tos == null) ? m_channel.getTOS() : tos.intValue();
   }

   /**
    * Verify port ranges are correct, and obtain an open connection from the TCP connection factory.
    * @param remoteAddr The remote socket address.
    * @param localAddr The local socket address.
    * @param trustedCertificate The trusted X.509 certificate.
    * @param certificateStore The TCP connection's client certificate and private key, bound together in a KeyStore.
    * @param sPassword The certificate password.
    * @param nTimeout The connection timeout in milliseconds (0 means infinite).
    * @return The corresponding TCPConnection object.
    * @throws IOException if an I/O error occurs.
    * @throws ResourceException if unable to open connection.
    */
   protected TCPConnection open(InetSocketAddress remoteAddr, InetSocketAddress localAddr,
      Certificate trustedCertificate, KeyStore certificateStore, String sPassword, int nTimeout) throws IOException, ResourceException
   {
      // validate ports
      if (remoteAddr.getPort() < TCPResourceAdapter.MIN_PORT
         || remoteAddr.getPort() > TCPResourceAdapter.MAX_PORT)
      {
         throw new RPCException("err.rpc.tcp.invalidRemotePort",
            new Object[] { "" + TCPResourceAdapter.MIN_PORT, "" + TCPResourceAdapter.MAX_PORT });
      }

      if (localAddr != null && (localAddr.getPort() < TCPResourceAdapter.MIN_PORT
         || localAddr.getPort() > TCPResourceAdapter.MAX_PORT))
      {
         throw new RPCException("err.rpc.tcp.invalidLocalPort",
            new Object[] { "" + TCPResourceAdapter.MIN_PORT, "" + TCPResourceAdapter.MAX_PORT });
      }

      // get connection object
      if (m_channel.isSecure())
      {
         return m_factory.open(
            remoteAddr,
            localAddr,
            true,
            trustedCertificate,
            certificateStore,
            sPassword,
            nTimeout,
            m_channel.isNoDelay(),
            m_channel.isKeepAlive(),
            m_channel.getSenderBufferSize(),
            m_channel.getReceiverBufferSize());
      }
      else
      {
         return m_factory.open(
            remoteAddr,
            localAddr,
            nTimeout,
            m_channel.isNoDelay(),
            m_channel.isKeepAlive(),
            m_channel.getSenderBufferSize(),
            m_channel.getReceiverBufferSize());
      }
   }

   /**
    * Verify port ranges are correct, and obtain an open connection from the TCP connection factory.
    * @param remoteAddr The remote socket address.
    * @param localAddr The local socket address.
    * @param nTimeout The connection timeout in milliseconds (0 means infinite).
    * @return The corresponding TCPConnection object.
    * @throws IOException if an I/O error occurs.
    * @throws ResourceException if unable to open connection.
    */
   protected TCPConnection open(InetSocketAddress remoteAddr, InetSocketAddress localAddr,
      int nTimeout) throws IOException, ResourceException
   {
      return open(remoteAddr, localAddr, null, null, null, nTimeout);
   }

   /**
    * Obtains a MessageOutputStream if needed and writes out the transfer object's data.
    * @param tobj The transfer object to send.
    * @param con The connection object to use for sending.
    * @param out The MessageOutputStream to use. If null, a new one will be created.
    * @throws IOException if an I/O error occurs.
    */
   public void send(TransferObject tobj, TCPConnection con, MessageOutputStream out) throws IOException
   {
      if (!m_channel.isSendable())
      {
         throw new RPCException("err.rpc.notSender", new Object[]{m_channel.getName()});
      }

      long lStartTime = System.nanoTime();
      String sSenderStatPath = m_channel.getSenderStatPath();

      try
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Sending a message on channel \"" + m_channel.getName() + "\"");
            s_logger.dump(tobj);
         }

         Object body = tobj.findValue(BODY);
         InputStream dataStream;

         if (body == null)
         {
            dataStream = new ByteArrayInputStream(new byte[0]);
         }
         else if (body instanceof String)
         {
            dataStream = new ByteArrayInputStream(((String)body).getBytes((m_channel.getEncoding() == null) ?
               XMLUtil.ENCODING : m_channel.getEncoding()));
         }
         else
         {
            dataStream = ((Binary)body).getInputStream();
         }

         // get message output stream
         if (out == null)
         {
            out = m_channel.getMessageStreamFactory(null).createMessageOutputStream(con.getOutputStream());
         }

         if (out.start(tobj))
         {
            m_sentCounter.add(1);
            StatUtil.incrCounter(m_channel.getType().getMetadata(),
               sSenderStatPath, Channel.STAT_TOTAL_COUNT, 1);

            InvocationContext context = ((InvocationContext)ThreadContextHolder.getContext());

            if (context != null)
            {
               context.addRPCCount(1);
            }

            // write data
            IOUtil.copy(out, dataStream);
            out.end(tobj);
         }
         else
         {
            con.invalidate();

            throw new RPCException("err.rpc.tcp.forciblyClosedConnection");
         }

      }
      finally
      {
         StatUtil.updateAverage(m_channel.getType().getMetadata(), sSenderStatPath,
               Channel.STAT_AVERAGE_SEND_TIME, (double)(System.nanoTime() - lStartTime) / 1000000);
      }
   }

   /**
    * Obtain a connection and send the given transfer object onto the channel.
    * @see nexj.core.integration.Sender#send(nexj.core.rpc.TransferObject)
    */
   public void send(TransferObject tobj) throws IntegrationException
   {
      TCPConnection con = null;

      try
      {
         InetSocketAddress remoteAddr = getRemoteAddress(tobj);

         con = open(remoteAddr, getLocalAddress(tobj, remoteAddr), getTrust(tobj),
            getCertificate(tobj), getPassword(tobj), getConnectionTimeout(tobj));
         con.setTOS(getTOS(tobj));
         send(tobj, con, null);
      }
      catch (Throwable t)
      {
         throw new RPCException("err.rpc.tcp", t);
      }
      finally
      {
         if (con != null)
         {
            con.close();
         }
      }
   }

   /**
    * Sends the given transfer object, waits for a response, and
    * constructs the resulting TransferObject.
    * @param tobj The transfer object to send.
    * @return The transfer object containing the response.
    */
   public TransferObject respond(TransferObject tobj) throws IntegrationException
   {
      TransferObject response;
      TCPConnection con = null;

      try
      {
         InetSocketAddress remoteAddr = getRemoteAddress(tobj);

         con = open(remoteAddr, getLocalAddress(tobj, remoteAddr), getTrust(tobj),
            getCertificate(tobj), getPassword(tobj), getConnectionTimeout(tobj));
         con.setTOS(getTOS(tobj));
         send(tobj, con, null);

         // Get the input channel
         MessageInputStream mis = m_channel.getMessageStreamFactory(null).createMessageInputStream(con.getInputStream());

         con.setReadTimeout(m_channel.getReadTimeout());
         remoteAddr = con.getRemoteAddress();

         InetSocketAddress localAddr = con.getLocalAddress();

         // Create the TransferObject
         response = new TransferObject("TCP", 6);
         response.setValue(TCPSender.REMOTE_HOST, remoteAddr.getAddress().getHostAddress());
         response.setValue(TCPSender.REMOTE_PORT, Primitive.createInteger(remoteAddr.getPort()));
         response.setValue(TCPSender.LOCAL_HOST, localAddr.getAddress().getHostAddress());
         response.setValue(TCPSender.LOCAL_PORT, Primitive.createInteger(localAddr.getPort()));
         response.setValue(TCPSender.CHANNEL, m_channel.getName());

         if (mis.next(response))
         {
            // get body
            BufferedInputStream bufStream = new BufferedInputStream(new NoCloseInputStream(mis));
            Object body;

            if (m_channel.getEncoding() != null)
            {
               String sEncoding = m_channel.getEncoding().toLowerCase(Locale.ENGLISH);
               Reader ireader = new BufferedReader(new InputStreamReader(
                  UTF8BOMIgnoreInputStream.wrap(bufStream, sEncoding), sEncoding));

               body = new ReaderInput(ireader);
            }
            else
            {
               body = new StreamInput(bufStream);
            }

            response.setValue(TCPSender.BODY, body);
         }
      }
      catch (RPCException e)
      {
         // don't wrap RPC exception
         throw e;
      }
      catch (Throwable t)
      {
         throw new RPCException("err.rpc.tcp", t);
      }
      finally
      {
         if (con != null)
         {
            con.close();
         }
      }

      return response;
   }

   /**
    * @see nexj.core.integration.Sender#send(java.util.Collection)
    */
   public void send(Collection col) throws IntegrationException
   {
      TransferObject tobj;
      TCPConnection con = null;
      InetSocketAddress prevRemoteAddr = null;
      InetSocketAddress prevLocalAddr = null;
      KeyStore prevCertificate = null;
      String sPrevPassword = null;
      Certificate prevTrust = null;
      MessageOutputStream msgOut = null;

      try
      {
         for (Iterator itr = col.iterator(); itr.hasNext(); )
         {
            tobj = (TransferObject)itr.next();

            try
            {
               InetSocketAddress remoteAddr = getRemoteAddress(tobj);
               InetSocketAddress localAddr = getLocalAddress(tobj, remoteAddr);
               KeyStore certificate = getCertificate(tobj);
               String sPassword = getPassword(tobj);
               Certificate trust = getTrust(tobj);
               int nTimeout = getConnectionTimeout(tobj);

               if (con == null
                  || prevRemoteAddr == null || !remoteAddr.equals(prevRemoteAddr)
                  || prevLocalAddr == null || !ObjUtil.equal(localAddr, prevLocalAddr)
                  || !ObjUtil.equal(certificate, prevCertificate)
                  || !ObjUtil.equal(sPassword, sPrevPassword)
                  || !ObjUtil.equal(trust, prevTrust))
               {
                  if (con != null)
                  {
                     con.close();
                  }

                  con = open(remoteAddr, localAddr, trust, certificate, sPassword, nTimeout);
                  con.setTOS(getTOS(tobj));
                  msgOut = m_channel.getMessageStreamFactory(null).createMessageOutputStream(con.getOutputStream());
               }

               send(tobj, con, msgOut);

               prevRemoteAddr = remoteAddr;
               prevLocalAddr = localAddr;
               prevCertificate = certificate;
               sPrevPassword = sPassword;
               prevTrust = trust;
            }
            catch (Throwable t)
            {
               throw new RPCException("err.rpc.tcp", t);
            }
         }
      }
      finally
      {
         if (con != null)
         {
            con.close();
         }
      }
   }

   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      if (m_bContained)
      {
         String sFactory = J2EEUtil.JNDI_ENV_PREFIX + "tcp/" + m_channel.getName();

         if (s_logger.isInfoEnabled())
         {
            s_logger.info("Binding to connection factory \"" + sFactory + "\"");
         }

         m_factory = (TCPConnectionFactory)new InitialContext().lookup(sFactory);
      }
      else
      {
         s_logger.info("Binding to TCP resource adapter connection factory");

         ManagedConnectionFactory mcf = (ManagedConnectionFactory)Class.forName(
            SysUtil.PACKAGE + ".core.rpc.tcp.ra.TCPManagedConnectionFactory").newInstance();

         m_factory = (TCPConnectionFactory)mcf.createConnectionFactory();
      }
   }
}
