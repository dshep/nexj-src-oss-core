// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.udp;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.util.Collection;
import java.util.Iterator;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.Sender;
import nexj.core.integration.io.ObjectOutput;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.channel.udp.UDPChannel;
import nexj.core.monitoring.ThreadLocalCounter;
import nexj.core.monitoring.jmx.StatUtil;
import nexj.core.rpc.RPCException;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Initializable;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.util.Binary;
import nexj.core.util.Logger;
import nexj.core.util.XMLUtil;

/**
 * UDP sender.
 */
public class UDPSender implements Sender, Initializable
{
   // constants

   /**
    * The message host property: String.
    */
   public final static String HOST = "host";

   /**
    * The message port property: int.
    */
   public final static String PORT = "port";

   /**
    * The message local host property: String.
    */
   public final static String LOCAL_HOST = "localHost";

   /**
    * The message local port property: int.
    */
   public final static String LOCAL_PORT = "localPort";

   /**
    * The message ttl property: int.
    */
   public final static String TTL = "ttl";

   /**
    * The message tos property: int.
    */
   public final static String TOS = "tos";

   // attributes

   /**
    * Counter of messages sent since the creation of this component
    */
   protected ThreadLocalCounter m_sentCounter = new ThreadLocalCounter();

   // associations

   /**
    * The channel metadata object.
    */
   protected UDPChannel m_channel;

   /**
    * The sender logger.
    */
   protected Logger m_logger;

   // operations

   /**
    * Sets the channel metadata object.
    * @param channel The channel metadata object to set.
    */
   public void setChannel(UDPChannel channel)
   {
      m_channel = channel;
   }

   /**
    * @return The channel metadata object.
    */
   public UDPChannel getChannel()
   {
      return m_channel;
   }

   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      m_logger = m_channel.getLogger();
   }

   /**
    * @see nexj.core.integration.Sender#createOutput()
    */
   public ObjectOutput createOutput()
   {
      return new ObjectOutput();
   }

   /**
    * @see nexj.core.integration.Sender#prepare(nexj.core.rpc.TransferObject, nexj.core.rpc.TransferObject, nexj.core.meta.integration.Message)
    */
   public void prepare(TransferObject raw, TransferObject tobj, Message message) throws IntegrationException
   {
   }

   /**
    * @see nexj.core.integration.Sender#send(nexj.core.rpc.TransferObject)
    */
   public void send(TransferObject tobj) throws IntegrationException
   {
      if (!m_channel.isSendable())
      {
         throw new RPCException("err.rpc.notSender", new Object[]{m_channel.getName()});
      }

      long lStartTime = System.nanoTime();
      String sSenderStatPath = m_channel.getSenderStatPath();

      if (m_logger.isDebugEnabled())
      {
         m_logger.debug("Sending a message on channel \"" + m_channel.getName() + "\"");
         m_logger.dump(tobj);
      }

      UDPConnection con = null;

      try
      {
         String sHost = (String)tobj.findValue(HOST, m_channel.getHost());
         Number port = (Number)tobj.findValue(PORT);
         int nPort = (port == null) ? m_channel.getPort() : port.intValue();

         if (sHost == null)
         {
            throw new RPCException("err.integration.udp.unspecifiedHost");
         }

         if (nPort < 0)
         {
            throw new RPCException("err.integration.udp.unspecifiedPort");
         }

         SocketAddress address = new InetSocketAddress(sHost, nPort);
         String sLocalHost = (String)tobj.findValue(LOCAL_HOST, m_channel.getLocalHost());
         Number localPort = (Number)tobj.findValue(LOCAL_PORT);
         int nLocalPort = (localPort == null) ? m_channel.getLocalPort() : localPort.intValue();
         SocketAddress bindAddress = null;

         if (sLocalHost != null || nLocalPort > 0)
         {
            if (sLocalHost == null)
            {
               bindAddress = new InetSocketAddress((InetAddress)null, nLocalPort);
            }
            else
            {
               bindAddress = new InetSocketAddress(sLocalHost, nLocalPort);
            }
         }

         Number ttl = (Number)tobj.findValue(TTL);
         int nTTL = (ttl == null) ? m_channel.getTTL() : ttl.intValue();

         Number tos = (Number)tobj.findValue(TOS);
         int nTOS = (tos == null) ? m_channel.getTOS() : tos.intValue();

         Object body = tobj.findValue(BODY);
         byte[] data;

         if (body == null)
         {
            data = new byte[0];
         }
         else if (body instanceof String)
         {
            data = ((String)body).getBytes((m_channel.getEncoding() == null) ?
               XMLUtil.ENCODING : m_channel.getEncoding());
         }
         else
         {
            data = ((Binary)body).getData();
         }

         con = (UDPConnection)m_channel.getResourcePool().get(bindAddress);
         m_sentCounter.add(1);
         StatUtil.incrCounter(m_channel.getType().getMetadata(),
            sSenderStatPath, Channel.STAT_TOTAL_COUNT, 1);

         InvocationContext context = ((InvocationContext)ThreadContextHolder.getContext());

         if (context != null)
         {
            context.addRPCCount(1);
         }

         con.send(address, data, 0, data.length, nTOS, nTTL);
      }
      catch (IntegrationException e)
      {
         throw e;
      }
      catch (Exception e)
      {
         throw new RPCException("err.rpc.udp", e);
      }
      finally
      {
         if (con != null)
         {
            con.release();
         }

         StatUtil.updateAverage(m_channel.getType().getMetadata(), sSenderStatPath,
               Channel.STAT_AVERAGE_SEND_TIME, (double)(System.nanoTime() - lStartTime) / 1000000);
      }
   }

   /**
    * @see nexj.core.integration.Sender#send(java.util.Collection)
    */
   public void send(Collection col) throws IntegrationException
   {
      for (Iterator itr = col.iterator(); itr.hasNext();)
      {
         send((TransferObject)itr.next());
      }
   }

   /**
    * @see nexj.core.integration.Sender#getSentCount()
    */
   public long getSentCount()
   {
      return m_sentCounter.get();
   }
}
