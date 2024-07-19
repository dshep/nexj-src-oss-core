// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp;

import java.io.InputStream;
import java.net.InetSocketAddress;
import java.security.cert.Certificate;

import nexj.core.meta.Repository;
import nexj.core.rpc.IntegrationMDB;
import nexj.core.rpc.ServerException;
import nexj.core.util.Logger;

/**
 * The JMS server Message Driven Bean
 */
public class TCPServerMDB extends IntegrationMDB implements TCPListener
{
   // constants

   /**
    *  Serialization version.
    */
   private final static long serialVersionUID = 7356270822442441454L;

   // associations

   /**
    * The MDB logger.
    */
   protected final static Logger s_logger = Logger.getLogger(TCPServerMDB.class);

   // operations

   /**
    * @see nexj.core.rpc.ServerMDB#getLogger()
    */
   protected Logger getLogger()
   {
      return s_logger;
   }

   /**
    * @see nexj.core.rpc.tcp.TCPListener#onMessage(java.io.InputStream, java.net.InetSocketAddress, java.net.InetSocketAddress, java.security.cert.Certificate[])
    */
   public void onMessage(InputStream in, InetSocketAddress remoteAddress,
      InetSocketAddress localAddress, Certificate[] clientCerts)
   {
      TCPListener receiver = (TCPListener)Repository.getMetadata().getChannel(m_sChannelName).
         getReceiver().getInstance(null);

      try
      {
         receiver.onMessage(in, remoteAddress, localAddress, clientCerts);
      }
      catch (Throwable t)
      {
         if (!(t instanceof ServerException) && s_logger.isDebugEnabled())
         {
            s_logger.debug("Error in " + this, t);
         }
      }
   }

}
