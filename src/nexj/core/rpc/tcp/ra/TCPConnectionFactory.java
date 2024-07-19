// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp.ra;

import java.net.InetSocketAddress;
import java.security.KeyStore;
import java.security.cert.Certificate;

import javax.resource.ResourceException;
import javax.resource.spi.ConnectionManager;

import nexj.core.rpc.ra.GenericConnectionFactory;
import nexj.core.rpc.tcp.TCPConnection;

/**
 * TCPConnectionFactory
 */
public class TCPConnectionFactory extends GenericConnectionFactory implements nexj.core.rpc.tcp.TCPConnectionFactory
{
   // attributes

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = 8237535046649726650L;

   // associations

   /**
    * The TCP managed connection factory.
    */
   protected TCPManagedConnectionFactory m_factory;

   // constructors

   /**
    * Constructs the connection factory.
    * @param factory The managed connection factory.
    * @param manager The connection manager.
    */
   protected TCPConnectionFactory(TCPManagedConnectionFactory factory, ConnectionManager manager)
   {
      super(manager);

      m_factory = factory;
   }

   // operations

   /**
    * @see nexj.core.rpc.tcp.TCPConnectionFactory#open(java.net.InetSocketAddress, java.net.InetSocketAddress, boolean, java.security.cert.Certificate, java.security.KeyStore, java.lang.String, int, boolean, boolean, int, int)
    */
   public TCPConnection open(InetSocketAddress remoteAddress, InetSocketAddress localAddr,
      boolean bSecure, Certificate trustedCertificate, KeyStore certificateStore, String sPassword,
      int nTimeout, boolean bNoDelay, boolean bKeepAlive, int nSenderBufferSize, int nReceiverBufferSize) throws ResourceException
   {
      return (TCPConnection)m_manager.allocateConnection(m_factory,
         new TCPConnectionRequestInfo(
            remoteAddress,
            localAddr,
            bSecure,
            trustedCertificate,
            certificateStore,
            sPassword,
            nTimeout,
            bNoDelay,
            bKeepAlive,
            nSenderBufferSize,
            nReceiverBufferSize));
   }

   /**
    * @see nexj.core.rpc.tcp.TCPConnectionFactory#open(java.net.InetSocketAddress, java.net.InetSocketAddress, int, boolean, boolean, int, int)
    */
   public TCPConnection open(InetSocketAddress remoteAddr, InetSocketAddress localAddr,
      int nTimeout, boolean bNoDelay, boolean bKeepAlive, int nSenderBufferSize, int nReceiverBufferSize) throws ResourceException
   {
      return (TCPConnection)m_manager.allocateConnection(m_factory,
         new TCPConnectionRequestInfo(
            remoteAddr,localAddr,
            nTimeout,
            bNoDelay,
            bKeepAlive,
            nSenderBufferSize,
            nReceiverBufferSize));
   }

   /**
    * @see nexj.core.rpc.tcp.TCPConnectionFactory#open(java.net.InetSocketAddress)
    */
   public TCPConnection open(InetSocketAddress remoteAddr) throws ResourceException
   {
      return open(remoteAddr, null, 0, false, false, 0, 0);
   }
}
