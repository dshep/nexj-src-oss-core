// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp;

import java.net.InetSocketAddress;
import java.security.KeyStore;
import java.security.cert.Certificate;

import javax.resource.ResourceException;

/**
 * TCP connection factory.
 */
public interface TCPConnectionFactory
{
   /**
    * Opens a TCP connection.
    * @param remoteAddr The remote socket address.
    * @param localAddr The local socket address.
    * @param bSecure True iff this connection should use SSL/TLS.
    * @param trustedCertificate The trusted X.509 certificate. Can be null.
    * @param certificateStore The TCP connection's client certificate and private key, bound together in a KeyStore. Can be null.
    * @param sPassword The key store's password.
    * @param nTimeout The connection timeout in milliseconds (0 means infinite).
    * @param bNoDelay True if the TCP no-delay property should be set on the underlying socket.
    * @param bKeepAlive True if the TCP keep-alive property should be set on the underlying socket.
    * @param nSenderBufferSize The SO_SNDBUF option for the underlying socket, 0 for system default.
    * @param nReceiverBufferSize The SO_RCVBUF option for the underlying socket, 0 for system default.
    * @return A usable TCPConnection object.
    * @throws ResourceException if unable to open connection.
    */
   TCPConnection open(InetSocketAddress remoteAddr, InetSocketAddress localAddr,
      boolean bSecure, Certificate trustedCertificate, KeyStore certificateStore, String sPassword,
      int nTimeout, boolean bNoDelay, boolean bKeepAlive, int nSenderBufferSize, int nReceiverBufferSize) throws ResourceException;

   /**
    * Opens a TCP connection.
    * @param remoteAddr The remote socket address.
    * @param localAddr The local socket address.
    * @param nTimeout The connection timeout in milliseconds (0 means infinite).
    * @param bNoDelay True if the TCP no-delay property should be set on the underlying socket.
    * @param bKeepAlive True if the TCP keep-alive property should be set on the underlying socket.
    * @param nSenderBufferSize The SO_SNDBUF option for the underlying socket, 0 for system default.
    * @param nReceiverBufferSize The SO_RCVBUF option for the underlying socket, 0 for system default.
    * @return A usable TCPConnection object.
    * @throws ResourceException if unable to open connection.
    */
   TCPConnection open(InetSocketAddress remoteAddr, InetSocketAddress localAddr,
      int nTimeout, boolean bNoDelay, boolean bKeepAlive, int nSenderBufferSize, int nReceiverBufferSize) throws ResourceException;

   /**
    * @param remoteAddr The remote socket address.
    * @return A usable TCPConnection object.
    * @throws ResourceException if unable to open connection.
    */
   TCPConnection open(InetSocketAddress remoteAddr) throws ResourceException;
}
