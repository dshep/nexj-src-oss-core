// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;

/**
 * TCP connection.
 */
public interface TCPConnection
{
   /**
    * @return The local socket address.
    */
   InetSocketAddress getLocalAddress();

   /**
    * @return the remote socket address
    */
   InetSocketAddress getRemoteAddress();

   /**
    * @return the socket's associated output stream
    */
   OutputStream getOutputStream() throws IOException;

   /**
    * @return the socket's associated input stream
    */
   InputStream getInputStream() throws IOException;

   /**
    * Closes the TCP socket connection.
    */
   void close();

   /**
    * @return True iff the connection is not in an error state.
    */
   public boolean isValid();

   /**
    * @return True iff the underlying socket has returned end-of-stream.
    */
   public boolean isEndOfStream();

   /**
    * Invalidates the connection.
    */
   public void invalidate();
   
   /**
    * @return The message read timeout (milliseconds).
    */
   public int getReadTimeout();

   /**
    * @param nTimeout The message read timeout (milliseconds).
    */
   public void setReadTimeout(int nTimeout);

   /**
    * @return The RFC 1349 type-of-service value (sum of lowCost=2, reliability=4, throughput=8, lowDelay=16).
    */
   public int getTOS();

   /**
    * @param nTOS The RFC 1349 type-of-service value (sum of lowCost=2, reliability=4, throughput=8, lowDelay=16).
    */
   public void setTOS(int nTOS);
}
