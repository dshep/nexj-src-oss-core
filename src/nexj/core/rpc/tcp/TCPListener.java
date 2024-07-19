// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp;

import java.io.InputStream;
import java.net.InetSocketAddress;
import java.security.cert.Certificate;

/**
 * Interface implemented by TCP listeners.
 */
public interface TCPListener
{
   /**
    * Receives a TCP message.
    * @param in An input stream from which to read the message.
    * @param remoteAddress The remote address (source) of the incoming message.
    * @param localAddress The local address of the incoming message.
    * @param certificateArray The client's certificate chain for SSL client authentication if applicable, otherwise null.
    */
   void onMessage(InputStream in, InetSocketAddress remoteAddress,
      InetSocketAddress localAddress, Certificate[] certificateArray);
}
