// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp;

import nexj.core.rpc.RPCException;

/**
 * Exception thrown by the MLLPSend service when a NAK is received after all retry attempts have been exhausted.
 */
public class MLLPNegativeAcknowledgementException extends RPCException
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = -1713888939291997448L;

   // constructors

   /**
    * Construct a MLLPNegativeAcknowledgementException.
    */
   public MLLPNegativeAcknowledgementException()
   {
      super("err.rpc.tcp.mllp.nak");
   }

}
