// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import java.util.Collection;

import nexj.core.integration.io.ObjectOutput;
import nexj.core.meta.integration.Message;
import nexj.core.rpc.TransferObject;

/**
 * Interface implemented by message client adapters.
 * Used by clients to initiate message processing.
 * The messages are passed in transfer objects, where
 * the class name is the name of the message metadata object.
 */
public interface Sender
{
   // constants
   
   // common message parts

   /**
    * The message channel part: String.
    */
   public final static String CHANNEL = "channel";

   /**
    * The message body part: Object.
    */
   public final static String BODY = "body";
   
   /**
    * The message correlation Id part: String | Binary.
    */
   public final static String CORRELATION_ID = "correlationId";
   
   // operations

   /**
    * @return Optimal object output for message formatting.  
    */
   ObjectOutput createOutput();

   /**
    * Prepares for output a raw message formatted from
    * another message by enhancing it with headers etc.
    * @param raw The raw message to enhance.
    * @param tobj The original message, from which raw has been formatted.
    * @param message The message metadata. 
    */
   void prepare(TransferObject raw, TransferObject tobj, Message message) throws IntegrationException;

   /**
    * Initiates processing of a single message by the implementor.
    * @param tobj The transfer object containing the message.
    */
   void send(TransferObject tobj) throws IntegrationException;

   /**
    * Initiates processing of an ordered message collection by the implementor.
    * @param col Transfer object collection.
    */
   void send(Collection col) throws IntegrationException;
   
   /**
    * @return Count of messages sent since the creation of this component
    */
   long getSentCount();
}
