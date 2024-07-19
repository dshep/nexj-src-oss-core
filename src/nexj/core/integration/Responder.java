// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import nexj.core.rpc.TransferObject;

/**
 * Interface for synchronous send-receive message processing.
 */
public interface Responder
{
   /**
    * Sends a message and receives a response.
    * @param tobj The transfer object containing the message.
    * @return The transfer object with the response.
    */
   TransferObject respond(TransferObject tobj) throws IntegrationException;
}
