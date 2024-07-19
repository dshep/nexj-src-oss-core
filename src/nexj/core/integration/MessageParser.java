// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessageTable;
import nexj.core.rpc.TransferObject;

/**
 * Interface implemented by the message parsing components.
 */
public interface MessageParser
{
   /**
    * Parses a known message from an input.
    * @param in The message input.
    * @param msh The message metadata.
    * @return The parsed message.
    */
   TransferObject parse(Input in, Message msg) throws IntegrationException;
   
   /**
    * Detects and parses a message from an input.
    * @param in The message input.
    * @param table An initialized message table.
    * @return The parsed message.
    */
   TransferObject parse(Input in, MessageTable table) throws IntegrationException;

   /**
    * Initializes a message table by creating an appropriate parser table on it.
    * @param table The message table.
    */
   void initializeMessageTable(MessageTable table) throws IntegrationException;
}
