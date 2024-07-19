// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.vcard;

import nexj.core.integration.Input;
import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageParser;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessageTable;
import nexj.core.rpc.TransferObject;

/**
 * vCard (and related formats) message parser.
 */
public class VCardMessageParser implements MessageParser
{
   // operations

   /**
    * @see nexj.core.integration.MessageParser#initializeMessageTable(nexj.core.meta.integration.MessageTable)
    */
   public void initializeMessageTable(MessageTable table) throws IntegrationException
   {
   }

   /**
    * @see nexj.core.integration.MessageParser#parse(nexj.core.integration.Input, nexj.core.meta.integration.Message)
    */
   public TransferObject parse(Input in, Message msg) throws IntegrationException
   {
      throw new UnsupportedOperationException("vCard message parser not implemented");
   }

   /**
    * @see nexj.core.integration.MessageParser#parse(nexj.core.integration.Input, nexj.core.meta.integration.MessageTable)
    */
   public TransferObject parse(Input in, MessageTable table) throws IntegrationException
   {
      throw new UnsupportedOperationException("vCard message parser not implemented");
   }
}
