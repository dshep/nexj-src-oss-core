// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import nexj.core.meta.integration.Message;
import nexj.core.rpc.TransferObject;

/**
 * Interface implemented by message formatting components.
 */
public interface MessageFormatter
{
   /**
    * Formats the message to the specified output.
    * @param tobj The message to format.
    * @param message The message metadata. While this can readily be
    * determined from the TO class name, at this point a lookup already
    * has been done to determine the formatter component.
    * @param out The message output.
    */
   void format(TransferObject tobj, Message message, Output out) throws IntegrationException;
}
