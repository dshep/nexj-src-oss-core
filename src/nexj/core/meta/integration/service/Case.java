// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.service;

import nexj.core.meta.integration.Message;
import nexj.core.meta.workflow.Branch;

/**
 * Message dispatch case.
 */
public class Case extends Branch
{
   // associations

   /**
    * The dispatch message.
    */
   protected Message m_message;

   // operations

   /**
    * Sets the dispatch message.
    * @param message The dispatch message to set.
    */
   public void setMessage(Message message)
   {
      verifyNotReadOnly();
      m_message = message;
   }

   /**
    * @return The dispatch message.
    */
   public Message getMessage()
   {
      return m_message;
   }
}
