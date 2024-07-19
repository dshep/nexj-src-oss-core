// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.io;

import java.io.OutputStream;
import java.io.Writer;

import nexj.core.integration.DelayedOutput;
import nexj.core.integration.IntegrationException;

/**
 * Delayed Output implementation.
 */
public class DelayedObjectOutput extends ObjectOutput implements DelayedOutput
{
   // associations

   /**
    * The Action object.
    */
   protected Action m_action;

   // operations

   /**
    * @see nexj.core.integration.io.ObjectOutput#getOutputStream()
    */
   public OutputStream getOutputStream() throws IntegrationException
   {
      return null;
   }

   /**
    * @see nexj.core.integration.io.ObjectOutput#getWriter()
    */
   public Writer getWriter() throws IntegrationException
   {
      return null;
   }

   /**
    * @see nexj.core.integration.io.ObjectOutput#getObject()
    */
   public Object getObject()
   {
      return new DelayedObjectInput(m_action);
   }

   /**
    * @see nexj.core.integration.io.ObjectOutput#setAction(nexj.core.integration.DelayedOutput.Action)
    */
   public void setAction(Action action)
   {
      m_action = action;
   }
}
