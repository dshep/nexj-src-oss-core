// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.io;

import java.io.InputStream;
import java.io.Reader;

import nexj.core.integration.DelayedInput;
import nexj.core.integration.IntegrationException;
import nexj.core.integration.Output;
import nexj.core.integration.DelayedOutput.Action;
import nexj.core.util.Binary;

/**
 * Delayed input implementation.
 */
public class DelayedObjectInput extends ObjectInput implements DelayedInput
{
   // associations

   /**
    * The Action object.
    */
   protected Action m_action;

   // constructors

   /**
    * Constructs the input.
    * @param action The delayed action.
    */
   public DelayedObjectInput(Action action)
   {
      super(null);

      m_action = action;
   }

   // operations

   /**
    * @see nexj.core.integration.Input#getInputStream()
    */
   public InputStream getInputStream() throws IntegrationException
   {
      if (m_obj == null)
      {
         createObject();
      }

      return super.getInputStream();
   }

   /**
    * @see nexj.core.integration.Input#getReader()
    */
   public Reader getReader() throws IntegrationException
   {
      if (m_obj == null)
      {
         createObject();
      }

      return super.getReader();
   }

   /**
    * @see nexj.core.integration.Input#getBinary()
    */
   public Binary getBinary() throws IntegrationException
   {
      if (m_obj == null)
      {
         createObject();
      }

      return super.getBinary();
   }
   
   /**
    * @see nexj.core.integration.Input#getString()
    */
   public String getString() throws IntegrationException
   {
      if (m_obj == null)
      {
         createObject();
      }

      return super.getString();
   }

   /**
    * @see nexj.core.integration.DelayedInput#output(nexj.core.integration.Output)
    */
   public void output(Output output)
   {
      m_action.run(output);
   }

   /**
    * Run m_action and return the generated object.
    */
   protected void createObject()
   {
      ObjectOutput out = new ObjectOutput();

      out.setEncoding(getEncoding());
      m_action.run(out);
      m_obj = out.getObject();
   }
}
