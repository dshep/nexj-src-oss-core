// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.wsdl;

import nexj.core.util.Named;

/**
 * Web services operation definition.
 */
public class Operation implements Named
{
   // attributes

   /**
    * The operation name, without namespace/prefix.
    */
   protected String m_sName;

   /**
    * The operation action.
    */
   protected String m_sAction;

   // associations

   /**
    * The input message.
    */
   protected Message m_input;

   /**
    * The output message.
    */
   protected Message m_output;

   // constructors

   /**
    * Constructs a new web services operation definition.
    * @param sName The operation name, without namespace/prefix.
    */
   public Operation(String sName)
   {
      m_sName = sName;
   }

   // operations

   /**
    * @see nexj.core.util.Named#getName()
    */
   public String getName()
   {
      return m_sName;
   }

   /**
    * Sets the input message.
    * @param input The input message.
    */
   public void setInput(Message input)
   {
      m_input = input;
   }

   /**
    * Gets the input message.
    * @return The input message.
    */
   public Message getInput()
   {
      return m_input;
   }

   /**
    * Sets the output message.
    * @param output The output message.
    */
   public void setOutput(Message output)
   {
      m_output = output;
   }

   /**
    * Gets the output message.
    * @return The output message.
    */
   public Message getOutput()
   {
      return m_output;
   }

   /**
    * Sets the operation action.
    * @param sAction The operation action.
    */
   public void setAction(String sAction)
   {
      m_sAction = sAction;
   }

   /**
    * Gets the operation action.
    * @return The operation action.
    */
   public String getAction()
   {
      return m_sAction;
   }
}
