// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;

/**
 * Scripting step.
 */
public class Script extends Code implements Scripted
{
   // associations
   
   /**
    * The action body.
    */
   protected Pair m_body;

   // constructors
   
   /**
    * Constructs the script.
    * @param sName The script name.
    */
   public Script(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the script.
    */
   public Script()
   {
      super();
   }

   // operations

   /**
    * Sets the action body.
    * @param body The action body to set.
    */
   public void setBody(Pair body)
   {
      verifyNotReadOnly();
      m_body = body;
   }

   /**
    * @return The action body.
    */
   public Pair getBody()
   {
      return m_body;
   }

   /**
    * @see nexj.core.meta.workflow.Step#generate(nexj.core.scripting.Machine)
    */
   public void generate(Machine machine)
   {
      super.generate(machine);
      m_body = null;
   }
}
