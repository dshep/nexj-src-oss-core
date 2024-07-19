// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import nexj.core.scripting.Pair;

/**
 * Step for triggering script execution with events.
 */
public class Trigger extends HandlerStep implements Scripted
{
   // associations
   
   /**
    * The action body.
    */
   protected Pair m_body;

   // constructors
   
   /**
    * Constructs the trigger.
    * @param sName The step name.
    * @param activity The containing activity.
    */
   public Trigger(String sName, Activity activity)
   {
      super(sName, activity);
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
}
