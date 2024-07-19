// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import java.util.Collections;
import java.util.List;

import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Symbol;

/**
 * Flow timeout wait.
 */
public class Wait extends Step
{
   // constants
   
   /**
    * The Wait step cleanup list, consisting of the timers association.
    */
   protected final static List CLEANUP_ASSOC_LIST = Collections.singletonList(Symbol.TIMERS);
   
   // associations
   
   /**
    * The associated timeout.
    */
   protected Timeout m_timeout;

   // constructors

   /**
    * @param timeout The preceding timeout step.
    * Constructs the wait.
    */
   public Wait(Timeout timeout)
   {
      super();
      m_timeout = timeout;
      setName(timeout.getName());
      setActivity(timeout.getActivity());
      m_cleanupAssocList = CLEANUP_ASSOC_LIST;
   }

   // operations
   
   /**
    * @return The associated timeout.
    */
   public Timeout getTimeout()
   {
      return m_timeout;
   }

   /**
    * @see nexj.core.meta.workflow.Step#getCleanupOrdinal()
    */
   public int getCleanupOrdinal()
   {
      return m_timeout.m_nOrdinal;
   }

   /**
    * @see nexj.core.meta.workflow.Step#isDependent()
    */
   public boolean isDependent()
   {
      return true;
   }

   /**
    * @see nexj.core.meta.workflow.Step#isPersistent()
    */
   public boolean isPersistent()
   {
      return true;
   }

   /**
    * @see nexj.core.meta.workflow.Step#step(nexj.core.meta.workflow.State)
    */
   public PCodeFunction step(State state)
   {
      state.remove(this, false);
      state.add(m_next);

      return null;
   }
}
