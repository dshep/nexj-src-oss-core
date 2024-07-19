// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.workflow.State.ThreadInfo;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Flow fork. 
 */
public class Fork extends Step
{
   // attributes

   /**
    * True if the concurrent activities should be executed in parallel.
    */
   protected boolean m_bParallel;

   // associations
   
   /**
    * The concurrent activity collection.
    */
   protected List m_concurrentList = new ArrayList(4); // of type Concurrent

   /**
    * The state management function.
    */
   protected PCodeFunction m_function;

   // constructors
   
   /**
    * Constructs the fork.
    * @param sName The fork name.
    */
   public Fork(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the fork. 
    */
   public Fork()
   {
      super();
   }
   
   // operations
   
   /**
    * Adds a new concurrent activity to the fork.
    * @param concurrent The concurrent activity to add.
    */
   public void addConcurrent(Concurrent concurrent)
   {
      verifyNotReadOnly();
      m_concurrentList.add(concurrent);
      concurrent.setFork(this);
   }

   /**
    * Gets a concurrent activity by ordinal number.
    * @param nOrdinal The concurrent activity ordinal number (0-based).
    * @return The concurrent activity object.
    */
   public Concurrent getConcurrent(int nOrdinal)
   {
      return (Concurrent)m_concurrentList.get(nOrdinal);
   }

   /**
    * @return The concurrent activity count.
    */
   public int getConcurrentCount()
   {
      return m_concurrentList.size();
   }

   /**
    * @return An iterator for the contained concurrent activity objects.
    */
   public Iterator getConcurrentIterator()
   {
      return m_concurrentList.iterator();
   }

   /**
    * Get whether parallel mode is enabled.
    * @return True iff parallel mode is enabled.
    */
   public boolean isParallel()
   {
      return m_bParallel;
   }

   /**
    * Set the parallel mode.
    * @param bParallel True iff parallel mode should be enabled.
    */
   public void setParallel(boolean bParallel)
   {
      m_bParallel = bParallel;
   }

   /**
    * @see nexj.core.meta.workflow.Step#generate(nexj.core.scripting.Machine)
    */
   public void generate(Machine machine)
   {
      if (m_bParallel)
      {
         verifyNotReadOnly();

         Pair parallelArgs = null;

         for (int i = getConcurrentCount() - 1; i >= 0; --i)
         {
            parallelArgs = new Pair(((Parallel)getConcurrent(i)).getArgs(), parallelArgs);
         }

         Pair body = Pair.list(
            Pair.list(Symbol.define(getActivity().getFlow().getSysMetaclass()), Pair.quote(Symbol.define("invokeParallel")),
               Symbol.define(":state"), getName(), getActivity().getFlow().getName(),
               new Integer(getActivity().getFlow().getVersion()), new Pair(Symbol.LIST, parallelArgs)
            ), Symbol.THIS
         );

         if (m_activity.getFlow().isPrivileged())
         {
            body = Pair.list(new Pair(Symbol.BEGIN_PRIVILEGED, body));
         }

         m_function = compile(ARGUMENTS, body, machine);
      }
   }

   /**
    * @see nexj.core.meta.workflow.Step#step(nexj.core.meta.workflow.State)
    */
   public PCodeFunction step(State state)
   {
      state.remove(this);

      boolean bAdded = false;
      PCodeFunction function = null;

      if (m_bParallel)
      {
         ThreadInfo threadInfo = state.getThreadInfo(getName());

         if (threadInfo != null)
         {
            Parallel parallel = (Parallel)getConcurrent(threadInfo.getIndex());

            if (parallel.getFirstStep() != null)
            {
               state.add(parallel.getFirstStep());
               state.setToken(parallel, state.getToken(m_activity));

               if (parallel.getVariable() != null)
               {
                  state.setValue(parallel.getVariable(), threadInfo.getArgument());
               }

               bAdded = true;
            }
         }
         else
         {
            function = m_function;
         }
      }
      else
      {
         for (int i = 0, nCount = getConcurrentCount(); i < nCount; ++i)
         {
            Step step = getConcurrent(i).getFirstStep();
            
            if (step != null)
            {
               state.add(step);

               if (state.getToken(m_activity) != null)
               {
                  state.setToken(getConcurrent(i), state.getToken(m_activity));
               }

               bAdded = true;
            }
         }
      }

      if (!bAdded)
      {
         state.add(m_next);
      }

      return function;
   }

   /**
    * @see nexj.core.meta.workflow.Step#visit(nexj.core.meta.workflow.Step.Visitor)
    */
   public void visit(Visitor visitor)
   {
      super.visit(visitor);
      
      for (int i = 0, nCount = m_concurrentList.size(); i < nCount; ++i)
      {
         ((Concurrent)m_concurrentList.get(i)).visit(visitor);
      }
   }

   /**
    * @see nexj.core.meta.workflow.Step#visit(nexj.core.meta.workflow.Activity.Visitor)
    */
   public void visit(Activity.Visitor visitor)
   {
      super.visit(visitor);
      
      for (int i = 0, nCount = m_concurrentList.size(); i < nCount; ++i)
      {
         ((Concurrent)m_concurrentList.get(i)).visit(visitor);
      }
   }
}
