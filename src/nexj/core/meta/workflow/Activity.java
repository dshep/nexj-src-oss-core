// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataObject;
import nexj.core.scripting.PCodeFunction;

/**
 * Container for flow elements.
 */
public abstract class Activity extends MetadataObject
{
   // attributes
   
   /**
    * The token name.
    */
   protected String m_sTokenName;

   /**
    * The layout coordinates for this workflow activity.
    */
   protected String m_sLayout;

   // associations
   
   /**
    * The activity step list: Step[].
    */
   protected List m_stepList = new ArrayList(4);

   // operations
   
   /**
    * @return The flow containing the activity.
    */
   public abstract Flow getFlow();

   /**
    * @return The parent activity.
    */
   public abstract Activity getParent();
   
   /**
    * @return The container step.
    */
   public abstract Step getContainer();
   
   /**
    * @return The containing fork step.
    */
   public abstract Fork getFork();

   /**
    * @return The exception handler.
    */
   public TryCatch getTryCatch()
   {
      Activity parent = getParent();

      if (parent != null)
      {
         return parent.getTryCatch();
      }

      return null;
   }
   
   /**
    * Sets the token name.
    * @param sTokenName The token name to set.
    */
   public void setTokenName(String sTokenName)
   {
      verifyNotReadOnly();
      m_sTokenName = sTokenName;
   }

   /**
    * @return The token name.
    */
   public String getTokenName()
   {
      return m_sTokenName;
   }
   
   /**
    * Adds a step to the activity.
    * @param step The step to add.
    * @throws MetadataException if there is a duplicate step.
    */
   public void addStep(Step step) throws MetadataException
   {
      if (step.getOrdinal() < 0)
      {
         getFlow().addFlowStep(step);
      }

      m_stepList.add(step);
      step.setActivity(this);
   }

   /**
    * Adds a step to the activity.
    * @param nOrdinal The position, in which to add the step.
    * @param step The step to add.
    * @throws MetadataException if there is a duplicate step.
    */
   public void addStep(int nOrdinal, Step step) throws MetadataException
   {
      getFlow().addFlowStep(step);
      m_stepList.add(nOrdinal, step);
      step.setActivity(this);
   }

   /**
    * Gets a step by the ordinal number.
    * @param nOrdinal The ordinal number.
    * @return The step.
    */
   public Step getStep(int nOrdinal)
   {
      return (Step)m_stepList.get(nOrdinal);
   }
   
   /**
    * @return The first step, or null if none.
    */
   public Step getFirstStep()
   {
      if (m_stepList.size() == 0)
      {
         return null;
      }
      
      return getStep(0);
   }

   /**
    * @return The count of all the steps in the activity.
    */
   public int getStepCount()
   {
      return m_stepList.size();
   }
   
   /**
    * @return The step iterator.
    */
   public Iterator getStepIterator()
   {
      return m_stepList.iterator();
   }

   /**
    * Sets the layout coordinates string.
    * @param sLayout The layout coordinate string
    */
   public void setLayout(String sLayout)
   {
      verifyNotReadOnly();
      m_sLayout = sLayout;
   }

   /**
    * Gets the layout coordinates string.
    * @return The layout
    */
   public String getLayout()
   {
      return m_sLayout;
   }

   /**
    * Determines if a given activity is contained, directly or indirectly, in this one.
    * @param activity the activity to check.
    * @return True if it is contained.
    */
   public boolean contains(Activity activity)
   {
      while (activity != null && activity != this)
      {
         activity = activity.getParent();
      }

      return activity != null;
   }

   /**
    * @return The root concurrent activity.
    */
   public Activity getRootConcurrent()
   {
      Fork fork = getFork();
      
      if (fork == null)
      {
         return getFlow();
      }

      Activity activity = this;

      while (activity.getParent() != null && activity.getParent().getFork() == fork)
      {
         activity = activity.getParent();
      }

      return activity;
   }
   
   /**
    * Visits all the steps in the activity.
    * @param visitor The visitor object.
    */
   public void visit(Step.Visitor visitor)
   {
      for (int i = 0, nCount = m_stepList.size(); i < nCount; ++i)
      {
         ((Step)m_stepList.get(i)).visit(visitor);
      }
   }
   
   /**
    * Visits this activity.
    * @param visitor The visitor object.
    */
   public void visit(Activity.Visitor visitor)
   {
      visitor.visit(this);
      
      for (int i = 0, nCount = m_stepList.size(); i < nCount; ++i)
      {
         ((Step)m_stepList.get(i)).visit(visitor);
      }
   }

   /**
    * Returns a function to execute when execution leaves this Activity.
    * 
    * @param state The current state.
    * @return The function to execute; null to do nothing.
    */
   public PCodeFunction cleanup(State state)
   {
      return null;
   }

   /**
    * @return True if this Activity should be cleaned up.
    */
   public boolean isCleanupRequired()
   {
      return false;
   }

   // inner classes

   /**
    * Activity visitor interface.
    */
   public interface Visitor
   {
      /**
       * Visits an activity.
       * @param activity The activity to visit.
       */
      void visit(Activity activity);
   }
}
