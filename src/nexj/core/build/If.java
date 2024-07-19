// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.build;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.taskdefs.Sequential;
import org.apache.tools.ant.taskdefs.condition.Condition;
import org.apache.tools.ant.taskdefs.condition.ConditionBase;

/**
 * An If task that allows execution of nested tasks in Then or Else child elements,
 * depending on how a few available conditions are evaluated.
 */
public class If extends Task
{
   /**
    * The container of tasks to execute for the Then evaluation of the conditions.
    */
   private Sequential m_ifThen;

   /**
    * The container of tasks to execute for the Else evaluation of the conditions.
    */
   private Sequential m_ifElse;

   /**
    * The NestedCondition task to evaluate as part of the condition for this task.
    */
   private NestedCondition m_nestedCondition;

   /**
    * The String that contains the property name whose presence would cause
    * the equivalent of a <code>true</code> evaluation of the If task condition
    * (i.e. whose absence would cause the equivalent of a <code>false</code>
    * evaluation of the If task condition).
    */
   private String m_sIfProperty;

   /**
    * The String that contains the property name whose absence would cause
    * the equivalent of a <code>true</code> evaluation of the If task condition
    * (i.e. whose presence would cause the equivalent of a <code>false</code>
    * evaluation of the If task condition).
    */
   private String m_sUnlessProperty;

   /**
    * Set a property whose existence is the equivalent to a true
    * evaluation of an if-statement's condition.
    *
    * @param sUnlessProperty The property whose existence will cause the execution of the nested Then tasks.
    */
   public void setIf(String sIfProperty)
   {
      m_sIfProperty = sIfProperty;
   }

   /**
    * Set a property whose existence is the equivalent to a false
    * evaluation of an if-statement's condition.
    *
    * @param sUnlessProperty The property whose existence will cause the execution of the nested Else tasks.
    */
   public void setUnless(String sUnlessProperty)
   {
      m_sUnlessProperty = sUnlessProperty;
   }

   /**
    * Add a condition element.
    * @return <code>ConditionBase</code> for the user to define a condition as part
    * of the if-then-else task.
    */
   public ConditionBase createCondition() 
   {
       if (m_nestedCondition != null) 
       {
           throw new BuildException("Only one nested condition is allowed");
       }

       m_nestedCondition = new NestedCondition();

       return m_nestedCondition;
   }

   /**
    * Returns a nested Sequential element for the Then part of the
    * if-then-else task.
    *
    * @return The Sequential element for the user to add if-then Tasks to.
    */
   public Sequential createThen()
   {
      if (m_ifThen != null)
      {
         throw new BuildException("Cannot have two nested Then elements");
      }

      m_ifThen = new Sequential();

      return m_ifThen;
   }

   /**
    * Returns a nested Sequential element for the Else part of the
    * if-then-else task.
    *
    * @return The Sequential element for the user to add if-else Tasks to.
    */
   public Sequential createElse()
   {
      if (m_ifElse != null)
      {
         throw new BuildException("Cannot have two nested Else elements");
      }

      m_ifElse = new Sequential();

      return m_ifElse;
   }

   /**
    * Check if all the mandatory Ant Task attribute combinations are set.
    */
   private void checkAttributes()
   {
      if (m_nestedCondition == null && m_sIfProperty == null 
         && m_sUnlessProperty == null)
      {
         throw new BuildException("Missing nested Condition element or an &quot;if&quot; or &quot;unless&quot; attribute");
      }
   }
   
   /**
    * @see org.apache.tools.ant.Task#execute()
    */
   public void execute() throws BuildException
   {
      checkAttributes();

      boolean bCondition = true;

      if (m_nestedCondition != null)
      {
         bCondition &= m_nestedCondition.eval();
      }

      if (m_sIfProperty != null)
      {
         bCondition &= getProject().getProperties().containsKey(m_sIfProperty);
      }

      if (m_sUnlessProperty != null)
      {
         bCondition &= (!getProject().getProperties().containsKey(m_sUnlessProperty));
      }

      if (bCondition)
      {
         if (m_ifThen != null)
         {
            m_ifThen.execute();
         }
      }
      else
      {
         if (m_ifElse != null)
         {
            m_ifElse.execute();
         }
      }
   }

   /**
    * @see org.apache.tools.ant.taskdefs.Exit.NestedCondition
    */
   private static class NestedCondition extends ConditionBase implements Condition 
   {

      public boolean eval()
      {
         if (countConditions() != 1)
         {
            throw new BuildException("A single nested condition is required");
         }

         return ((Condition)(getConditions().nextElement())).eval();
      }
   }

}
