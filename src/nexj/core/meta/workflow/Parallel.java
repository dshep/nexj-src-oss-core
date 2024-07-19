// Copyright 2012 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import nexj.core.scripting.Pair;

/**
 * Parallel activity.
 */
public class Parallel extends Concurrent
{
   // attributes

   /**
    * The name of the variable that stores the parallel argument.
    */
   protected String m_sVariable;

   // associations

   /**
    * Scheme expression evaluating to a list of arguments,
    * one for each of the threads invoked for this concurrent activity.
    */
   protected Pair m_args;

   // constructors

   /**
    * Construct a Parallel object.
    * @param fork The fork element.
    * @param sVariable The name of the variable that stores the parallel argument.
    * @param args The list of thread arguments.
    */
   public Parallel(Fork fork, String sVariable, Pair args)
   {
      super(fork);

      m_sVariable = sVariable;
      m_args = args;
   }

   // operations

   /**
    * Sets the name of the variable that stores the parallel argument.
    * @param sVariable The name of the variable that stores the parallel argument.
    */
   public void setVariable(String sVariable)
   {
      m_sVariable = sVariable;
   }

   /**
    * Gets the name of the variable that stores the parallel argument.
    * @return The name of the variable that stores the parallel argument.
    */
   public String getVariable()
   {
      return m_sVariable;
   }

   /**
    * Sets the scheme expression evaluating to a list of arguments,
    * one for each of the threads invoked for this concurrent activity.
    * @param args The argument list.
    */
   public void setArgs(Pair args)
   {
      m_args = args;
   }

   /**
    * Gets the scheme expression evaluating to a list of arguments,
    * one for each of the threads invoked for this concurrent activity.
    * @return The argument list.
    */
   public Pair getArgs()
   {
      return m_args;
   }
}
