// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

/**
 * Delayed message output interface.
 */
public interface DelayedOutput extends Output
{
   /**
    * Set the delayed action.
    * @param action The delayed action.
    */
   void setAction(Action action);

   /**
    * Action interface.
    */
   interface Action
   {
      /**
       * Run this action with the given output.
       * @param output The Output object.
       */
      void run(Output output);
   }
}
