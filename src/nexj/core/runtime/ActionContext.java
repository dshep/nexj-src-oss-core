// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import nexj.core.meta.Action;

/**
 * Interface for accessing action arguments and invoking the next action.
 */
public interface ActionContext
{
   /**
    * @return The current action metadata.
    */
   Action getAction();
   
   /**
    * Sets the argument value.
    * @param nOrdinal The argument ordinal number, 0-based.
    * @param value The argument value. 
    */
   void setArg(int nOrdinal, Object value);

   /**
    * Gets the argument value.
    * @param nOrdinal The argument ordinal number.
    * @return The argument value.
    */
   Object getArg(int nOrdinal);

   /**
    * Invokes the next action.
    * @return The action return value.
    */
   Object callNext();
}
