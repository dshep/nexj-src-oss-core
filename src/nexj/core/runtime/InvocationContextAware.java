// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

/**
 * Interface implemented by components that should
 * receive the invocation context.
 */
public interface InvocationContextAware
{
   /**
    * This method is invoked after all properties have been
    * assigned to a component and before its initializer
    * method is invoked.
    * @param context The invocation context to set.
    * Can be null to indicate no invocation context.
    */
   void setInvocationContext(InvocationContext context);
}
