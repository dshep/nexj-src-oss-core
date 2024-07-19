// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

/**
 * Interface implemented by components that have the invocation context.
 */
public interface InvocationContextHolder
{
   /**
    * @return The invocation context.
    */
   InvocationContext getInvocationContext();
}
