// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

/**
 * Interface implemented by components that should be
 * notified that their initialization is complete.
 */
public interface Initializable
{
   /**
    * This method is invoked after all the properties
    * have been assigned to a component, including the
    * invocation context.
    */
   void initialize() throws Exception;
}
