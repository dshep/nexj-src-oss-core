// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Interface for managing the life cycle of an object.
 */
public interface Lifecycle extends Suspendable
{
   /**
    * Starts the object up.
    */
   void startup() throws Exception;
   
   /**
    * Shuts the object down.
    */
   void shutdown();
}
