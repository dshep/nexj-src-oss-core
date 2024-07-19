// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Implementors may be suspended and resumed.
 */
public interface Suspendable
{
   /**
    * Suspends.
    * @throws IllegalStateException If already suspended.
    */
   public void suspend() throws Exception;

   /**
    * Resumes.
    * @throws IllegalStateException If not suspended.
    */
   public void resume() throws Exception;
}
