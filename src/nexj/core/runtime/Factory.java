// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

/**
 * Generic factory interface.
 */
public interface Factory
{
   /**
    * Creates the object.
    */
   public Object create() throws Exception;
}
