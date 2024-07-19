// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

/**
 * Interface implemented by objects containing an instance.
 */
public interface InstanceHolder
{
   /**
    * @return The contained instance. Can be null.
    */
   Instance getInstance();
}
