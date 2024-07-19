// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

/**
 * Interface declaring that this object contains a "top level" PCodeFunction.
 */
public interface PCodeHolder
{
   /**
    * @return The function. May be null.
    */
   public PCodeFunction getPCode();
}
