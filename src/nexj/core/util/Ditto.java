// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Interface implemented by object that can
 * clone themselves without their contents.
 */
public interface Ditto
{
   /**
    * @return A new initialized instance of the same class.
    */
   Object ditto();
}
