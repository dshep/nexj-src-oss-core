// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Iterator;

/**
 * Property map iterator.
 */
public interface PropertyIterator extends Iterator
{
   /**
    * @return The property name retrieved by the last next() invocation. May be
    *         null if no String name defined.
    */
   String getName();

   /**
    * @return The value associated with the last next() invocation.
    */
   Object getValue();

   /**
    * Replaces the value associated with the last next() invocation.
    * @param value The value to set.
    */
   void setValue(Object value);
}
