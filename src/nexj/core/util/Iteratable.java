// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Iterator;

/**
 * Interface implemented by object having a common iterator.
 */
public interface Iteratable
{
   /**
    * @return The iterator.
    */
   Iterator iterator();
}
