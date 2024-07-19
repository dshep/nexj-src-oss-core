// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Comparator;

/**
 * Interface implemented by objects that can be sorted in place.
 */
public interface Sortable
{
   /**
    * Sorts the object using an item comparator.
    * @param cmp The comparator to use.
    */
   void sort(Comparator cmp);
}
