// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.List;

/**
 * Combined Holder/List interface. (An ordered set)
 */
public interface HolderList extends Holder, List
{
   /**
    * Minimize the storage of this instance by trimming excess storage capacity to the
    * count of elements actually in this collection.
    */
   public void trimToSize();
}
