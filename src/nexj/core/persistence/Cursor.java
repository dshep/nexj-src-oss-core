// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceList;
import nexj.core.util.Lookup;

/**
 * Cursor interface for gradual retrieval of a set of persisted instances.
 */
public interface Cursor
{
   /**
    * Releases the resources allocated by the cursor.
    */
   void close();

   /**
    * Retrieves the next instance.
    * @return The next instance or null if EOF has been encountered.
    */
   Instance next();

   /**
    * Retrieves the next page of instances.
    * @param nMaxCount The maximum number of instances to retrieve.
    * @return The retrieved instance list.
    */
   InstanceList next(int nMaxCount);
   
   /**
    * Retrieves one branch of instances.
    * @param map The query-instance output map.
    * @return True if the step has been made.
    */
   boolean step(Lookup map);
}
