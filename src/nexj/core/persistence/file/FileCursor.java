// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.file;

import nexj.core.persistence.Cursor;
import nexj.core.persistence.Query;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceArrayList;
import nexj.core.runtime.InstanceList;
import nexj.core.util.Lookup;

/**
 * A cursor for navigating result sets retrieved by the file persistence adapter.
 * 
 * Since the file persistence adapter can only return zero or one instances to
 * a read query, this cursor's first next() invocation will return the single
 * result (or null if no result) and all subsequent invocations will return null.
 */
public class FileCursor implements Cursor
{
   // attributes

   /**
    * True if the cursor has been advanced past the end of the results.
    */
   protected boolean m_bDone;


   // associations

   /**
    * The file adapter to use for reading results.
    */
   protected FileAdapter m_adapter;

   /**
    * The query being executed by this cursor.
    */
   protected Query m_query;


   // constructors

   public FileCursor(FileAdapter adapter, Query query)
   {
      m_adapter = adapter;
      m_query = query;
   }


   // operations

   /**
    * @see nexj.core.persistence.Cursor#close()
    */
   public void close()
   {
      m_bDone = true;
      m_adapter = null;
      m_query = null;
   }


   /**
    * Gets the next result.
    * 
    * @return The instance; null if past EOF or query matches nothing.
    * @see nexj.core.persistence.Cursor#next()
    */
   public Instance next()
   {
      if (m_bDone)
      {
         return null;
      }
      
      Instance result = null;
      InstanceList resultList = m_adapter.read(m_query);
      
      m_bDone = true;
      
      if (resultList != null && resultList.size() == 1)
      {
         result = (Instance)resultList.get(0);
      }
      
      return result;
   }


   /**
    * Gets the next group of results.
    * 
    * @return List of instances; empty list if past EOF, query matches nothing,
    *         or nMaxCount is less/equal 0.
    * @see nexj.core.persistence.Cursor#next(int)
    */
   public InstanceList next(int nMaxCount)
   {
      if (m_bDone || nMaxCount <= 0)
      {
         return new InstanceArrayList(0);
      }
      
      InstanceList resultList = m_adapter.read(m_query);
      
      m_bDone = true;
      
      if (resultList != null && resultList.size() == 1)
      {
         return resultList;
      }
      
      return new InstanceArrayList(0);
   }


   /**
    * @see nexj.core.persistence.Cursor#step(nexj.core.util.Lookup)
    */
   public boolean step(Lookup map)
   {
      // The first step always takes us past EOF
      m_bDone = true;
      return false;
   }
}
