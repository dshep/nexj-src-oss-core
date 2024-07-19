// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.sql.Connection;
import java.sql.SQLException;

import nexj.core.runtime.Resource;
import nexj.core.runtime.ResourceManager;

/**
 * The SQL connection.
 */
public class SQLConnection extends Resource
{
   // associations
   
   /**
    * The contained connection.
    */
   protected Connection m_connection;

   /**
    * The SQL adapter.
    */
   protected SQLAdapter m_adapter;

   // constructors
   
   /**
    * Constructs the resource.
    * @param adapter The SQL adapter.
    * @param connection The SQL connection.
    */
   public SQLConnection(SQLAdapter adapter, Connection connection)
   {
      m_adapter = adapter;
      m_connection = connection;
   }

   // operations

   /**
    * @return The SQL connection.
    */
   public Connection getConnection()
   {
      return m_connection;
   }

   /**
    * @see nexj.core.runtime.Resource#getResourceManager()
    */
   public ResourceManager getResourceManager()
   {
      return m_adapter;
   }

   /**
    * @see nexj.core.runtime.Resource#isReleased()
    */
   public boolean isReleased()
   {
      if (m_connection != null)
      {
         try
         {
            if (!m_connection.isClosed())
            {
               return false; // still have a valid connection
            }
         }
         catch (SQLException e)
         {
         }

         release(); // the connection is not valid
      }

      return true;
   }

   /**
    * @see nexj.core.runtime.Resource#isShareableDefault()
    */
   protected boolean isShareableDefault()
   {
      return true;
   }

   /**
    * @see nexj.core.runtime.Resource#release()
    */
   public void release()
   {
      if (m_connection != null)
      {
         m_adapter.close(m_connection);
         m_connection = null;
      }
   }
}
