// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql.pseudoxa;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;

import javax.sql.ConnectionEvent;
import javax.sql.ConnectionEventListener;

/**
 * An object imitating an XAConnection
 */
public class XAConnection implements javax.sql.XAConnection
{
   /**
    * The wrapped connection.
    */
   protected Connection m_connection;

   /**
    * List of listeners for XA connection events.
    */
   protected ArrayList/*<ConnectionEventListener>*/ m_listenerList =
      new ArrayList/*<ConnectionEventListener>*/(1); // usually at least the connection manager

   /**
    * The XAResource associated with this XAConnection (lazy init).
    */
   protected XAResource m_resource;

   /**
    * Constructor.
    * @param connection The wrapped connection.
    */
   public XAConnection(Connection connection)
   {
      assert connection != null;

      m_connection = connection;
   }

   /**
    * @see javax.sql.XAConnection#getXAResource()
    */
   public javax.transaction.xa.XAResource getXAResource() throws SQLException
   {
      if (m_resource == null)
      {
         m_resource = new XAResource(m_connection);
      }

      return m_resource;
   }

   /**
    * @see javax.sql.PooledConnection#addConnectionEventListener(javax.sql.ConnectionEventListener)
    */
   public void addConnectionEventListener(ConnectionEventListener listener)
   {
      assert !m_listenerList.contains(listener);

      m_listenerList.add(listener);
   }

   /**
    * @see javax.sql.PooledConnection#close()
    */
   public void close() throws SQLException
   {
      ConnectionEvent ev = new ConnectionEvent(this);

      try
      {
         m_connection.close();
      }
      finally
      {
         RuntimeException ex = null;

         // notify listeners
         for (int i = 0, nCount = m_listenerList.size(); i < nCount;)
         {
            try
            {
               ((ConnectionEventListener)m_listenerList.get(i)).connectionClosed(ev);
            }
            catch (RuntimeException e)
            {
               if (ex == null)
               {
                  ex = e; // remember first error
               }
            }
         }

         if (ex != null)
         {
            throw ex;
         }
      }
   }

   /**
    * @see javax.sql.PooledConnection#getConnection()
    */
   public Connection getConnection() throws SQLException
   {
      return m_connection;
   }

   /**
    * @see javax.sql.PooledConnection#removeConnectionEventListener(javax.sql.ConnectionEventListener)
    */
   public void removeConnectionEventListener(ConnectionEventListener listener)
   {
      m_listenerList.remove(listener);
   }
}