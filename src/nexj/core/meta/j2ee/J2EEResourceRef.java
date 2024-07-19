// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.j2ee;

import java.sql.Connection;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.util.EmptyIterator;

/**
 * J2EE descriptor resource reference.
 */
public class J2EEResourceRef extends J2EEEnvRef
{
   // constants

   /**
    * No transaction support.
    */
   public final static byte TX_NONE = 0;

   /**
    * Local transaction supported.
    */
   public final static byte TX_LOCAL = 1;

   /**
    * XA transactions supported.
    */
   public final static byte TX_XA = 2;
   
   // attributes

   /**
    * The authentication alias.
    */
   protected String m_sAuthAlias;

   /**
    * The resource adapter name.
    */
   protected String m_sResourceAdapterName;
   
   /**
    * The maximum size of the connection pool.
    */
   protected int m_nMaxConnections = 16;

   /**
    * The transaction isolation level, one of java.sql.Connection.TRANSACTION_* constants.
    */
   protected int m_nIsolationLevel = Connection.TRANSACTION_NONE;

   /**
    * The transaction mode, one of the TX_* constants.
    */
   protected byte m_nTxMode;

   /**
    * The shareable flag.
    */
   protected boolean m_bShareable = true;

   /**
    * The flag indicating whether the connection pool is partioned by connection request info.
    */
   protected boolean m_bConnectionPoolPartitioned;
   
   // associations

   /**
    * The connection property collection.
    */
   protected List m_propertyList; // of type J2EEProperty

   // constructors

   /**
    * Constructs the resource reference.
    * @param sName The resource reference name.
    * @param sJNDIName The resource JNDI name.
    * @param sClassName The resource class name.
    * @param sAuthAlias The authentication alias. Can be null.
    */
   public J2EEResourceRef(String sName, String sJNDIName, String sClassName, String sAuthAlias)
   {
      super(sName, sJNDIName, sClassName);
      m_sAuthAlias = sAuthAlias;
   }

   /**
    * Constructs the resource reference.
    * @param sName The resource reference name.
    * @param sJNDIName The resource JNDI name.
    * @param sClassName The resource class name.
    */
   public J2EEResourceRef(String sName, String sJNDIName, String sClassName)
   {
      super(sName, sJNDIName, sClassName);
   }

   // operations

   /**
    * Sets the resource adapter name.
    * @param sResourceAdapterName The resource adapter name to set.
    */
   public void setResourceAdapterName(String sResourceAdapterName)
   {
      m_sResourceAdapterName = sResourceAdapterName;
   }

   /**
    * @return The resource adapter name.
    */
   public String getResourceAdapterName()
   {
      return m_sResourceAdapterName;
   }

   /**
    * Sets the authentication alias.
    * @param sAuthAlias The authentication alias to set.
    */
   public void setAuthAlias(String sAuthAlias)
   {
      m_sAuthAlias = sAuthAlias;
   }

   /**
    * @return The authentication alias.
    */
   public String getAuthAlias()
   {
      return m_sAuthAlias;
   }

   /**
    * Sets the transaction isolation level.
    * @param nIsolationLevel The transaction isolation level to set.
    */
   public void setIsolationLevel(int nIsolationLevel)
   {
      m_nIsolationLevel = nIsolationLevel;
   }

   /**
    * @return The transaction isolation level.
    */
   public int getIsolationLevel()
   {
      return m_nIsolationLevel;
   }

   /**
    * Sets the transaction mode.
    * @param nTxMode The transaction mode to set.
    */
   public void setTxMode(byte nTxMode)
   {
      m_nTxMode = nTxMode;
   }

   /**
    * @return The transaction mode.
    */
   public byte getTxMode()
   {
      return m_nTxMode;
   }
   
   /**
    * Sets the shareable flag.
    * @param bShareable The shareable flag to set.
    */
   public void setShareable(boolean bShareable)
   {
      m_bShareable = bShareable;
   }

   /**
    * @return The shareable flag.
    */
   public boolean isShareable()
   {
      return m_bShareable;
   }

   /**
    * Sets the maximum size of the connection pool.
    * @param nMaxConnections The maximum size of the connection pool.
    */
   public void setMaxConnections(int nMaxConnections)
   {
      m_nMaxConnections = nMaxConnections;
   }

   /**
    * @return The maximum size of the connection pool.
    */
   public int getMaxConnections()
   {
      return m_nMaxConnections;
   }
   
   /**
    * Sets the flag indicating whether the connection pool is partioned by connection request info.
    * @param bConnectionPoolPartitioned The flag indicating whether the connection pool is partioned by connection request info to set.
    */
   public void setConnectionPoolPartitioned(boolean bConnectionPoolPartitioned)
   {
      m_bConnectionPoolPartitioned = bConnectionPoolPartitioned;
   }

   /**
    * @return The flag indicating whether the connection pool is partioned by connection request info.
    */
   public boolean isConnectionPoolPartitioned()
   {
      return m_bConnectionPoolPartitioned;
   }

   /**
    * Adds a new connection property to the resource reference.
    * @param property The connection property to add.
    */
   public void addProperty(J2EEProperty property)
   {
      if (m_propertyList == null)
      {
         m_propertyList = new ArrayList();
      }

      m_propertyList.add(property);
   }

   /**
    * Gets a connection property by ordinal number.
    * @param nOrdinal The connection property ordinal number (0-based).
    * @return The connection property object.
    */
   public J2EEProperty getProperty(int nOrdinal)
   {
      return (J2EEProperty) m_propertyList.get(nOrdinal);
   }

   /**
    * @return The connection property count.
    */
   public int getPropertyCount()
   {
      if (m_propertyList == null)
      {
         return 0;
      }

      return m_propertyList.size();
   }

   /**
    * @return An iterator for the contained connection property objects.
    */
   public Iterator getPropertyIterator()
   {
      if (m_propertyList == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_propertyList.iterator();
   }
}
