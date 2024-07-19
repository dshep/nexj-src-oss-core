// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import java.io.File;
import java.io.IOException;

import javax.resource.ResourceException;
import javax.resource.spi.ConnectionManager;
import javax.resource.spi.ConnectionRequestInfo;
import javax.resource.spi.ManagedConnection;
import javax.security.auth.Subject;

import nexj.core.rpc.ra.GenericManagedConnectionFactory;
import nexj.core.util.ObjUtil;

/**
 * A factory for creating FileManagedConnection objects. This class is a Bean and
 * its configuration properties are set by the app server.
 */
public class FileManagedConnectionFactory extends GenericManagedConnectionFactory
{
   // constants

   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = -1585485957895541023L;


   // attributes

   /**
    * A flag indicating that this factory is for input connections. If it is
    * false, then this factory creates output connections.
    */
   protected boolean m_bInputConnection;

   /**
    * A flag indicating that this connection is a file persistence connection.
    * If false, then this connection is a file message connection.
    */
   protected boolean m_bPersistenceConnection = false;

   /**
    * The number of characters in each split path component.
    */
   protected int m_nNameSplitSize;

   /**
    * The maximum number of name splits to perform on a file name.
    */
   protected int m_nMaxNameSplits;

   /**
    * The name of the journal for this connection.
    */
   protected String m_sJournalName;


   // associations

   /**
    * The directory to scan for incoming message files.
    */
   protected File m_incomingDirectory;

   /**
    * The directory in which to place outgoing message files.
    */
   protected File m_outgoingDirectory;

   /**
    * The directory to use for preparing outgoing messages.
    */
   protected File m_outgoingTempDirectory;

   /**
    * The directory in which to place incoming message files after they
    * have been processed. If null, the files will be deleted after they
    * are processed.
    */
   protected File m_processedDirectory;

   /**
    * The directory to use for the transactional journal for this adapter. In
    * a clustered configuration, the transactional journal directory should be
    * unique to each node. If it is a relative path, it will be modified to use
    * the server data directory as root.
    */
   protected File m_journalDirectory;


   // operations

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnectionFactory#getDefaultConnectionManager()
    */
   protected ConnectionManager getDefaultConnectionManager()
   {
      return FileResourceAdapter.getDefaultConnectionManager();
   }


   /**
    * @see javax.resource.spi.ManagedConnectionFactory#createConnectionFactory(javax.resource.spi.ConnectionManager)
    */
   public Object createConnectionFactory(ConnectionManager manager) throws ResourceException
   {
      return new FileConnectionFactory(this, manager);
   }


   /**
    * @see javax.resource.spi.ManagedConnectionFactory#createManagedConnection(javax.security.auth.Subject, javax.resource.spi.ConnectionRequestInfo)
    */
   public ManagedConnection createManagedConnection(Subject subject, ConnectionRequestInfo cri) throws ResourceException
   {
      if (!m_journalDirectory.isAbsolute())
      {
         try
         {
            m_journalDirectory = new File(
               ((FileResourceAdapter)m_adapter).getPlatformConfig().createDataDirectory(),
               m_journalDirectory.getPath()
            );
         }
         catch (IOException e)
         {
            throw new ResourceException(e);
         }
      }
      
      FileManagedConnection result = new FileManagedConnection(m_journalDirectory);
      
      if (!m_bPersistenceConnection)
      {
         if (m_bInputConnection)
         {
            result.setPrimaryDirectory(m_incomingDirectory);
            result.setSecondaryDirectory(m_processedDirectory);
         }
         else
         {
            result.setPrimaryDirectory(m_outgoingTempDirectory);
            result.setSecondaryDirectory(m_outgoingDirectory);
         }
      }
      else
      {
         result.setPrimaryDirectory(m_outgoingTempDirectory);
         result.setSecondaryDirectory(m_outgoingDirectory);
      }
      
      result.setInputConnection(m_bInputConnection);
      result.setPersistenceConnection(m_bPersistenceConnection);
      
      result.setNameSplitSize(m_nNameSplitSize);
      result.setMaxNameSplits(m_nMaxNameSplits);

      return result;
   }


   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (!(obj instanceof FileManagedConnectionFactory))
      {
         return false;
      }
      
      FileManagedConnectionFactory other = (FileManagedConnectionFactory)obj;
      
      if (m_bPersistenceConnection != other.m_bPersistenceConnection)
      {
         return false;
      }
      
      if (!m_bPersistenceConnection)
      {
         if (m_bInputConnection != other.m_bInputConnection)
         {
            return false;
         }

         if (m_bInputConnection)
         {
            return ObjUtil.equal(m_incomingDirectory, other.m_incomingDirectory);
         }
         else
         {
            return ObjUtil.equal(m_outgoingDirectory, other.m_outgoingDirectory);
         }
      }
      else
      {
         return ObjUtil.equal(m_outgoingDirectory, other.m_outgoingDirectory);
      }
   }


   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      if (!m_bPersistenceConnection)
      {
         if (m_bInputConnection)
         {
            return m_incomingDirectory.hashCode();
         }
         else
         {
            return m_outgoingDirectory.hashCode();
         }
      }
      else
      {
         return m_outgoingDirectory.hashCode();
      }
   }


   /**
    * Gets the directory to scan for incoming message files.
    * 
    * @return The name of the directory to scan for incoming message files.
    */
   public String getIncomingDirectory()
   {
      return (m_incomingDirectory == null) ? null : m_incomingDirectory.getPath();
   }


   /**
    * Sets the directory to scan for incoming message files.
    * 
    * @param sName The name of directory to scan for incoming message files.
    */
   public void setIncomingDirectory(String sName)
   {
      m_incomingDirectory = (sName == null) ? null : new File(sName);
   }


   /**
    * Gets the directory in which to place outgoing message files.
    * 
    * @return The name of the directory in which outgoing message files should be placed.
    */
   public String getOutgoingDirectory()
   {
      return (m_outgoingDirectory == null) ? null : m_outgoingDirectory.getPath();
   }


   /**
    * Sets the directory in which to place outgoing message files.
    * 
    * @param sName The name of the directory in which outgoing message files should be placed.
    */
   public void setOutgoingDirectory(String sName)
   {
      m_outgoingDirectory = (sName == null) ? null : new File(sName);
   }


   /**
    * Gets the directory to use for preparing outgoing messages. Should be on the
    * same volume as the outgoing directory.
    * 
    * @return The name of the directory to use for preparing outgoing messages.
    */
   public String getOutgoingTempDirectory()
   {
      return (m_outgoingTempDirectory == null) ? null : m_outgoingDirectory.getPath();
   }


   /**
    * Gets the directory to use for preparing outgoing messages. Should be on the
    * same volume as the outgoing directory.
    * 
    * @param sName The name of the directory to use for preparing outgoing messages.
    */
   public void setOutgoingTempDirectory(String sName)
   {
      m_outgoingTempDirectory = (sName == null) ? null : new File(sName);
   }


   /**
    * Gets the directory in which to place incoming message files after they
    * have been processed.
    * 
    * @return The name of the directory in which incoming message files should be placed
    *         after they have been processed; null to delete incoming messages
    *         when done processing.
    */
   public String getProcessedDirectory()
   {
      return (m_processedDirectory == null) ? null : m_processedDirectory.getPath();
   }


   /**
    * Sets the directory in which to place incoming message files after they
    * have been processed.
    * 
    * @param sName The name of the directory in which incoming message files should be
    *              placed after they have been processed; null to delete
    *              incoming messages when done processing.
    */
   public void setProcessedDirectory(String sName)
   {
      m_processedDirectory = (sName == null) ? null : new File(sName);
   }


   /**
    * Gets the directory to use for the transactional journal for this adapter. In
    * a clustered configuration, the transactional journal should be unique
    * to each machine in the cluster.
    * 
    * @return The transactional journal file name.
    */
   public String getJournalDirectory()
   {
      return (m_journalDirectory == null) ? null : m_journalDirectory.getPath();
   }


   /**
    * Sets the directory to use for the transactional journal for this adapter. In
    * a clustered configuration, the transactional journal should be unique to each
    * machine in the cluster.
    * 
    * @param sName The transactional journal file name.
    */
   public void setJournalDirectory(String sName)
   {
      m_journalDirectory = (sName == null) ? null : new File(sName);
   }


   /**
    * Gets a flag indicating that this factory is for input connections. If it is
    * false, then this factory creates output connections.
    * 
    * @return True if this factory creates input connections; false if it creates
    *         output connections.
    */
   public boolean isInputConnection()
   {
      return m_bInputConnection;
   }


   /**
    * Sets a flag indicating that this factory is for input connections. If it is
    * false, then this factory creates output connections.
    * 
    * @param bInputConnection True to make this factory create input connections;
    *                         false to make this factory create output connections.
    */
   public void setInputConnection(boolean bInputConnection)
   {
      m_bInputConnection = bInputConnection;
   }


   /**
    * Gets a flag indicating that this factory is for file persistence connections.
    * If it is false, then this factory creates message connections.
    * 
    * @return True if this factory creates file persistence connecctions; false if
    *         it creates message connections.
    */
   public boolean isPersistenceConnection()
   {
      return m_bPersistenceConnection;
   }


   /**
    * Sets a flag indicating that this factoory is for file persistence connections.
    * If it is false, then this factory creates message connections.
    * 
    * @param bPersistenceConnection True to make this factory create file persistence
    *                               connections; false to make this factory create
    *                               message connections.
    */
   public void setPersistenceConnection(boolean bPersistenceConnection)
   {
      m_bPersistenceConnection = bPersistenceConnection;
   }


   /**
    * Sets the maximum number of splits to perform on a file name.
    * 
    * @param nMaxSplits The maximum number of splits to perform.
    */
   public void setMaxNameSplits(int nMaxSplits)
   {
      m_nMaxNameSplits = nMaxSplits;
   }


   /**
    * Gets the maximum number of splits to perform on a file name.
    * 
    * @return The maximum number of splits to perform.
    */
   public int getMaxNameSplits()
   {
      return m_nMaxNameSplits;
   }


   /**
    * Sets the number of characters in each split path component.
    * 
    * @param nSplitSize The number of characters in each component.
    */
   public void setNameSplitSize(int nSplitSize)
   {
      m_nNameSplitSize = nSplitSize;
   }


   /**
    * Gets the number of characters in each split path component.
    * 
    * @return The number of characters in each component.
    */
   public int getNameSplitSize()
   {
      return m_nNameSplitSize;
   }


   /**
    * Sets the name to use for the journal, as a relative pathname.
    * 
    * @param sJournalName The relative path to the journal in host OS format.
    */
   public void setJournalName(String sJournalName)
   {
      m_sJournalName = sJournalName;
   }


   /**
    * Gets the name to use for the journal, as a relative pathname.
    * 
    * @return The relative path to the journal in host OS format.
    */
   public String getJournalName()
   {
      return m_sJournalName;
   }
}
