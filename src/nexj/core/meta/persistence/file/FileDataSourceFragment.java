// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.file;

import nexj.core.meta.Component;
import nexj.core.meta.persistence.DataSourceFragment;
import nexj.core.util.SysUtil;

/**
 * The metadata object representing a file data source fragment. Each fragment
 * contains all of the configuration parameters necessary to establish a
 * "connection" to a different data directory.
 */
public class FileDataSourceFragment extends DataSourceFragment
{
   // attributes

   /**
    * The path to the root of the directory tree used for storing the data files.
    */
   protected String m_sDataDirectory;

   /**
    * The path to the temporary directory to be used for preparing data files
    * for persistence. (used to support transactional operations)
    */
   protected String m_sTemporaryDirectory;

   /**
    * The path to the transactional journal directory used by this adapter.
    * If this path is a relative path, then the resource adapter uses
    * the server node's data directory as the root.
    * Path should be specified in the host OS path format.
    */
   protected String m_sJournalPath;

   /**
    * The maximum number of name splits to perform on a file name.
    */
   protected int m_nMaxNameSplits = 3;

   /**
    * The number of characters in each split path component.
    */
   protected int m_nNameSplitSize = 2;
   
   /**
    * The maximum connection pool size.
    */
   protected int m_nMaxPoolSize = 33;


   // associations

   /**
    * The connection factory component.
    */
   protected Component m_component;


   // operations

   /**
    * Sets the root of the directory tree used for storing the data files.
    * 
    * @param sDataDirectory The full path to the root directory.
    */
   public void setDataDirectory(String sDataDirectory)
   {
      verifyNotReadOnly();
      m_sDataDirectory = sDataDirectory;
   }

   /**
    * Gets the root of the directory tree used for storing the data files.
    * 
    * @return The full path to the root directory.
    */
   public String getDataDirectory()
   {
      return m_sDataDirectory;
   }

   /**
    * Sets the temporary directory used to prepare data files for persistence.
    * 
    * @param sTemporaryDirectory The full path to the temporary directory.
    */
   public void setTemporaryDirectory(String sTemporaryDirectory)
   {
      verifyNotReadOnly();
      m_sTemporaryDirectory = sTemporaryDirectory;
   }

   /**
    * Gets the temporary directory used to prepare data files for persistence.
    * 
    * @return The full path to the temporary directory.
    */
   public String getTemporaryDirectory()
   {
      return m_sTemporaryDirectory;
   }

   /**
    * Sets the directory in which to store the transactional journal.
    * 
    * @param sJournalPath The full path to the journal directory.
    */
   public void setJournalPath(String sJournalPath)
   {
      verifyNotReadOnly();
      m_sJournalPath = sJournalPath;
   }

   /**
    * Gets the directory in which to store the transactional journal. If
    * it is null, then a default relative path is returned. This relative
    * path should be rooted at the server node's data directory.
    * 
    * @return The path to the journal directory.
    */
   public String getJournalPath()
   {
      if (m_sJournalPath != null)
      {
         return m_sJournalPath;
      }
      
      return "datasources" + SysUtil.FILE_SEP + m_dataSource.getName() + getSuffix();
   }

   /**
    * Sets the maximum number of splits to perform on a file name.
    * 
    * @see nexj.core.persistence.file.FileAdapter
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
    * @see nexj.core.persistence.file.FileAdapter 
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
    * @see nexj.core.persistence.file.FileAdapter
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
    * @see nexj.core.persistence.file.FileAdapter
    * 
    * @return The number of characters in each component.
    */
   public int getNameSplitSize()
   {
      return m_nNameSplitSize;
   }

   /**
    * Sets the maximum connection pool size.
    * @param nMaxPoolSize The maximum connection pool size to set.
    */
   public void setMaxPoolSize(int nMaxPoolSize)
   {
      verifyNotReadOnly();
      m_nMaxPoolSize = nMaxPoolSize;
   }

   /**
    * @return The maximum connection pool size.
    */
   public int getMaxPoolSize()
   {
      return m_nMaxPoolSize;
   }
   
   /**
    * Sets the connection factory component.
    * @param component The connection factory component to set.
    */
   public void setConnectionFactory(Component component)
   {
      verifyNotReadOnly();
      m_component = component;
   }

   /**
    * @return The connection factory component.
    */
   public Component getConnectionFactory()
   {
      return m_component;
   }

   /**
    * Sets the fragment configuration parameters without checking the read only
    * flag. FOR INTERNAL USE ONLY.
    * 
    * @param sDataDir The full path to the data directory.
    * @param sTempDir The full path to the temporary directory.
    * @param sJournalPath The full path to the journal directory.
    */
   public void debugSettings(String sDataDir, String sTempDir, String sJournalPath)
   {
      m_sDataDirectory = sDataDir;
      m_sTemporaryDirectory = sTempDir;
      m_sJournalPath = sJournalPath;
   }
}
