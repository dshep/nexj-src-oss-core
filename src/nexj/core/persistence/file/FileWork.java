// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.file;

import java.io.IOException;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.Key;
import nexj.core.meta.persistence.file.FileDataSourceFragment;
import nexj.core.meta.persistence.file.FilePrimitiveMapping;
import nexj.core.persistence.OptimisticLockException;
import nexj.core.persistence.PersistenceAdapter;
import nexj.core.persistence.PersistenceException;
import nexj.core.persistence.Work;
import nexj.core.rpc.file.FileConnection;
import nexj.core.runtime.Instance;
import nexj.core.util.Binary;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * A file persistence adapter work item.
 */
public class FileWork extends Work
{
   // constants

   /**
    * Identifies work items that perform a delete operation.
    */
   public final static int DELETE = 1;

   /**
    * Identifies work items that perform an update operation.
    */
   public final static int UPDATE = 2;

   /**
    * Identifies work items that perform an insert operation.
    */
   public final static int INSERT = 3;


   // attributes

   /**
    * The kind of work operation being performed by this work item; one
    * of DELETE, UPDATE, or INSERT.
    */
   protected int m_nType;


   // associations

   /**
    * The persistence adapter instance for which this work item was created.
    */
   protected FileAdapter m_adapter;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(FileWork.class);


   // constructors

   /**
    * Creates a new file persistence adapter work item.
    * 
    * @param instance The metaclass instance that is being persisted.
    * @param nType    The kind of work to perform, one of FileWork.DELETE,
    *                 FileWork.UPDATE, or FileWork.INSERT.
    * @param adapter  The persistence adapter instance for which this work item
    *                 was created.
    */
   public FileWork(Instance instance, int nType, FileAdapter adapter)
   {
      super(instance);
      
      m_nType = nType;
      m_adapter = adapter;
   }


   // operations

   /**
    * @see nexj.core.persistence.Work#compareTo(nexj.core.persistence.Work)
    */
   protected int compareTo(Work work)
   {
      return 0;
   }


   /**
    * @see nexj.core.persistence.Work#equals(nexj.core.persistence.Work)
    */
   public boolean equals(Work work)
   {
      return m_instance == ((FileWork)work).m_instance;
   }


   /**
    * @see nexj.core.persistence.Work#getAdapter()
    */
   public PersistenceAdapter getAdapter()
   {
      return m_adapter;
   }


   /**
    * @see nexj.core.persistence.Work#getType()
    */
   public int getType()
   {
      return m_nType;
   }


   /**
    * @see nexj.core.persistence.Work#hashCode()
    */
   public int hashCode()
   {
      return m_instance.hashCode();
   }


   /**
    * @see nexj.core.persistence.Work#setKeyValue(nexj.core.meta.persistence.Key, nexj.core.meta.persistence.Key, nexj.core.runtime.Instance)
    */
   public void setKeyValue(Key dstKey, Key srcKey, Instance instance)
   {
   }


   /**
    * Changes the instance to which this work item applies.
    * 
    * @param instance The new instance.
    */
   public void setData(Instance instance)
   {
      m_instance = instance;
   }


   /**
    * Performs the work represented by this work item.
    */
   public void executeCommand()
   {
      Attribute lockAttrib = m_mapping.getLockingAttribute();
      
      //Compute filename from instance id
      String sDataFile = m_adapter.mapOIDToFile(m_instance.getOID());
      
      if (sDataFile == null)
      {
         throw new PersistenceException("err.persistence.file.invalidFileName",
            new Object[]{m_instance.getOID().toString(), m_instance.getClassName()});
      }
      
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Executing on file: " + sDataFile);
      }
      
      
      //Get the connection to use
      FileConnection connection = m_adapter.getConnection((FileDataSourceFragment)getFragment(), sDataFile);

      try
      {
         //Check for optimistic lock violation
         if (m_nType == UPDATE || m_nType == DELETE)
         {
            Object lockValue = m_instance.getOldValueDirect(lockAttrib.getOrdinal());
            long lCurrentLockValue = connection.getLastModified();
            
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("LOCKING " + ( (((Long)lockValue).longValue() < lCurrentLockValue) ? "BAD" : "GOOD") + " instance=" + lockValue + " file=" + lCurrentLockValue);
            }
            
            if (((Long)lockValue).longValue() < lCurrentLockValue)
            {
               throw new OptimisticLockException(m_instance);
            }
         }
         
         Object dataToWrite = null;
         
         if (m_nType == INSERT || m_nType == UPDATE)
         {
            Metaclass metaclass = m_instance.getMetaclass();
            
            for (int i = 0, nCount = metaclass.getInstanceAttributeCount(); i < nCount; i++)
            {
               Attribute attribute = metaclass.getInstanceAttribute(i);
               AttributeMapping attributeMapping = m_mapping.getAttributeMapping(attribute);
               
               if (attributeMapping == null)
               {
                  continue;
               }
               
               FilePrimitiveMapping mapping = (FilePrimitiveMapping)attributeMapping;
               
               if (mapping.getSysId() == FilePrimitiveMapping.SYSID_DATA)
               {
                  dataToWrite = m_instance.getValueDirect(i);
                  break;
               }
            }
            
            if (dataToWrite instanceof String)
            {
               connection.write((String)dataToWrite);
            }
            else if (dataToWrite instanceof Binary)
            {
               connection.write((Binary)dataToWrite);
            }
            else
            {
               throw new IllegalArgumentException("Data being written must be Binary or String");
            }
            
         }
         else if (m_nType == DELETE)
         {
            connection.delete();
         }
         else
         {
            throw new IllegalStateException("Unknown work type");
         }
      
         //Update the instance locking attribute
         if (m_nType == INSERT || m_nType == UPDATE)
         {
            /*
             * Do not modify instance locking attribute in DELETE case because that
             * will immediately set "locking" to zero and replication to fragments
             * will fail because they will see this zero value.
             */
            
            long lNewLockValue = connection.getLastModifiedThisTxn();
            
            m_instance.setValueDirect(lockAttrib.getOrdinal(),
               Primitive.createLong(lNewLockValue)
               );
         }
      }
      catch (IOException ex)
      {
         ObjUtil.rethrow(ex);
      }
      finally
      {
         connection.close();
      }
   }


   /**
    * Provides a string representation for debugging purposes.
    * 
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder(64);

      buf.append(super.toString());
      buf.append("(instance=\"");
      buf.append(m_instance);
      buf.append("\", type=");
      
      switch (m_nType)
      {
         case INSERT:
            buf.append("INSERT");
            break;
         
         case UPDATE:
            buf.append("UPDATE");
            break;
            
         case DELETE:
            buf.append("DELETE");
            break;
            
         default:
            buf.append(m_nType);
      }
      
      buf.append(')');
      
      return buf.toString();
   }
}
