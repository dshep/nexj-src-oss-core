// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.file;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Iterator;

import nexj.core.meta.Attribute;
import nexj.core.meta.Component;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.Key;
import nexj.core.meta.persistence.Schema;
import nexj.core.meta.persistence.SchemaUpgrade;
import nexj.core.meta.persistence.file.FileDataSourceFragment;
import nexj.core.meta.persistence.file.FileMapping;
import nexj.core.meta.persistence.file.FilePrimitiveMapping;
import nexj.core.meta.upgrade.UpgradeState;
import nexj.core.persistence.Cursor;
import nexj.core.persistence.Field;
import nexj.core.persistence.GenericPersistenceAdapter;
import nexj.core.persistence.InvalidQueryException;
import nexj.core.persistence.OID;
import nexj.core.persistence.OIDGenerator;
import nexj.core.persistence.OIDHolder;
import nexj.core.persistence.PersistenceException;
import nexj.core.persistence.Query;
import nexj.core.persistence.SchemaVersion;
import nexj.core.persistence.Source;
import nexj.core.persistence.Work;
import nexj.core.persistence.operator.AttributeOperator;
import nexj.core.persistence.operator.ComparisonOperator;
import nexj.core.persistence.operator.ConstantOperator;
import nexj.core.persistence.operator.EqualsOperator;
import nexj.core.rpc.file.FileConnection;
import nexj.core.rpc.file.ra.LockableFile;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceArrayList;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.UnitOfWork;
import nexj.core.runtime.ValidationException;
import nexj.core.util.Logger;
import nexj.core.util.PropertyMap;
import nexj.core.util.Undefined;

/**
 * The File Persistence Adapter. Stores and retrieves instance data from
 * files in a directory specified in metadata.
 */
public class FileAdapter extends GenericPersistenceAdapter
{
   // associations

   /**
    * The File work item lookup key.
    */
   protected FileWork m_workKey;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(FileAdapter.class);


   // operations

   /**
    * @see nexj.core.persistence.PersistenceAdapter#addDenorm(nexj.core.runtime.UnitOfWork, nexj.core.runtime.Instance, nexj.core.meta.persistence.AttributeMapping)
    */
   public void addDenorm(UnitOfWork uow, Instance instance, AttributeMapping mapping) throws PersistenceException
   {
      //Do nothing.
   }


   /**
    * @see nexj.core.persistence.PersistenceAdapter#addDependency(nexj.core.runtime.UnitOfWork, nexj.core.persistence.Work, nexj.core.runtime.Instance, nexj.core.meta.persistence.Key, nexj.core.meta.persistence.Key, boolean)
    */
   public void addDependency(UnitOfWork uow, Work work, Instance instance, Key dstKey, Key srcKey, boolean bSuccessor)
      throws PersistenceException
   {
      //Do nothing.
   }


   /**
    * @see nexj.core.persistence.PersistenceAdapter#addWork(nexj.core.runtime.UnitOfWork, nexj.core.runtime.Instance)
    */
   public void addWork(UnitOfWork uow, Instance instance) throws PersistenceException
   {
      switch (instance.getState())
      {
         case Instance.NEW:
            addInsert(uow, instance);
            break;
         
         case Instance.DIRTY:
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("addUpdate(uow=\"" + uow + "\", instance=\"" + instance + "\")");
            }
            
            getWork(uow, FileWork.UPDATE, instance);
            break;
            
         case Instance.DELETED:
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("addDelete(uow=\"" + uow + "\", instance=\"" + instance + "\")");
            }
            
            getWork(uow, FileWork.DELETE, instance);
            break;
         
         default:
            throw new IllegalStateException();
      }
   }


   /**
    * Creates an insert work item.
    * 
    * @param uow      The unit of work to which the work item will belong.
    * @param instance The instance that will be inserted.
    */
   protected void addInsert(UnitOfWork uow, Instance instance)
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("addInsert(uow=\"" + uow + "\", instance=\"" + instance + "\")");
      }
      
      FileMapping fileMapping = (FileMapping)instance.getPersistenceMapping();
      OID oid = instance.getOID();

      getWork(uow, FileWork.INSERT, instance);

      if (oid == null)
      {
         //Generate OID using key generator specified in metadata configuration
         Component component = fileMapping.getKeyGenerator();

         oid = ((OIDGenerator)component.getInstance(uow.getInvocationContext())).generateOID(instance, this);

         //OID must be a single value, converted to a string.
         Object[] oidValues = oid.getValueArray();

         if (oidValues.length != 1)
         {
            throw new PersistenceException("err.persistence.oidValueCount",
               new Object[]{component.getName(), Primitive.ONE_INTEGER,
                  Primitive.createInteger(oidValues.length)});
         }

         oidValues[0] = Primitive.toString(oidValues[0]);
         instance.setOID(oid);
      }
   }


   /**
    * Gets a unit of work for the given instance, creating one of the given type
    * if it is not found in the unit of work.
    * 
    * @param uow      The unit of work of which the work item is a part.
    * @param nType    The type of work to do; one of FileWork.INSERT, FileWork.UPDATE,
    *                 or FileWork.DELETE.
    * @param instance The instance for which the work item is applicable.
    * @return The work item.
    */
   protected FileWork getWork(UnitOfWork uow, int nType, Instance instance)
   {
      FileWork work = findWork(uow, instance);
      
      if (work == null)
      {
         work = addWork(uow, nType, instance);
      }
      
      return work;
   }


   /**
    * Finds the work item for the given instance.
    * 
    * @param uow      The unit of work to search for work items.
    * @param instance The instance for which the work item should be found.
    * @return The work item; null if no item exists for the given instance
    *         in the unit of work.
    */
   protected synchronized FileWork findWork(UnitOfWork uow, Instance instance)
   {
      if (m_workKey == null)
      {
         m_workKey = new FileWork(instance, -1, this)
         {
            public void executeCommand()
            {
               // no-op
            }
         };
      }
      else
      {
         m_workKey.setData(instance);
      }
      
      return (FileWork)uow.findWork(m_workKey);
   }


   /**
    * Creates a new work item for the given instance and adds it to the unit of
    * work.
    * 
    * @param uow      The unit of work to which the work item should be added.
    * @param nType    The type of operation to perform on the instance, one of
    *                 FileWork.INSERT, FileWork.UPDATE, or FileWork.DELETE.
    * @param instance The instance containing the data to be used when doing the work.
    * @return The work item.
    */
   protected FileWork addWork(UnitOfWork uow, int nType, Instance instance)
   {
      FileWork work;
      
      work = new FileWork(instance, nType, this);
      uow.addWork(work);
      
      return work;
   }


   /**
    * @see nexj.core.persistence.PersistenceAdapter#execute(nexj.core.persistence.Work[], int, int)
    */
   public void execute(Work[] workArray, int nStart, int nEnd) throws PersistenceException
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("execute(workArray=\"" + Arrays.toString(workArray) + "\", nStart=" + nStart + ", nEnd=" + nEnd + ")");
      }
      
      for (int i = nStart; i < nEnd; i++)
      {
         FileWork work = (FileWork)workArray[i];
         
         work.executeCommand();
      }
   }


   /**
    * @see nexj.core.persistence.PersistenceAdapter#getFields(Source)
    */
   public Field[] getFields(Source source) throws PersistenceException
   {
      return (Field[])source.getItem();
   }


   /**
    * @see nexj.core.persistence.PersistenceAdapter#getValues(nexj.core.persistence.OID, Source)
    */
   public Object[] getValues(OID oid, Source source) throws PersistenceException
   {
      return oid.getValueArray();
   }


   /**
    * @see nexj.core.persistence.PersistenceAdapter#getValue(OID, nexj.core.persistence.Source)
    */
   public Object getValue(OID oid, Source source)
   {
      return Undefined.VALUE;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#getValue(PropertyMap, nexj.core.persistence.Source)
    */
   public Object getValue(PropertyMap instance, Source source)
   {
      return Undefined.VALUE;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#getOID(nexj.core.util.PropertyMap, java.lang.Object)
    */
   public OID getOID(PropertyMap instance, Object item)
   {
      return null;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#validate(nexj.core.meta.persistence.AttributeMapping, java.lang.Object)
    */
   public void validate(AttributeMapping mapping, Object value) throws ValidationException
   {
   }


   /**
    * @see nexj.core.persistence.PersistenceAdapter#getVersion(nexj.core.meta.persistence.Schema)
    */
   public SchemaVersion getVersion(Schema schema) throws PersistenceException
   {
      return null;
   }


   /**
    * @see nexj.core.persistence.PersistenceAdapter#mapQuery(nexj.core.persistence.Query)
    */
   public void mapQuery(Query query) throws PersistenceException
   {
      //Do nothing (persistence mapping done at read time, no pre-caching required here)
   }


   /**
    * @see nexj.core.persistence.PersistenceAdapter#read(nexj.core.persistence.Query)
    */
   public InstanceList read(Query query) throws PersistenceException
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("read(query=\"" + query +"\")");
      }
      
      if (query.getWhere() == null || query.getWhere().getOrdinal() != EqualsOperator.ORDINAL)
      {
         throw new InvalidQueryException("err.persistence.file.query");
      }
      
      ComparisonOperator op = (ComparisonOperator)query.getWhere();
      AttributeOperator aop;
      ConstantOperator con;
      
      if (op.getLeft().getOrdinal() == AttributeOperator.ORDINAL)
      {
         aop = (AttributeOperator)op.getLeft();
      }
      else if (op.getRight().getOrdinal() == AttributeOperator.ORDINAL)
      {
         aop = (AttributeOperator)op.getRight();
      }
      else
      {
         throw new InvalidQueryException("err.persistence.file.query");
      }
      
      if (op.getLeft().getOrdinal() == ConstantOperator.ORDINAL)
      {
         con = (ConstantOperator)op.getLeft();
      }
      else if (op.getRight().getOrdinal() == ConstantOperator.ORDINAL)
      {
         con = (ConstantOperator)op.getRight();
      }
      else
      {
         throw new InvalidQueryException("err.persistence.file.query");
      }

      OID oid;
      
      /*
       * Two kinds of queries:
       * 1) Lookup by equating a class to an OID.
       * 2) Lookup by equating name field to a string.
       */
      if (aop.getType() instanceof Metaclass)
      {
         if(con.getValue() instanceof OIDHolder)
         {
            oid = ((OIDHolder)con.getValue()).getOID();
         }
         else
         {
            throw new InvalidQueryException("err.persistence.file.query");
         }
      }
      else
      {
         if (con.getType() == Primitive.STRING)
         {
            oid = new OID(new Object[]{con.getValue()});
         }
         else
         {
            throw new InvalidQueryException("err.persistence.file.query");
         }
      }

      String sDataFileName = mapOIDToFile(oid);
      
      if (sDataFileName == null)
      {
         //Interpret badly-formatted OID as "not found"
         return new InstanceArrayList(0);
      }

      //Open file directly
      FileDataSourceFragment fragment = (FileDataSourceFragment)query.getFragment();
      File dataFile = nexj.core.rpc.file.ra.FileManagedConnection.splitNameToSubdirs(
         new File(fragment.getDataDirectory()), sDataFileName,
            fragment.getMaxNameSplits(), fragment.getNameSplitSize(), false);

      if (!dataFile.exists())
      {
         //Not found
         return new InstanceArrayList(0);
      }

      LockableFile directFile = new LockableFile(dataFile);

      //BLOCKING
      directFile.lock(true);
      
      try
      {
         Instance instance = m_context.findInstance(query.getMetaclass(), oid);

         if (instance == null)
         {
            instance = new Instance(query.getMetaclass(), m_context);
            instance.cache(oid);
         }

         for (Iterator i = query.getFieldIterator(); i.hasNext(); )
         {
            Field field = (Field)i.next();
            Attribute attribute = field.getAttribute();
            FilePrimitiveMapping primitiveMapping = (FilePrimitiveMapping)query.getPersistenceMapping().getAttributeMapping(attribute);
            
            switch (primitiveMapping.getSysId())
            {
               case FilePrimitiveMapping.SYSID_ID:
                  instance.setOldValueDirect(attribute.getOrdinal(), oid.getValue(0));
                  break;
                  
               case FilePrimitiveMapping.SYSID_DATA:
                  Object value = null;
                  
                  try
                  {
                     value = (primitiveMapping.getAttribute().getType() == Primitive.STRING) ?
                        (Object)directFile.getDataAsString() : directFile.getDataAsBinary();
                  }
                  catch (IOException ex)
                  {
                     throw new PersistenceException("err.persistence.file.io", ex);
                  }
                  
                  instance.setOldValueDirect(attribute.getOrdinal(), value);
                  
                  break;
               
               case FilePrimitiveMapping.SYSID_LOCKING:
                  instance.setOldValueDirect(attribute.getOrdinal(),
                     Primitive.createLong(dataFile.lastModified()) );
                  break;
            }
         }
         
         InstanceArrayList result = new InstanceArrayList(1);
         
         result.add(instance);
         
         return result;
      }
      finally
      {
         if (directFile != null)
         {
            directFile.unlock();
            directFile.close();
         }
      }
   }


   /**
    * Converts an OID to a file name string.
    * 
    * @param oid The oid to convert, must have only one value of type String.
    * @return The file name, or null if the oid is invalid or contains invalid characters.
    */
   protected String mapOIDToFile(OID oid)
   {
      if (oid.getCount() != 1 || !(oid.getValue(0) instanceof String))
      {
         return null;
      }
      
      String sFileName = (String)oid.getValue(0);
      
      //Validate file name
      for (int i = sFileName.length() - 1; i >= 0; i--)
      {
         char ch = sFileName.charAt(i);
         
         //CreateFile function: http://msdn2.microsoft.com/en-us/library/aa363858.aspx
         //Has list of forbidden characters: http://msdn2.microsoft.com/en-us/library/aa365247.aspx
         if (ch == '<' || ch == '>' || ch == ':' || ch == '"' || ch == '/' || ch == '\\' || ch == '|' || ch == '?' || ch == '*' || ch <= 31)
         {
            return null;
         }
      }
      
      return sFileName;
   }


   /**
    * @see nexj.core.persistence.PersistenceAdapter#openCursor(nexj.core.persistence.Query)
    */
   public Cursor openCursor(Query query) throws PersistenceException
   {
      return new FileCursor(this, query);
   }


   /**
    * @see nexj.core.persistence.PersistenceAdapter#setVersion(nexj.core.meta.persistence.Schema, nexj.core.persistence.SchemaVersion)
    */
   public void setVersion(Schema schema, SchemaVersion version) throws PersistenceException
   {
      //Do nothing.
   }


   /**
    * @see nexj.core.persistence.PersistenceAdapter#upgrade(nexj.core.meta.persistence.SchemaUpgrade, nexj.core.meta.upgrade.UpgradeState, nexj.core.persistence.SchemaVersion)
    */
   public void upgrade(SchemaUpgrade upgrade, UpgradeState state, SchemaVersion version) throws PersistenceException
   {
      throw new UnsupportedOperationException();
   }

   /**
    * Gets a FileConnection.
    * 
    * @param fragment The data source fragment.
    * @param sDataFile The name of the data file to which the connection should point.
    * @return A FileConnection.
    */
   public synchronized FileConnection getConnection(FileDataSourceFragment fragment, String sDataFile)
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Creating new connection");
      }

      FileConnection connection = ((FileStorageConnectionFactory)fragment.getConnectionFactory()
         .getInstance(m_context)).getConnection(this);

      if (!connection.attachToFile(sDataFile))
      {
         throw new PersistenceException("err.persistence.file.io");
      }

      return connection;
   }
}
