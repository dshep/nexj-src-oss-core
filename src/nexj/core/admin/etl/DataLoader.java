// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.admin.etl;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Writer;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.meta.persistence.SchemaUpgrade;
import nexj.core.meta.persistence.sql.RelationalClassMapping;
import nexj.core.meta.persistence.sql.RelationalDatabase;
import nexj.core.meta.persistence.sql.RelationalDatabaseFragment;
import nexj.core.meta.persistence.sql.RelationalMapping;
import nexj.core.meta.persistence.sql.RelationalPrimitiveMapping;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.meta.upgrade.ScriptUpgrade;
import nexj.core.meta.upgrade.Upgrade;
import nexj.core.meta.upgrade.UpgradeState;
import nexj.core.meta.upgrade.VersionUpgrade;
import nexj.core.persistence.OID;
import nexj.core.persistence.PersistenceAdapter;
import nexj.core.persistence.Query;
import nexj.core.persistence.SchemaVersion;
import nexj.core.persistence.sql.SQLAdapter;
import nexj.core.persistence.sql.SQLConnectionFactory;
import nexj.core.persistence.sql.SQLConnectionInterceptor;
import nexj.core.persistence.sql.SQLSchemaManager;
import nexj.core.persistence.sql.SQLWriterConnection;
import nexj.core.rpc.CharacterStreamMarshaller;
import nexj.core.rpc.CharacterStreamUnmarshaller;
import nexj.core.rpc.InstanceFactory;
import nexj.core.rpc.RPCUtil;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.text.TextMarshaller;
import nexj.core.runtime.Context;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceArrayList;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.scripting.Function;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.IOUtil;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Named;
import nexj.core.util.NoCloseInputStream;
import nexj.core.util.NoCloseOutputStream;
import nexj.core.util.ObjUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.Undefined;
import nexj.core.util.XMLUtil;

/**
 * Data import/export tool.
 */
public class DataLoader
{
   // attributes

   /**
    * The data marshaller class name.
    */
   protected String m_sMarshallerClassName = TextMarshaller.class.getName();

   /**
    * The data unmarshaller class name.
    * Null to auto-detect.
    */
   protected String m_sUnmarshallerClassName;

   /**
    * The data compression flag.
    */
   protected boolean m_bCompressed = true;
   
   /**
    * The version maintained flag.
    */
   protected boolean m_bVersionMaintained = true;

   // associations

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(DataLoader.class);

   // constructors

   /**
    * Constructs the player with an invocation context.
    * @param context The invocation context.
    */
   public DataLoader(InvocationContext context)
   {
      m_context = context;
   }

   // operations

   /**
    * Sets the data marshaller class name.
    * @param sMarshallerClassName The data marshaller class name to set.
    */
   public void setMarshallerClassName(String sMarshallerClassName)
   {
      m_sMarshallerClassName = sMarshallerClassName;
   }

   /**
    * @return The data marshaller class name.
    */
   public String getMarshallerClassName()
   {
      return m_sMarshallerClassName;
   }

   /**
    * Sets the data unmarshaller class name.
    * @param sUnmarshallerClassName The data unmarshaller class name to set.
    */
   public void setUnmarshallerClassName(String sUnmarshallerClassName)
   {
      m_sUnmarshallerClassName = sUnmarshallerClassName;
   }

   /**
    * @return The data unmarshaller class name.
    */
   public String getUnmarshallerClassName()
   {
      return m_sUnmarshallerClassName;
   }

   /**
    * Sets the data compression flag.
    * @param bCompressed The data compression flag to set.
    */
   public void setCompressed(boolean bCompressed)
   {
      m_bCompressed = bCompressed;
   }

   /**
    * @return The data compression flag.
    */
   public boolean isCompressed()
   {
      return m_bCompressed;
   }

   /**
    * Sets the version maintained flag.
    * @param bVersionMaintained The version maintained flag to set.
    */
   public void setVersionMaintained(boolean bVersionMaintained)
   {
      m_bVersionMaintained = bVersionMaintained;
   }

   /**
    * @return The version maintained flag.
    */
   public boolean isVersionMaintained()
   {
      return m_bVersionMaintained;
   }

   /**
    * Outputs an object to a stream.
    * @param ostream The object dump output stream.
    * @param obj The object to write.
    */
   protected void output(OutputStream ostream, Object obj) throws Exception
   {
      CharacterStreamMarshaller msh = (CharacterStreamMarshaller)Class.forName(m_sMarshallerClassName)
         .getConstructor(new Class[]{Context.class})
         .newInstance(new Object[]{m_context});

      if (msh instanceof TextMarshaller)
      {
         ((TextMarshaller)msh).setVersion(3);
      }

      if (m_bCompressed)
      {
         ostream = new GZIPOutputStream(new NoCloseOutputStream(ostream));
      }

      Writer writer = IOUtil.openBufferedWriter(ostream, XMLUtil.ENCODING);

      msh.serialize(obj, writer);

      writer.close();
   }

   /**
    * Inputs an object from a stream.
    * @param istream The object dump output stream.
    */
   protected Object input(InputStream istream) throws Exception
   {
      boolean bCompressed = m_bCompressed;

      if (!istream.markSupported())
      {
         istream = new BufferedInputStream(istream);
      }

      if (bCompressed)
      {
         istream.mark(512);

         try
         {
            istream = new GZIPInputStream(istream);
         }
         catch (IOException e)
         {
            istream.reset();
            bCompressed = false;
         }
      }

      String[] sClassNameArray = (m_sUnmarshallerClassName != null) ?
         new String[]{m_sUnmarshallerClassName} :
         new String[]
         {
            SysUtil.PACKAGE + ".core.rpc.json.JSONUnmarshaller",
            SysUtil.PACKAGE + ".core.rpc.text.TextUnmarshaller",
            SysUtil.PACKAGE + ".core.rpc.soap.SOAPUnmarshaller",
            SysUtil.PACKAGE + ".core.rpc.xml.XMLUnmarshaller",
         };

      Exception e = null;
      boolean bReset = (sClassNameArray.length > 1);

      if (bReset)
      {
         if (!istream.markSupported())
         {
            istream = new BufferedInputStream(istream);
         }

         istream = new NoCloseInputStream(istream);
      }

      for (int i = 0; i < sClassNameArray.length; ++i)
      {
         if (bReset)
         {
            istream.mark(16384);
         }

         try
         {
            CharacterStreamUnmarshaller unmsh = (CharacterStreamUnmarshaller)Class
               .forName(sClassNameArray[i])
               .getConstructor(new Class[]{Context.class})
               .newInstance(new Object[]{m_context});

            return unmsh.deserialize(IOUtil.openBufferedReader(istream, XMLUtil.ENCODING));
         }
         catch (Exception x)
         {
            e = x;

            if (bReset)
            {
               istream.reset();
            }
         }
      }

      throw e;
   }

   /**
    * Resolves a map value by looking up consecutively a class and its base classes.
    * @param metaclass The class object. Can be null.
    * @param map The value map. Can be null.
    * @return The resolved value, or null if not found.
    */
   protected static Object resolve(Metaclass metaclass, Lookup map)
   {
      if (map != null)
      {
         for (; metaclass != null; metaclass = metaclass.getBase())
         {
            Object value = map.get(metaclass);

            if (value != null)
            {
               return value;
            }
         }
      }

      return null;
   }

   /**
    * Reads instances.
    * @param metaclass The class object.
    * @param attributes The attributes to read.
    * @param where The where clause (can be an OID collection).
    * @return The instances read.
    */
   protected InstanceList read(Metaclass metaclass, Pair attributes, Object where)
   {
      if (where instanceof Collection)
      {
         return read(metaclass, attributes, (Collection)where, 128);
      }

      Query query = Query.createRead(metaclass, attributes, where, null, -1, 0, false, Query.SEC_NONE, m_context);

      query.setLimit(-1);
      query.reduce();

      return query.read();
   }

   /**
    * Reads instances specified by their OIDs.
    * @param metaclass The class object.
    * @param attributes The attributes to read.
    * @param oidList The collection of instance OIDs.
    * @return The instances read.
    */
   protected InstanceList read(Metaclass metaclass, Pair attributes, Collection oidList, int nPageSize)
   {
      List sortedOIDList = new ArrayList(oidList);

      Collections.sort(sortedOIDList);

      int nCount = sortedOIDList.size();

      if (nPageSize <= 0)
      {
         nPageSize = nCount;
      }

      InstanceList instanceList = new InstanceArrayList();

      for (int i = 0; i < nCount; i += nPageSize)
      {
         InstanceList list = read(metaclass, attributes,
            Pair.list(Symbol.IN_P, Pair.list(Symbol.AT),
               sortedOIDList.subList(i, Math.min(i + nPageSize, nCount))));

         for (int k = 0; k < list.size(); ++k)
         {
            instanceList.add(list.getInstance(k), InstanceList.REPLACE);
         }
      }

      return instanceList;
   }

   /**
    * Exports the data from the system.
    * @param dataSourceSet Set of data sources to export. Null for all.
    * @param whereMap Maps a class object to a where clause or an OID collection: Object[Metaclass].
    * @param bExclusive True to limit the exported classes to those in whereMap.
    * @return List of transfer object.
    */
   public List exportData(Set dataSourceSet, Lookup whereMap, boolean bExclusive)
   {
      Metadata metadata = m_context.getMetadata();
      List resultList = new ArrayList();
      Lookup identityMap = new IdentityHashTab();
      Lookup classMap = new HashTab();

      for (Iterator itr = metadata.getMetaclassIterator(); itr.hasNext();)
      {
         Metaclass metaclass = (Metaclass)itr.next();
         PersistenceMapping mapping = metaclass.getPersistenceMapping();
         Object where = resolve(metaclass, whereMap);

         if (mapping == null ||
            dataSourceSet != null && !dataSourceSet.contains(mapping.getDataSource()) ||
            where == null && bExclusive ||
            Boolean.FALSE.equals(where) ||
            !isWritable(mapping))
         {
            continue;
         }

         Metaclass root = metaclass.getPersistenceRoot();
         Pair attributes = null;

         for (int i = 0, n = metaclass.getInstanceAttributeCount(); i < n; ++i)
         {
            Attribute attribute = metaclass.getInstanceAttribute(i);
            Metaclass declarator = attribute.getDeclarator();

            if (attribute.isPersistent() &&
               attribute.getWhere() == null &&
               (!attribute.isCollection() || attribute.getReverse() == null) &&
               attribute != mapping.getLockingAttribute() &&
               (declarator == metaclass ||
                  declarator.getPersistenceMapping() == null ||
                  declarator.getPersistenceMapping().isCompatible(metaclass.getPersistenceMapping())) &&
               isWritable(metaclass.getPersistenceMapping().getAttributeMapping(attribute)))
            {
               attributes = new Pair(attribute.getSymbol(), attributes);
            }
         }

         if (metaclass != root)
         {
            attributes = new Pair(new Pair(Symbol.ATAT, new Pair(metaclass.getSymbol(), attributes)));
         }

         classMap.put(root, Pair.append((Pair)classMap.get(root), attributes));
      }

      for (Iterator itr = classMap.iterator(); itr.hasNext();)
      {
         Metaclass metaclass = (Metaclass)itr.next();

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Reading class \"" + metaclass.getName() + "\"");
         }

         Pair attributes = (Pair)classMap.get(metaclass);

         resultList.add(new Pair(read(metaclass, attributes, resolve(metaclass, whereMap)), attributes));
      }

      List list = new ArrayList();

      for (int i = 0, n = resultList.size(); i < n; ++i)
      {
         Pair pair = (Pair)resultList.get(i);
         InstanceList ilist = (InstanceList)pair.getHead();

         if (s_logger.isDebugEnabled())
         {
            if (!ilist.isEmpty())
            {
               s_logger.debug("Exporting class \"" + ilist.getInstance(0).getMetaclass()
                  .getPersistenceRoot().getName() + "\"");
            }
         }

         list.addAll((List)RPCUtil.transfer(ilist, pair.getNext(), identityMap, RPCUtil.TF_ALL | RPCUtil.TF_LAZY));
      }

      return list;
   }

   /**
    * Exports the data from the system.
    * @param ostream Object dump output stream.
    * @param dataSourceSet Set of data sources to export. Null for all.
    * @param whereMap Maps a class object to a where clause: Object[Metaclass].
    * @param bExclusive True to limit the exported classes to those in whereMap.
    */
   public void exportData(OutputStream ostream, Set dataSourceSet, Lookup whereMap, boolean bExclusive) throws Exception
   {
      output(ostream, exportData(dataSourceSet, whereMap, bExclusive));
   }

   /**
    * Exports data corresponding to a given read specification.
    * @param specItr Read specification iterator: (class attributes where)
    * @return List of transfer object.
    */
   public List exportData(Iterator specItr)
   {
      Lookup identityMap = new IdentityHashTab(128); 

      while (specItr.hasNext())
      {
         Object spec = specItr.next();
         Metaclass metaclass;
         Pair attributes = null;
         Object where = null;
         Object obj;
         Pair pair;

         if (spec instanceof Pair)
         {
            pair = (Pair)spec;
            obj = pair.getHead();
            pair = pair.getNext();
         }
         else
         {
            obj = spec;
            pair = null;
         }

         if (obj instanceof Metaclass)
         {
            metaclass = (Metaclass)obj;
         }
         else if (obj instanceof Symbol || obj instanceof String)
         {
            metaclass = m_context.getMetadata().getMetaclass(obj.toString());
         }
         else
         {
            throw new IllegalArgumentException("Invalid class specification");
         }

         if (pair != null)
         {
            attributes = (Pair)pair.getHead();
            pair = pair.getNext();
         }

         if (pair != null)
         {
            where = pair.getHead();

            if (pair.getTail() != null)
            {
               throw new IllegalArgumentException("Invalid query specification");
            }
         }

         RPCUtil.transfer(read(metaclass, attributes, where), attributes,
            identityMap, RPCUtil.TF_ALL | RPCUtil.TF_LAZY);
      }

      Lookup whereMap = new HashTab();

      for (Iterator itr = identityMap.iterator(); itr.hasNext();)
      {
         Object obj = itr.next();

         if (obj instanceof Instance)
         {
            Instance instance = (Instance)obj;
            OID oid = instance.getOID();

            if (oid != null)
            {
               Metaclass metaclass = instance.getLazyMetaclass().getPersistenceRoot();
               Set oidSet = (Set)whereMap.get(metaclass);

               if (oidSet == null)
               {
                  oidSet = new HashHolder();
                  whereMap.put(metaclass, oidSet);
               }

               oidSet.add(oid);
            }
         }
      }

      return exportData(null, whereMap, true);
   }

   /**
    * Exports data corresponding to a given read specification.
    * @param ostream Object dump output stream.
    * @param specItr Read specification iterator: (class attributes where)
    * @return List of transfer object.
    */
   public void exportData(OutputStream ostream, Iterator specItr) throws Exception
   {
      output(ostream, exportData(specItr));
   }

   /**
    * Imports the data into the persistent storage.
    * @param tobjItr TransferObject iterator.
    * @param bEmpty True if the persistent storage is empty.
    */
   public void importData(Iterator tobjItr, boolean bEmpty) throws Exception
   {
      importData(tobjItr, null, bEmpty);
   }

   /**
    * Imports the data into the persistent storage.
    * @param tobjItr TransferObject iterator.
    * @param dataSourceSet Set of data sources to import. Null for all.
    * @param bEmpty True if the persistent storage is empty.
    */
   public void importData(Iterator tobjItr, Set dataSourceSet, boolean bEmpty)
   {
      m_context.getUnitOfWork().setMaxChangeCount(-1);

      InstanceFactory factory = new InstanceFactory(m_context);

      while (tobjItr.hasNext())
      {
         factory.instantiate((TransferObject)tobjItr.next());
      }

      for (Iterator itr = factory.getIdentityMap().valueIterator(); itr.hasNext();)
      {
         Object obj = itr.next();

         if (obj instanceof Instance)
         {
            Instance inst = (Instance)obj;

            if (inst.getOID() != null)
            {
               Metaclass metaclass = inst.getMetaclass();
               PersistenceMapping mapping = inst.getPersistenceMapping();

               if (isRoot(metaclass) && isWritable(mapping) &&
                   (dataSourceSet == null || dataSourceSet.contains(mapping.getDataSource())))
               {
                  if (bEmpty && (!m_bVersionMaintained || !metaclass.getName().equals(Metadata.VERSION_CLASS_NAME)) ||
                      Query.createRead(metaclass, null, Pair.attribute("").eq(inst.getOID()), null,
                                       -1, 0, false, Query.SEC_NONE, m_context).read().isEmpty())
                  {
                     inst.setNew();

                     for (int i = 0, n = metaclass.getInstanceAttributeCount(); i < n ; ++i)
                     {
                        Attribute attribute = metaclass.getInstanceAttribute(i);

                        if (attribute.isPersistent() &&
                           attribute.isCollection() &&
                           inst.getValueDirect(i) == Undefined.VALUE)
                        {
                           InstanceList list = new InstanceArrayList(0);

                           list.setLazy(false);
                           list.setAssociation(inst, attribute, true);
                           inst.setValueDirect(i, list);
                        }
                     }
                  }
                  else
                  {
                     inst.setDirty();

                     // reset "old" values or UOW will think nothing changed and will skip instance
                     for (int i = 0, n = metaclass.getInstanceAttributeCount(); i < n ; ++i)
                     {
                        inst.setOldValueDirect(i, Undefined.VALUE);
                     }
                  }

                  inst.replicate();
               }
               else
               {
                  inst.setClean();
               }
            }

            inst.setEventPending(false);
            inst.setCommitPending(false);
         }
      }

      m_context.getUnitOfWork().setRaw(true);
   }

   /**
    * Imports the data into the persistent storage.
    * @param istream The dump input stream.
    * @param bEmpty True if the persistent storage is empty.
    */
   public void importData(InputStream istream, boolean bEmpty) throws Exception
   {
      importData(istream, null, bEmpty);
   }

   /**
    * Imports the data into the persistent storage.
    * @param istream The dump input stream.
    * @param dataSourceSet Set of data sources to import. Null for all.
    * @param bEmpty True if the persistent storage is empty.
    */
   public void importData(InputStream istream, Set dataSourceSet, boolean bEmpty) throws Exception
   {
      Metadata metadata = m_context.getMetadata();
      List list = (List)input(istream);

      if (metadata.findMetaclass(Metadata.VERSION_CLASS_NAME) != null)
      {
         for (int i = 0, nCount = list.size(); i < nCount; ++i)
         {
            TransferObject tobj = (TransferObject)list.get(i);

            if (tobj.getClassName().equals(Metadata.VERSION_CLASS_NAME))
            {
               if (!metadata.getNamespace().equals(tobj.findValue("namespace")) ||
                  !metadata.getVersion().equals(tobj.findValue("version")))
               {
                  throw new IllegalArgumentException("Version mismatch: dump version " +
                     tobj.findValue("namespace") + '#' + tobj.findValue("version") +
                     ", metadata version " + metadata.getNamespace() + '#' +
                     metadata.getVersion());
               }

               break;
            }
         }
      }

      importData(list.iterator(), dataSourceSet, bEmpty);
      m_context.getUnitOfWork().commit();
   }

   /**
    * Update the version record in a given dump file and write the resulting dump file to an output stream.
    * @param ostream Object dump output stream.
    * @param istream Object dump input stream.
    * @param sNamespace New repository namespace.
    * @param sVersion New repository version.
    */
   public void setDataVersion(OutputStream ostream, InputStream istream, String sNamespace, String sVersion) throws Exception
   {
      List list = (List)input(istream);
      int nCount = list.size();

      for (int i = 0; i < nCount; ++i)
      {
         TransferObject tobj = (TransferObject)list.get(i);

         if (tobj.getClassName().equals(Metadata.VERSION_CLASS_NAME))
         {
            tobj.setOID(new OID(new Object[] {sNamespace}));
            tobj.setValue("namespace", sNamespace);
            tobj.setValue("version", sVersion);
         }
      }

      output(ostream, list);
   }

   /**
    * Deletes the data in data sources.
    * @param dataSourceSet The data source set, or null for all.
    */
   public void deleteData(Set dataSourceSet) throws Exception
   {
      for (Iterator itr = getDataSourceIterator(dataSourceSet); itr.hasNext();)
      {
         deleteData((DataSource)itr.next());
      }
   }

   /**
    * Deletes the data in a data source.
    * @param dataSource The data source.
    */
   public void deleteData(DataSource dataSource) throws Exception
   {
      if (dataSource instanceof RelationalDatabase)
      {
         RelationalDatabase database = (RelationalDatabase)dataSource;
         SQLAdapter adapter = (SQLAdapter)database.getComponent().getInstance(m_context);
         SQLSchemaManager manager = adapter.createSchemaManager(database);
         RelationalSchema schema = (RelationalSchema)database.getSchema();
         String sOrigFragment = m_context.getFragmentName();
         Connection connection = null;

         try
         {
            for (Iterator itr = dataSource.getFragmentIterator(); itr.hasNext();) // each fragment
            {
               m_context.setFragmentName(((Named)itr.next()).getName()); // connection for fragment

               try
               {
                  if (s_logger.isDebugEnabled())
                  {
                     s_logger.debug("Deleting data in data source \"" + dataSource.getName() + '"');
                  }

                  connection = adapter.getConnectionFactory().getConnection(adapter);
                  manager.setSQLAppender(new SQLSchemaManager.SQLConnectionAppender(connection, false));
                  manager.truncateSchema(schema);

                  if (s_logger.isDebugEnabled())
                  {
                     s_logger.debug("Deletion completed");
                  }
               }
               finally
               {
                  adapter.close(connection);
               }
            }
         }
         finally
         {
            m_context.setFragmentName(sOrigFragment); // reset to original fragment
         }
      }
   }

   /**
    * Drops and creates data source schemas.
    * @param dataSourceSet The data source set, or null for all.
    */
   public void recreateSchema(Set dataSourceSet) throws Exception
   {
      for (Iterator itr = getDataSourceIterator(dataSourceSet); itr.hasNext();)
      {
         dropSchema((DataSource)itr.next());
      }

      boolean bFirst = true;

      for (Iterator itr = getDataSourceIterator(dataSourceSet); itr.hasNext();)
      {
         DataSource ds = (DataSource)itr.next();

         if (!bFirst && ds instanceof RelationalDatabase && ds.getSchema().getMetadata().isTestEnvironment())
         {
            dropSchema((RelationalDatabase)ds, false);
         }

         createSchema(ds);
         bFirst = false;
      }
   }

   /**
    * Drops data source schemas.
    * @param dataSourceSet The data source set, or null for all.
    */
   public void dropSchema(Set dataSourceSet) throws Exception
   {
      for (Iterator itr = getDataSourceIterator(dataSourceSet); itr.hasNext();)
      {
         dropSchema((DataSource)itr.next());
      }
   }

   /**
    * Creates data source schemas.
    * @param dataSourceSet The data source set, or null for all.
    */
   public void createSchema(Set dataSourceSet) throws Exception
   {
      for (Iterator itr = getDataSourceIterator(dataSourceSet); itr.hasNext();)
      {
         createSchema((DataSource)itr.next());
      }
   }

   /**
    * Drops and creates a data source schema.
    * @param dataSource The data source.
    */
   public void recreateSchema(DataSource dataSource) throws Exception
   {
      dropSchema(dataSource);
      createSchema(dataSource);
   }

   /**
    * Drops a data source schema.
    * @param dataSource The data source.
    */
   public void dropSchema(DataSource dataSource) throws Exception
   {
      if (dataSource instanceof RelationalDatabase)
      {
         dropSchema((RelationalDatabase)dataSource, dataSource.getSchema().getMetadata().isTestEnvironment());
      }
   }

   /**
    * Drops a data source schema.
    * @param database The relational data source.
    * @param bOld true to drop old schema objects.
    */
   protected void dropSchema(RelationalDatabase database, boolean bOld) throws Exception
   {
      SQLAdapter adapter = (SQLAdapter)database.getComponent().getInstance(m_context);
      SQLSchemaManager manager = adapter.createSchemaManager(database);
      RelationalSchema schema = (RelationalSchema)database.getSchema();
      String sOrigFragment = m_context.getFragmentName();
      Connection connection = null;

      try
      {
         for (Iterator itr = database.getFragmentIterator(); itr.hasNext();) // each fragment
         {
            m_context.setFragmentName(((Named)itr.next()).getName()); // connection for fragment

            try
            {
               connection = adapter.getConnectionFactory().getConnection(adapter);
               manager.setSQLAppender(
                  new SQLSchemaManager.SQLConnectionAppender(connection, true));

               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug(
                     "Dropping the schema of data source \"" + database.getName() + '"');
               }

               manager.dropSchema(schema, bOld);

               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Schema dropped");
               }
            }
            finally
            {
               adapter.close(connection);
            }
         }
      }
      finally
      {
         m_context.setFragmentName(sOrigFragment); // reset to original fragment
      }
   }

   /**
    * Creates a data source schema.
    * @param dataSource The data source.
    */
   public void createSchema(DataSource dataSource) throws Exception
   {
      if (dataSource instanceof RelationalDatabase)
      {
         RelationalDatabase database = (RelationalDatabase)dataSource;
         SQLAdapter adapter = (SQLAdapter)database.getComponent().getInstance(m_context);
         SQLSchemaManager manager = adapter.createSchemaManager(database);
         RelationalSchema schema = (RelationalSchema)database.getSchema();
         String sOrigFragment = m_context.getFragmentName();
         Connection connection = null;

         try
         {
            for (Iterator itr = dataSource.getFragmentIterator(); itr.hasNext();) // each fragment
            {
               m_context.setFragmentName(((Named)itr.next()).getName()); // connection for fragment

               try
               {
                  connection = adapter.getConnectionFactory().getConnection(adapter);
                  manager.setSQLAppender(
                     new SQLSchemaManager.SQLConnectionAppender(connection, false));

                  if (s_logger.isDebugEnabled())
                  {
                     s_logger.debug(
                        "Creating the schema of data source \"" + dataSource.getName() + '"');
                  }

                  manager.createSchema(schema);

                  if (s_logger.isDebugEnabled())
                  {
                     s_logger.debug("Schema created");
                  }
               }
               finally
               {
                  if (connection != null)
                  {
                     try
                     {
                        connection.close();
                     }
                     catch (SQLException e)
                     {
                     }
                  }
               }
            }
         }
         finally
         {
            m_context.setFragmentName(sOrigFragment); // reset to original fragment
         }
      }
   }

   /**
    * Interprets the input stream as a dump file and writes corresponding SQL statements to writer.
    * @param out The Output writer to receive the generated script.
    * @param in The input data stream containing the dump file.
    * @param dataSourceSet The set of data sources to consider.
    * @throws Exception If an dump file import exception occurs.
    */
   public void generateScript(final Writer out, InputStream in, Set dataSourceSet) throws Exception
   {
      SQLConnectionFactory outCF = new SQLWriterConnectionFactory(out, false);
      List/*<SQLAdapter>*/ adapterList = new ArrayList/*<SQLAdapter>*/();
      Set importDataSourceSet = new HashHolder();

      // inject a stream outputting component into every DataSource so none hits DB
      for (Iterator/*<DataSource>*/ itr = getDataSourceIterator(dataSourceSet);
           itr.hasNext();)
      {
         DataSource ds = (DataSource)itr.next();

         // only support RelationalDatabase for SQL dump
         if (!(ds instanceof RelationalDatabase))
         {
            continue; // skip DataSources without any Metaclasses mapped to them
         }

         importDataSourceSet.add(ds);

         SQLAdapter adapter = (SQLAdapter)ds.getComponent().getInstance(m_context);

         adapterList.add(adapter); //reference locally so not GCed
         adapter.setConnectionFactory(outCF); // override adapter's SQLConnectionFactory
      }

      importData(in, importDataSourceSet, true); // assume no data in DB
   }

   /**
    * Retrieves the repository version info from dump contents.
    * @param istream The input stream.
    * @return pair instance as (namespace . version) or null if version record is not found.
    */
   public Pair getDumpVersion(InputStream istream) throws Exception
   {
      List list = (List)input(istream);
      int nCount = list.size();

      for (int i = 0; i < nCount; ++i)
      {
         TransferObject tobj = (TransferObject)list.get(i);

         if (tobj.getClassName().equals(Metadata.VERSION_CLASS_NAME))
         {
            return new Pair(tobj.getValue("namespace"), tobj.getValue("version"));
         }
      }

      return null;
   }

   /**
    * Gets the repository version from the persistent storage.
    * @param dataSource The data source. Null to use the version class data source.
    * @return The schema version, or null if not found.
    */
   public SchemaVersion getSchemaVersion(DataSource dataSource)
   {
      if (dataSource == null)
      {
         Metaclass metaclass = m_context.getMetadata().findMetaclass(Metadata.SERVICE_CLASS_NAME);

         if (metaclass != null && metaclass.getPersistenceMapping() != null)
         {
            dataSource = metaclass.getPersistenceMapping().getDataSource();
         }

         if (dataSource == null)
         {
            return null;
         }
      }

      PersistenceAdapter adapter = (PersistenceAdapter)dataSource.getComponent().getInstance(m_context);

      return adapter.getVersion(dataSource.getSchema());
   }

   /**
    * Upgrades the database.
    * @param dataSourceSet The set of data sources to upgrade, or null for all.
    * @param upgrade The upgrade metadata.
    * @param bForce True to ignore the upgradable flag in the data source.
    */
   public void upgrade(Set dataSourceSet, Upgrade upgrade, boolean bForce)
   {
      Metadata metadata = m_context.getMetadata();
      SchemaVersion version = null;
      Iterator dataSourceItr = getDataSourceIterator(dataSourceSet);

      dataSourceSet = new HashHolder(4);

      while (dataSourceItr.hasNext())
      {
         DataSource ds = (DataSource)dataSourceItr.next();
         SchemaVersion dsVersion = ((PersistenceAdapter)ds.getComponent()
            .getInstance(m_context)).getVersion(ds.getSchema());

         if (dsVersion != null)
         {
            if (version == null)
            {
               version = dsVersion;

               if (!ObjUtil.equal(metadata.getNamespace(), version.getNamespace()))
               {
                  throw new IllegalStateException("Namespace mismatch in data source \"" +
                     ds.getName() + "\" (expected \"" + metadata.getNamespace() +
                     "\", got \"" + version.getNamespace() + "\")");
               }
            }
            else
            {
               if (!ObjUtil.equal(version.getNamespace(), dsVersion.getNamespace()))
               {
                  throw new IllegalStateException("Namespace mismatch in data source \"" +
                     ds.getName() + "\" (expected \"" + version.getNamespace() +
                     "\", got \"" + dsVersion.getNamespace() + "\")");
               }

               if (!ObjUtil.equal(version.getVersion(), dsVersion.getVersion()))
               {
                  throw new IllegalStateException("Version mismatch in data source \"" +
                     ds.getName() + "\" (expected \"" + version.getVersion() +
                     "\", got \"" + dsVersion.getVersion() + "\")");
               }
            }

            if (dsVersion.isUpgradable() || bForce)
            {
               dataSourceSet.add(ds);
            }
         }
      }

      if (dataSourceSet.isEmpty())
      {
         throw new IllegalStateException("No upgradable data sources");
      }

      if (version == null)
      {
         throw new IllegalStateException("Unavailable persistent storage version");
      }

      VersionUpgrade lastUpgrade = upgrade.getVersion(version.getVersion());
      Lookup/*<Object, UpgradeState>*/ stateMap = Upgrade.getInitialState(lastUpgrade);

      version.setUpgradable(true);
      lastUpgrade.apply(Upgrade.getState(stateMap, lastUpgrade)); // advance to finished current ver

      for (VersionUpgrade u = lastUpgrade.getNext(); u != null; u = u.getNext())
      {
         DataSource dataSource = null;
         version.setVersion(u.getName());
         version.setStep(0);

         if (u.getName() == null)
         {
            u.apply(Upgrade.getState(stateMap, u));

            continue;
         }

         if (u instanceof SchemaUpgrade)
         {
            SchemaUpgrade su = (SchemaUpgrade)u;

            dataSource = su.getDataSource();

            if (dataSourceSet.contains(dataSource))
            {
               PersistenceAdapter adapter = (PersistenceAdapter)dataSource.getComponent().getInstance(m_context);

               adapter.upgrade(su, Upgrade.getState(stateMap, su), version);
            }
            else
            {
               su.apply(Upgrade.getState(stateMap, su));
            }
         }
         else if (u instanceof ScriptUpgrade)
         {
            Function fun = ((ScriptUpgrade)u).getFunction();

            if (fun != null)
            {
               if (u.getNext() == null)
               {
                  // Script upgrade must be run with metadata matching the final or previous data version
                  m_context.getMachine().invoke(fun, (Pair)null);
                  m_context.getUnitOfWork().commit(false); // precommit the UOW
               }
               else
               {
                  throw new UnsupportedOperationException(
                     "Unable to run script upgrade for version " + u.getName() +
                     ", temporarily revert metadata used by the upgrade to version "
                     + u.getName() + " before retrying upgrade.");
               }
            }
         }

         version.setStep(-1);

         for (Iterator itr = dataSourceSet.iterator(); itr.hasNext();)
         {
            DataSource ds = (DataSource)itr.next();

            if (ds != dataSource)
            {
               ((PersistenceAdapter)ds.getComponent().getInstance(m_context))
                  .setVersion(ds.getSchema(), version);
            }
         }
      }

      for (Lookup.Iterator itr = stateMap.valueIterator(); itr.hasNext();)
      {
         ((UpgradeState)itr.next()).end();
      }

      m_context.complete(true);
   }

   /**
    * Gets a data source iterator.
    * @param dataSourceSet The data source set, or null for all.
    */
   public Iterator getDataSourceIterator(Set dataSourceSet)
   {
      if (dataSourceSet != null)
      {
         return dataSourceSet.iterator();
      }

      return m_context.getMetadata().getDataSourceIterator();
   }

   /**
    * Determines if a class is with a root persistence mapping.
    * @param metaclass The class to check.
    * @return True if the class has a root persistence mapping.
    */
   protected static boolean isRoot(Metaclass metaclass)
   {
      for (Metaclass base = metaclass.getBase(); base != null; base = base.getBase())
      {
         base = base.getPersistenceRoot();

         if (base != metaclass.getPersistenceRoot() &&
            base.getPersistenceMapping() != null &&
            base.getPersistenceMapping().isCompatible(
               metaclass.getPersistenceRoot().getPersistenceMapping()))
         {
            return false;
         }
      }

      return true;
   }

   /**
    * TODO: Move to the persistence mapping.
    * Determines if a persistence mapping is writable.
    * @param mapping The persistence mapping to check.
    * @return True if the mapping is writable.
    */
   protected static boolean isWritable(PersistenceMapping mapping)
   {
      if (mapping instanceof RelationalMapping)
      {
         return ((RelationalMapping)mapping).getPrimaryTable().getType() == Table.MANAGED;
      }

      return mapping != null;
   }

   /**
    * TODO: Move to the persistence mapping.
    * Determines if the attribute mapping is writable.
    * @param mapping The attribute mapping to check.
    * @return True if the mapping is writable.
    */
   protected static boolean isWritable(AttributeMapping mapping)
   {
      if (mapping instanceof RelationalPrimitiveMapping)
      {
         return ((RelationalPrimitiveMapping)mapping).getColumn().getTable().getType() == Table.MANAGED;
      }

      if (mapping instanceof RelationalClassMapping)
      {
         return ((RelationalClassMapping)mapping).getSourceKey().getTable().getType() == Table.MANAGED;
      }

      return true;
   }

   /**
    * Connection factory that creates connections writing their output to the specified writer.
    */
   protected static class SQLWriterConnectionFactory
      implements SQLConnectionFactory, SQLConnectionInterceptor
   {
      /**
       * Write statements that would produce a ResultSet.
       */
      protected boolean m_bQueryWrittenThough;

      /**
       * The writer to initialize SQLWriterConnection with.
       */
      Writer m_writer;

      /**
       * Constructor.
       * @param writer The writer to initialize SQLWriterConnection with.
       * @param bQueryWrittenThough Write statements that would produce a ResultSet
       */
      public SQLWriterConnectionFactory(Writer writer, boolean bQueryWrittenThough)
      {
         m_bQueryWrittenThough = bQueryWrittenThough;
         m_writer = writer;
      }

      /**
       * @see nexj.core.persistence.sql.SQLConnectionFactory#getConnection(nexj.core.persistence.sql.SQLAdapter)
       */
      public Connection getConnection(SQLAdapter adapter) throws SQLException
      {
         return new SQLWriterConnection(adapter, m_writer, m_bQueryWrittenThough);
      }

      /**
       * @see nexj.core.persistence.sql.SQLConnectionInterceptor#getConnection(nexj.core.persistence.sql.SQLAdapter, nexj.core.meta.persistence.sql.RelationalDatabaseFragment)
       */
      public Connection getConnection(SQLAdapter adapter, RelationalDatabaseFragment fragment)
         throws SQLException
      {
         return getConnection(adapter);
      }
   }
}