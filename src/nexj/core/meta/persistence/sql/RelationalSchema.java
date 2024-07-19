// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import nexj.core.meta.Attribute;
import nexj.core.meta.Component;
import nexj.core.meta.ContextMetadata;
import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.DataSourceAdapter;
import nexj.core.meta.persistence.Schema;
import nexj.core.persistence.Converter;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;

/**
 * The relational database schema metadata.
 */
public final class RelationalSchema extends Schema
{
   // attributes

   /**
    * The managed table prefix.
    */
   protected String m_sPrefix;

   /**
    * The default tablespace name.
    */
   protected String m_sTablespaceName;

   /**
    * The default index tablespace name.
    */
   protected String m_sIndexspaceName;

   /**
    * The default long column tablespace name.
    */
   protected String m_sLongspaceName;

   /**
    * The schema access role name.
    */
   protected String m_sRoleName;

   /**
    * The default index page fill factor percent. 0 for the database type default.
    */
   protected byte m_nIndexFill;

   /**
    * The portability flag. If true, the schema
    * objects name length is checked. 
    */
   protected boolean m_bPortable = true;

   // associations

   /**
    * The relational database table map: Table[String].
    */
   protected Lookup m_tableMap = new HashTab(512); 
   
   /**
    * The relational database index map: Index[String].
    */
   protected Lookup m_indexMap = new HashTab(1024);

   /**
    * A map of of SQLObjects (non-table/index) residing in this relational database.
    */
   protected Lookup/*<String, SQLObject>*/ m_objectMap = new HashTab/*<String, SQLObject>*/(256);

   /**
    * Set of hints supported by all adapters (lazy init). Available only for duration of validate().
    */
   protected Set/*<String>*/ m_hintSet;

   /**
    * The version table.
    */
   protected Table m_versionTable;

   // operations

   /**
    * Check if hint is supported by at least one SQLAdapter.
    * This method is valid only during validate().
    * @param sHint The hint to check.
    * @return The hint is supported by at least one SQLAdapter.
    */
   public synchronized boolean isHintSupported(String sHint)
   {
      if (m_hintSet == null) // not null for hint injection by jUnit tests
      {
         m_hintSet = new HashHolder/*<String>*/(4);

         for (Iterator/*<DataSourceAdapter>*/ itr = getDataSource().getType().getAdapterIterator();
              itr.hasNext();)
         {
            try
            {
               Object hints =
                  ((DataSourceAdapter)itr.next()).getClassObject().getDeclaredField("HINTS")
                     .get(null);

               if (hints != null)
               {
                  for (StringTokenizer tokenizer = new StringTokenizer(hints.toString());
                       tokenizer.hasMoreTokens();)
                  {
                     m_hintSet.add(tokenizer.nextToken());
                  }
               }
            }
            catch (Exception e) // ignore exceptions for HINTS not defined
            {
            }
         }
      }

      return m_hintSet.contains(sHint);
   }

   /**
    * Sets the managed table prefix.
    * @param sPrefix The managed table prefix to set.
    */
   public void setPrefix(String sPrefix)
   {
      verifyNotReadOnly();
      m_sPrefix = sPrefix;
   }

   /**
    * @return The managed table prefix.
    */
   public String getPrefix()
   {
      return m_sPrefix;
   }

   /**
    * Sets the default tablespace name.
    * @param sTablespaceName The default tablespace name to set.
    */
   public void setTablespaceName(String sTablespaceName)
   {
      verifyNotReadOnly();
      m_sTablespaceName = sTablespaceName;
   }

   /**
    * @return The default tablespace name.
    */
   public String getTablespaceName()
   {
      return m_sTablespaceName;
   }

   /**
    * Sets the default index tablespace name.
    * @param sIndexspaceName The index tablespace name to set.
    */
   public void setIndexspaceName(String sIndexspaceName)
   {
      verifyNotReadOnly();
      m_sIndexspaceName = sIndexspaceName;
   }

   /**
    * @return The default index tablespace name.
    */
   public String getIndexspaceName()
   {
      return m_sIndexspaceName;
   }

   /**
    * Sets the default long column tablespace name.
    * @param sLongspaceName The default long column tablespace name to set.
    */
   public void setLongspaceName(String sLongspaceName)
   {
      verifyNotReadOnly();
      m_sLongspaceName = sLongspaceName;
   }

   /**
    * @return The default long column tablespace name.
    */
   public String getLongspaceName()
   {
      return m_sLongspaceName;
   }

   /**
    * Sets the schema access role name.
    * @param sRoleName The schema access role name to set.
    */
   public void setRoleName(String sRoleName)
   {
      verifyNotReadOnly();
      m_sRoleName = sRoleName;
   }

   /**
    * @return The schema access role name.
    */
   public String getRoleName()
   {
      return m_sRoleName;
   }

   /**
    * Sets the default index page fill factor percent.
    * @param nIndexFill The default index page fill factor percent to set.
    */
   public void setIndexFill(int nIndexFill)
   {
      verifyNotReadOnly();

      if (nIndexFill < 0 || nIndexFill > 100)
      {
         throw new MetadataException("err.meta.schemaIndexFill",
            new Object[]{Primitive.createInteger(nIndexFill), m_dataSource.getName()});
      }

      m_nIndexFill = (byte)nIndexFill;
   }

   /**
    * @return The default index page fill factor percent.
    */
   public byte getIndexFill()
   {
      return m_nIndexFill;
   }
   
   /**
    * Sets the portability flag.
    * @param bPortable The portability flag to set.
    */
   public void setPortable(boolean bPortable)
   {
      verifyNotReadOnly();
      m_bPortable = bPortable;
   }

   /**
    * @return The portability flag.
    */
   public boolean isPortable()
   {
      return m_bPortable;
   }

   /**
    * Adds a new SQL Object to the schema.
    * @param sqlObject The SQL Object to add.
    * @throws MetadataException if an non-table object with the same name already exists.
    */
   public void addObject(SQLObject sqlObj)
   {
      verifyNotReadOnly();

      Object oldObject = m_objectMap.put(sqlObj.getName(), sqlObj);

      if (oldObject != null)
      {
         m_objectMap.put(sqlObj.getName(), oldObject);

         throw new MetadataException(
            "err.meta.objectDup", new Object[]{sqlObj.getName(), m_dataSource.getName()});
      }

      sqlObj.setSchema(this);
   }

   /**
    * Removes an SQL Object from the schema.
    * @param sqlObj The SQLObject to remove.
    */
   public void removeObject(SQLObject sqlObj)
   {
      verifyNotReadOnly();
      m_objectMap.remove(sqlObj.getName());
   }

   /**
    * Finds an SQL Object by name.
    * @param sName The SQL Object name.
    * @return The SQL Object object, or null if not found.
    */
   public SQLObject findObject(String sName)
   {
      return (SQLObject)m_objectMap.get(sName);
   }

   /**
    * Gets a SQL Object by name.
    * @param sName The SQL Object name.
    * @return The SQL Object object.
    * @throws MetadataLookupException if the SQL Object does not exist.
    */
   public SQLObject getObject(String sName)
   {
      SQLObject obj = (SQLObject)m_objectMap.get(sName);

      if (obj != null)
      {
         return obj;
      }

      throw new MetadataLookupException("err.meta.objectLookup", sName, this);
   }

   /**
    * @return The SQL Object count.
    */
   public int getObjectCount()
   {
      return m_objectMap.size();
   }

   /**
    * @return An iterator for the contained table objects.
    */
   public Iterator getObjectIterator()
   {
      return m_objectMap.valueIterator();
   }

   /**
    * Adds a new table to the schema.
    * @param table The table to add.
    * @throws MetadataException if a table
    * with the same name already exists.
    */
   public void addTable(Table table)
   {
      verifyNotReadOnly();

      Object oldTable = m_tableMap.put(table.getName(), table);

      if (oldTable != null)
      {
         m_tableMap.put(table.getName(), oldTable);

         throw new MetadataException("err.meta.tableDup", new Object[]{table.getName(), m_dataSource.getName()});
      }

      table.setSchema(this);
   }

   /**
    * Removes a table from the schema.
    * @param table The table to remove.
    */
   public void removeTable(Table table)
   {
      verifyNotReadOnly();
      m_tableMap.remove(table.getName());

      for (int i = table.getIndexCount() - 1; i >= 0; --i)
      {
         removeIndex(table.getIndex(i));
      }

      if (table.isAspect())
      {
         for (Iterator itr = getTableIterator(); itr.hasNext();)
         {
            Table pointcut = (Table)itr.next();

            if (pointcut.isPointcut())
            {
               pointcut.removeAspect(table);
               pointcut.removeAspectOverride(table);
            }
         }
      }
   }

   /**
    * Finds a table by name.
    * @param sName The table name.
    * @return The table object, or null if not found.
    */
   public Table findTable(String sName)
   {
      return (Table)m_tableMap.get(sName);
   }
   
   /**
    * Gets a table by name.
    * @param sName The table name.
    * @return The table object.
    * @throws MetadataLookupException if the table does not exist.
    */
   public Table getTable(String sName)
   {
      Table table = (Table)m_tableMap.get(sName);

      if (table != null)
      {
         return table;
      }
      
      throw new MetadataLookupException("err.meta.tableLookup", sName, this);
   }
   
   /**
    * @return The table count.
    */
   public int getTableCount()
   {
      return m_tableMap.size();
   }
   
   /**
    * @return An iterator for the contained table objects.
    */
   public Iterator getTableIterator()
   {
      return m_tableMap.valueIterator();
   }

   /**
    * Sets the version table.
    * @param versionTable The version table to set.
    */
   public void setVersionTable(Table versionTable)
   {
      verifyNotReadOnly();
      m_versionTable = versionTable;
   }

   /**
    * @return The version table.
    */
   public Table getVersionTable()
   {
      return m_versionTable;
   }
   
   /**
    * Adds a new index to the schema.
    * @param index The index to add.
    * @throws MetadataException if an index
    * with the same name already exists.
    */
   public void addIndex(Index index)
   {
      verifyNotReadOnly();

      Object oldIndex = m_indexMap.put(index.getName(), index);
      
      if (oldIndex != null)
      {
         m_indexMap.put(index.getName(), oldIndex);
         
         throw new MetadataException("err.meta.indexDup", new Object[]{index.getName(), m_dataSource.getName()});
      }
   }

   /**
    * Removes an index from the schema.
    * @param index The index to remove.
    */
   public void removeIndex(Index index)
   {
      verifyNotReadOnly();

      m_indexMap.remove(index.getName());
      index.getTable().removeIndex(index);

      if (index.isAspect())
      {
         for (Iterator itr = getIndexIterator(); itr.hasNext();)
         {
            Index pointcut = (Index)itr.next();

            if (pointcut.isPointcut())
            {
               pointcut.removeAspect(index);
               pointcut.removeAspectOverride(index);
            }
         }
      }
   }

   /**
    * Finds an index by name.
    * @param sName The index name.
    * @return The index object, or null if not found.
    */
   public Index findIndex(String sName)
   {
      return (Index)m_indexMap.get(sName);
   }

   /**
    * Gets an index by name.
    * @param sName The index name.
    * @return The index object.
    * @throws MetadataLookupException if the index does not exist.
    */
   public Index getIndex(String sName)
   {
      Index index = (Index)m_indexMap.get(sName);
      
      if (index != null)
      {
         return index;
      }
      
      throw new MetadataLookupException("err.meta.schemaIndexLookup", sName, this);
   }
   
   /**
    * @return The index count.
    */
   public int getIndexCount()
   {
      return m_indexMap.size();
   }
   
   /**
    * @return An iterator for the contained index objects.
    */
   public Iterator getIndexIterator()
   {
      return m_indexMap.valueIterator();
   }
   
   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      for (Iterator itr = getTableIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getIndexIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      super.makeReadOnly();
   }

   /**
    * Generates metaclasses from the schema.
    * @param sPrefix The prefix to add to the generated class names. Can be null.
    * @param nameMap Map of table name to class name for overriding the default mapping rule.
    *    More than one table can map to the same class. Can be null.
    * @param primarySet The primary table name set. Can be null.
    */
   public void generateMetaclasses(String sPrefix, Map nameMap, final Set primarySet)
   {
      final List fixupList = new ArrayList();
      final List fixupList2 = new ArrayList();
      List tableList = null;

      if (primarySet != null)
      {
         tableList = new ArrayList(getTableCount());

         for (Iterator itr = getTableIterator(); itr.hasNext();)
         {
            tableList.add(itr.next());
         }

         // Sort the primary tables first
         Collections.sort(tableList, new Comparator()
         {
            public int compare(Object left, Object right)
            {
               return ((primarySet.contains(((Table)left).getName())) ? 0 : 1) -
                  ((primarySet.contains(((Table)right).getName())) ? 0 : 1);
            }
         });
      }

      for (Iterator itr = (tableList != null) ? tableList.iterator() : getTableIterator(); itr.hasNext();)
      {
         final Table table = (Table)itr.next();

         if (!table.isAspect())
         {
            final Metaclass metaclass = getMetaclass(getClassName(table.getTableName(), sPrefix, nameMap));
            final RelationalMapping relationalMapping = (RelationalMapping)metaclass.getPersistenceMapping();
            
            if (relationalMapping.getPrimaryTable() == null)
            {
               relationalMapping.setPrimaryTable(table);
            }
   
            metaclass.setDescription(table.getDescription());
   
            for (int i = 0; i < table.getColumnCount(); ++i)
            {
               final Column column = table.getColumn(i);
               final Attribute attribute = new Attribute(getAttributeName(column.getName(), metaclass));
               boolean bRequired = !column.isNullable() && column.getType() != Primitive.STRING;

               if (bRequired)
               {
                  Component component = column.getConverter();

                  if (component != null)
                  {
                     try
                     {
                        bRequired = (((Converter)component.getInstance(null)).getInverseFunction().invoke(null) == null);
                     }
                     catch (Throwable e)
                     {
                        // Still required if an error occurs
                     }
                  }
               }

               attribute.setDescription(column.getDescription());
               attribute.setMetaclass(metaclass);
               attribute.setDeclarator(metaclass);
               attribute.setRootDeclarator(metaclass);
               attribute.setType(column.getType());
               attribute.setRequired(bRequired);
               attribute.setOrdinal(metaclass.getAttributeCount());
               metaclass.addAttribute(attribute);

               final RelationalPrimitiveMapping mapping = new RelationalPrimitiveMapping();

               mapping.setColumn(column);
               mapping.setAttribute(attribute);

               fixupList2.add(new Fixup()
               {
                  public void fixup()
                  {
                     relationalMapping.addAttributeMapping(mapping);
                  }
               });
            }
            
            for (int i = 0; i < table.getRelatedKeyCount(); ++i)
            {
               final Index index = table.getRelatedKey(i);
   
               if (!index.getTable().isAspect())
               {
                  final Index primaryKey = table.getPrimaryKey();
                  final String sClassName = getClassName(index.getTable().getTableName(), sPrefix, nameMap);
                  final Attribute attribute = new Attribute(getAttributeName(sClassName, metaclass));
      
                  attribute.setMetaclass(metaclass);
                  attribute.setDeclarator(metaclass);
                  attribute.setRootDeclarator(metaclass);
                  attribute.setCollection(true);
                  attribute.setOrdinal(metaclass.getAttributeCount());
                  metaclass.addAttribute(attribute);
      
                  final RelationalClassMapping mapping = new RelationalClassMapping();
      
                  mapping.setAttribute(attribute);
                  mapping.setSourceKey(primaryKey);
                  mapping.setDestinationKey(index);
      
                  fixupList.add(new Fixup()
                  {
                     public void fixup()
                     {
                        final Metaclass mc = getMetadata().getMetaclass(sClassName);
                        
                        attribute.setType(mc);
                        mapping.setMapping(mc.getPersistenceMapping());
                        
                        Attribute a = new Attribute(getAttributeName(metaclass.getName(), mc));
                        
                        a.setMetaclass(mc);
                        a.setDeclarator(mc);
                        a.setRootDeclarator(mc);
                        a.setOrdinal(mc.getAttributeCount());
                        a.setType(metaclass);
                        a.setReverse(attribute);
                        attribute.setReverse(a);
                        mc.addAttribute(a);
      
                        final RelationalClassMapping m = new RelationalClassMapping();
      
                        m.setAttribute(a);
                        m.setSourceKey(index);
                        m.setDestinationKey(primaryKey);
                        
                        fixupList2.add(new Fixup()
                        {
                           public void fixup()
                           {
                              relationalMapping.addAttributeMapping(mapping);
                              m.setMapping(metaclass.getPersistenceMapping());
                              ((RelationalMapping)mc.getPersistenceMapping()).addAttributeMapping(m);                  
                           }
                        });
                     }
                  });
               }
            }
         }
      }

      for (int i = 0; i < fixupList.size(); ++i)
      {
         ((Fixup)fixupList.get(i)).fixup();
      }

      for (int i = 0; i < fixupList2.size(); ++i)
      {
         ((Fixup)fixupList2.get(i)).fixup();
      }
   }
   
   /**
    * Gets a metaclass by name, creating a new one if not found.
    * @param sName The class name.
    * @return The metaclass.
    */
   protected Metaclass getMetaclass(String sName)
   {
      Metaclass metaclass = getMetadata().findMetaclass(sName);

      if (metaclass == null)
      {
         metaclass = new Metaclass(sName);
         getMetadata().addMetaclass(metaclass);
      }

      if (metaclass.getPersistenceMapping() == null)
      {
         RelationalMapping mapping = new RelationalMapping();
         
         metaclass.setPersistenceMapping(mapping);
         mapping.setDataSource(m_dataSource);
         mapping.setMetaclass(metaclass);
      }

      return metaclass;
   }
   
   /**
    * Gets a class name from table name.
    * @param sTableName The table name.
    * @param sPrefix The optional prefix to add to the class name. Can be null.
    * @param nameMap Maps table name to class name to override
    *    the default mapping rule. Can be null.
    * @return The formatted name.
    */
   public static String getClassName(String sTableName, String sPrefix, Map nameMap)
   {
      String sClassName = null;
         
      if (nameMap != null)
      {
         sClassName = (String)nameMap.get(sTableName);
      }
         
      if (sClassName == null)
      {
         sClassName = formatName(sTableName.replace('.', '_'), true);
      }
         
      if (sPrefix != null)
      {
         sClassName = sPrefix + sClassName;
      }

      return sClassName;
   }
   
   /**
    * Gets an attribute name from column name and ensures that there are no duplicates.
    * @param sColumnName The raw column name.
    * @param metaclass The metaclass to which the attribute applies.
    */
   private static String getAttributeName(String sColumnName, Metaclass metaclass)
   {
      String sName = formatName(sColumnName, false);
      
      for (int i = 0; ; ++i)
      {
         String sAttributeName;
         
         if (i == 0)
         {
            sAttributeName = sName;
         }
         else
         {
            sAttributeName = sName + (i + 1);
         }
         
         if (metaclass.findAttribute(sAttributeName) == null)
         {
            return sAttributeName;
         }
      }
   }

   /**
    * Formats a database object name as a class or attribute name by
    * removing the underscores and lowercasing the uppercase identifiers.
    * @param sName The name to format.
    * @param bCapitalized True if the first formatted character must be uppercase.
    * @return The formatted name.
    */
   private static String formatName(String sName, boolean bCapitalized)
   {
      if (sName.equals(sName.toUpperCase(Locale.ENGLISH)))
      {
         sName = sName.toLowerCase(Locale.ENGLISH);
      }

      StringBuffer buf = new StringBuffer(sName);
      int i = 0;

      for (;;)
      {
         i = buf.indexOf("_", i);
         
         if (i < 0)
         {
            break;
         }
         
         buf.delete(i, i + 1);
         
         if (i < buf.length())
         {
            buf.setCharAt(i, Character.toUpperCase(buf.charAt(i)));
         }
         else
         {
            break;
         }
      }
      
      if (buf.length() == 0)
      {
         return sName;
      }

      if (bCapitalized)
      {
         buf.setCharAt(0, Character.toUpperCase(buf.charAt(0)));
      }
      else
      {
         for (i = 0; i < buf.length(); ++i)
         {
            if (Character.isUpperCase(buf.charAt(i)))
            {
               buf.setCharAt(i, Character.toLowerCase(buf.charAt(i)));
            }
            else
            {
               break;
            }
         }
      }

      return buf.toString();
   }

   /**
    * @see nexj.core.meta.MetadataObject#clone()
    */
   public Object clone()
   {
      RelationalSchema schema = (RelationalSchema)super.clone();

      schema.m_tableMap = new HashTab(m_tableMap.size());
      schema.m_indexMap = new HashTab(m_indexMap.size());
      schema.m_objectMap = new HashTab/*<String, SQLObject>*/(m_objectMap.size());

      for (Lookup.Iterator itr = m_tableMap.valueIterator(); itr.hasNext();)
      {
         Table table = (Table)itr.next();

         if (table.isAspect())
         {
            table = table.clone(schema);
            schema.m_tableMap.put(table.getName(), table);
         }
      }

      for (Lookup.Iterator itr = m_tableMap.valueIterator(); itr.hasNext();)
      {
         Table table = (Table)itr.next();

         if (!table.isAspect())
         {
            table = table.clone(schema);
            schema.m_tableMap.put(table.getName(), table);
         }
      }

      if (m_versionTable != null)
      {
         schema.m_versionTable = (Table)schema.m_tableMap.get(m_versionTable.getName());
      }

      for (Lookup.Iterator itr = schema.m_tableMap.valueIterator(); itr.hasNext();)
      {
         Table table = (Table)itr.next();

         for (int i = 0, n = table.m_indexList.size(); i < n; ++i)
         {
            Index index = (Index)table.m_indexList.get(i);

            if (index.m_relatedTable != null)
            {
               index.m_relatedTable = (Table)schema.m_tableMap.get(index.m_relatedTable.getName());
            }
         }

         List relatedKeyList = new ArrayList(table.m_relatedKeyList.size());

         for (int i = 0, n = relatedKeyList.size(); i < n; ++i)
         {
            relatedKeyList.add(schema.m_indexMap.get(((Index)table.m_relatedKeyList.get(i)).getName()));
         }

         table.m_relatedKeyList = relatedKeyList;
      }

      // create seperate instances of SQLObjects in the new schema
      for (Lookup.Iterator itr = m_objectMap.valueIterator(); itr.hasNext();)
      {
         SQLObject obj = (SQLObject)itr.next();

         obj = obj.clone(schema);
         schema.m_objectMap.put(obj.getName(), obj);
      }
 
      synchronized (this)
      {
         if (m_hintSet != null)
         {
            schema.m_hintSet = (Set)(((HashHolder)m_hintSet).clone());
         }
      }

      return schema;
   }

   /**
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, nexj.core.util.ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      super.validate(metadata, warnings);
      validate(m_indexMap.valueIterator(), metadata, warnings);
      validate(m_objectMap.valueIterator(), metadata, warnings);
      validate(m_tableMap.valueIterator(), metadata, warnings);
   }

   // inner classes

   /**
    * Interface for fixing up references.
    */
   private interface Fixup
   {
      /**
       * Fixes up the reference.
       */
      void fixup();
   }
}
