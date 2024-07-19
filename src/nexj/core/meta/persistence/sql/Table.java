// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import nexj.core.meta.Aspect;
import nexj.core.meta.AspectHelper;
import nexj.core.meta.ContextMetadata;
import nexj.core.meta.MetadataCompoundValidationException;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.Pointcut;
import nexj.core.meta.PointcutHelper;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.DataSourceAdapter;
import nexj.core.util.EmptyIterator;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.StringUtil;
import nexj.core.util.UncheckedException;

/**
 * Relational database table metadata.
 */
public final class Table extends RelationalObject implements Aspect, Pointcut
{
   // constants

   /**
    * Managed table - included in the DDL.
    */
   public final static byte MANAGED = 0;

   /**
    * External table - excluded from the DDL.
    */
   public final static byte EXTERNAL = 1;

   /**
    * Cached query result,
    * a.k.a. materialized view, a.k.a. materialized query table, a.k.a. indexed view
    */
   public final static byte QUERY = 2;

   /**
    * View type.
    */
   public final static byte VIEW = 3;

   /**
    * Aspect table - template that is added to other tables
    * according to the pointcuts and aspect overrides.
    */
   public final static byte ASPECT = 4;

   /**
    * Maximum table name length.
    */
   public final static int MAX_NAME_LENGTH = 29;

   // attributes

   /**
    * The table name, which must be unique for a given owner.
    * In contrast, the metadata object stores the full name: ownerName.tableName,
    * which is what is returned from getName().
    */
   protected String m_sTableName;

   /**
    * The table owner name.
    */
   protected String m_sOwnerName;

   /**
    * The table name prefix.
    */
   protected String m_sPrefix;

   /**
    * The table alias.
    */
   protected String m_sAlias;

   /**
    * The tablespace name.
    */
   protected String m_sTablespaceName;

   /**
    * The index tablespace name.
    */
   protected String m_sIndexspaceName;

   /**
    * The long column tablespace name.
    */
   protected String m_sLongspaceName;

   /**
    * The table type, one of the Table.* constants.
    */
   protected byte m_nType;

   /**
    * The long column indicator.
    */
   protected boolean m_bLongColumn;

   /**
    * Should QUERY table contents be automatically updated by the RDBMS from referenced tables.
    */
   protected boolean m_bViewAutoUpdated = true;

   // associations

   /**
    * Map of column names to column objects.
    */
   protected Lookup m_columnMap = new HashTab(8); // of type Column[String]

   /**
    * The collection of the table columns.
    */
   protected List m_columnList = new ArrayList(8); // of type Column

   /**
    * Collection of contained indexes and constraints.
    */
   protected List m_indexList = new ArrayList(4); // of type Index

   /**
    * The primary key of this table.
    */
   protected Index m_primaryKey;

   /**
    * Collection of the foreign keys referencing this table.
    */
   protected List m_relatedKeyList = new ArrayList(4); // of type Index

   /**
    * Map of a metaclass to a primitive mapping array indexed by column number:
    * RelationalPrimitiveMapping[][Metaclass].
    */
   protected Lookup m_mappingMap = new HashTab(2);

   /**
    * The aspect helper.
    */
   protected TableAspectHelper m_aspectHelper;

   /**
    * The pointcut helper.
    */
   protected TablePointcutHelper m_pointcutHelper;

   /**
    * The view SQL script.
    */
   protected SQLScript m_viewScript;

   /**
    * A collection of DDL hints set on the table (lazy init)
    */
   protected Set/*<String>*/ m_hintSet;

   // constructors

   /**
    * Creates a table object.
    * @param schema The relational schema.
    */
   public Table(RelationalSchema schema)
   {
      super(schema);
   }

   // operations

   /**
    * Set the specified DDL hint.
    * @param sHint The hint to set (not null).
    */
   public void addHint(String sHint)
   {
      verifyNotReadOnly();

      if (m_hintSet == null)
      {
         m_hintSet = new HashHolder/*<String>*/(2);
      }

      if (!m_hintSet.add(sHint))
      {
         throw new MetadataException("err.meta.tableHintDup", new Object[]{sHint, getName()});
      }
   }

   /**
    * @return An iterator over all the set hints, or null if no hints set on the table.
    */
   public Iterator/*<String>*/ getHintIterator()
   {
      return (m_hintSet == null || m_hintSet.size() == 0)
             ? EmptyIterator.getInstance() : m_hintSet.iterator();
   }

   /**
    * Check if the specified hint is set.
    * @param sHint The hint to check (not null)
    */
   public boolean isHintEnabled(String sHint)
   {
      return m_hintSet != null && m_hintSet.contains(sHint);
   }

   /**
    * Unset the specified DDL hint.
    * @param sHint The hint to unset (not null).
    */
   public void removeHint(String sHint)
   {
      verifyNotReadOnly();

      if (m_hintSet == null || !m_hintSet.remove(sHint))
      {
         throw new MetadataException("err.meta.tableHintDup", new Object[]{sHint, getName()});
      }
   }

   /**
    * @return Is this a temporary RDBMS-only definition.
    */
   public boolean isTemporary()
   {
      return m_schema == null || m_sName == null || m_schema.findTable(m_sName) != this;
   }

   /**
    * @return If QUERY view contents should be automatically updated.
    */
   public boolean isViewAutoUpdated()
   {
      return m_bViewAutoUpdated;
   }

   /**
    * @param bUpdate If QUERY view contents should be automatically updated.
    */
   public void setViewAutoUpdated(boolean bAutoUpdate)
   {
      verifyNotReadOnly();
      m_bViewAutoUpdated = bAutoUpdate;
   }

   /**
    * @see nexj.core.meta.NamedMetadataObject#setName(java.lang.String)
    */
   public void setName(String sName)
   {
      sName = StringUtil.intern(sName); // make sure always use interned value from setName()

      if (m_sName != null)
      {
         if (m_schema != null)
         {
            if (sName != null)
            {
               Table table = m_schema.findTable(sName);

               if (table != null && table != this) 
               {
                  throw new MetadataException("err.meta.tableDup",
                     new Object[]{sName, m_schema.getDataSource().getName()});
               }
            }

            if (!isTemporary() && m_schema.m_tableMap.remove(m_sName) != null)
            {
               m_schema.m_tableMap.put(sName, this);
            }
         }
      }

      super.setName(sName);

      m_sPrefix = null;

      if (sName == null)
      {
         m_sTableName = null;
         m_sOwnerName = null;
      }
      else
      {
         int i = sName.lastIndexOf('.');

         if (i < 0 && (m_nType == MANAGED || m_nType == QUERY || m_nType == VIEW))
         {
            String sPrefix = m_schema.getPrefix();

            if (sPrefix != null)
            {
               i = sPrefix.lastIndexOf('.');

               if (i < sPrefix.length() - 1)
               {
                  m_sPrefix = sPrefix.substring(i + 1);
               }

               sName = sPrefix + sName;
               i = sName.lastIndexOf('.');
            }
         }

         if (i > 0)
         {
            m_sOwnerName = sName.substring(0, i);
         }
         else
         {
            m_sOwnerName = null;
         }

         m_sTableName = sName.substring(i + 1);

         if (m_nType != ASPECT && m_sTableName.length() > MAX_NAME_LENGTH)
         {
            if ((m_schema == null || m_schema.isPortable()) && !isTemporary())
            {
               throw new MetadataException("err.meta.tableNameLength",
                  new Object[]{m_sTableName, Primitive.createInteger(MAX_NAME_LENGTH)});
            }
         }
      }
   }

   /**
    * Overrides the full table name for non-portable non-managed tables
    * @param sAlias The table alias  to set. Can be null.
    */
   public void setAlias(String sAlias)
   {
      verifyNotReadOnly();

      if (sAlias != null)
      {
         if (m_schema != null && m_schema.isPortable())
         {
            throw new MetadataException("err.meta.portableTableAlias", new Object[]{m_sName});
         }

         if (m_nType != EXTERNAL)
         {
            throw new MetadataException("err.meta.nonExternalTableAlias", new Object[]{m_sName});
         }

         m_sAlias = sAlias;
         m_sPrefix = null;
      }
   }

   /**
    * @return The table alias .
    */
   public String getAlias()
   {
      return m_sAlias;
   }

   /**
    * @return The table name prefix.
    */
   public String getPrefix()
   {
      return m_sPrefix;
   }

   /**
    * @return The table owner name. Can be null.
    */
   public String getOwnerName()
   {
      return m_sOwnerName;
   }

   /**
    * @see nexj.core.meta.persistence.sql.RelationalObject#getObjectName()
    */
   public String getObjectName()
   {
      return m_sTableName;
   }

   /**
    * @return The table name (without the owner name).
    */
   public String getTableName()
   {
      return m_sTableName;
   }

   /**
    * Sets the tablespace name.
    * @param sTablespaceName The tablespace name to set.
    */
   public void setTablespaceName(String sTablespaceName)
   {
      verifyNotReadOnly();
      m_sTablespaceName = sTablespaceName;
   }

   /**
    * @return The tablespace name.
    */
   public String getTablespaceName()
   {
      return m_sTablespaceName;
   }

   /**
    * Sets the index tablespace name.
    * @param sIndexspaceName The index tablespace name to set.
    */
   public void setIndexspaceName(String sIndexspaceName)
   {
      verifyNotReadOnly();
      m_sIndexspaceName = sIndexspaceName;
   }

   /**
    * @return The index tablespace name.
    */
   public String getIndexspaceName()
   {
      return m_sIndexspaceName;
   }

   /**
    * Sets the long column tablespace name.
    * @param sLongspaceName The long column tablespace name to set.
    */
   public void setLongspaceName(String sLongspaceName)
   {
      verifyNotReadOnly();
      m_sLongspaceName = sLongspaceName;
   }

   /**
    * @return The long column tablespace name.
    */
   public String getLongspaceName()
   {
      return m_sLongspaceName;
   }

   /**
    * Sets the table type, one of the Table.* constants.
    * @param nType The table type, one of the Table.* constants to set.
    */
   public void setType(byte nType)
   {
      verifyNotReadOnly();
      m_nType = nType;
   }

   /**
    * @return The table type, one of the Table.* constants.
    */
   public byte getType()
   {
      return m_nType;
   }

   /**
    * @return The table type string representation.
    */
   public String getTypeString()
   {
      switch (m_nType)
      {
         case MANAGED:
            return "managed";

         case EXTERNAL:
            return "external";

         case QUERY:
            return "query";

         case VIEW:
            return "view";

         case ASPECT:
            return "aspect";

         default:
            return null;
      }
   }

   /**
    * Adds a new column to the table.
    * @param column The column to add.
    * @throws MetadataException if a column
    * with the same name already exists.
    */
   public void addColumn(Column column)
   {
      verifyNotReadOnly();

      Object oldColumn = m_columnMap.put(column.getName(), column);
      
      if (oldColumn != null)
      {
         m_columnMap.put(column.getName(), oldColumn);

         throw new MetadataException("err.meta.columnDup", new Object[]{column.getName(), getName()});
      }

      column.setOrdinal(m_columnList.size());
      m_columnList.add(column);
      column.setTable(this);

      if (column.getAllocation() == Column.LOCATOR ||
         column.getType() == Primitive.STRING && column.getPrecision() > Column.MAX_NVARCHAR_PRECISION ||
         column.getType() == Primitive.BINARY && column.getPrecision() > Column.MAX_VARBINARY_PRECISION)
      {
         m_bLongColumn = true;
      }
   }

   /**
    * Removes a column from the table.
    * @param column The column to remove.
    */
   public void removeColumn(Column column)
   {
      verifyNotReadOnly();

      int nColumn = column.getOrdinal();

      assert m_columnList.get(nColumn) == column;
      assert column.getTable() == this;

      m_columnList.remove(nColumn);
      m_columnMap.remove(column.getName());
      column.setOrdinal(-1);

      for (int i = nColumn, nCount = m_columnList.size(); i < nCount; ++i)
      {
         ((Column)m_columnList.get(i)).setOrdinal(i);
      }

      for (Lookup.Iterator itr = m_mappingMap.valueIterator(); itr.hasNext();)
      {
         RelationalPrimitiveMapping[] mappings = (RelationalPrimitiveMapping[])itr.next();

         // Taking into account column removal during the upgrade
         if (nColumn < mappings.length)
         {
            System.arraycopy(mappings, nColumn + 1, mappings, nColumn, mappings.length - nColumn - 1);
            mappings[mappings.length - 1] = null;
         }
      }

      for (int k = 0, n = m_indexList.size(); k < n; ++k)
      {
         Index index = (Index)m_indexList.get(k);
         IndexColumn indexColumn = index.findIndexColumn(column);

         if (indexColumn != null)
         {
            index.removeIndexColumn(indexColumn);
         }
      }
   }

   /**
    * Gets a column by name.
    * @param sName The column name.
    * @return The column object.
    * @throws MetadataLookupException if the column does not exist.
    */
   public Column getColumn(String sName)
   {
      Column column = (Column)m_columnMap.get(sName);
      
      if (column != null)
      {
         return column;
      }

      throw new MetadataLookupException("err.meta.columnLookup", sName, this);
   }

   /**
    * Finds a column by name.
    * @param sName The column name.
    * @return The column object, or null if not found.
    */
   public Column findColumn(String sName)
   {
      return (Column)m_columnMap.get(sName);
   }

   /**
    * Gets a column by ordinal.
    * @param nOrdinal The column ordinal number (0-based).
    * @return The column object.
    */
   public Column getColumn(int nOrdinal)
   {
      return (Column)m_columnList.get(nOrdinal);
   }

   /**
    * @return The column count.
    */
   public int getColumnCount()
   {
      return m_columnList.size();
   }

   /**
    * @return An iterator for the contained column objects.
    */
   public Iterator getColumnIterator()
   {
      return m_columnList.iterator();
   }
   
   /**
    * Adds a new index to the table.
    * @param index The index to add.
    */
   public void addIndex(Index index)
   {
      verifyNotReadOnly();
      index.setTable(this);

      if (!index.isAspect() && index.getIndexColumnCount() == 0)
      {
         throw new MetadataException("err.meta.noIndexColumns");
      }

      if (index.getType() == Index.CLUSTER)
      {
         for (int i = getIndexCount() - 1; i >= 0; --i)
         {
            Index other = getIndex(i);

            if (other.getType() == Index.CLUSTER && other != index)
            {
               throw new MetadataException("err.meta.dupClusteredIndex",
                  new Object[]{index.getName(), getName()});
            }
         }
      }

      if (!isTemporary())
      {
         m_schema.addIndex(index);
      }

      m_indexList.add(index);
   }

   /**
    * Removes an index from the table.
    * @param index The index to remove.
    */
   public void removeIndex(Index index)
   {
      verifyNotReadOnly();

      m_indexList.remove(index);

      if (index == m_primaryKey)
      {
         m_primaryKey = null;
      }

      if (index.m_relatedTable != null)
      {
         index.m_relatedTable.m_relatedKeyList.remove(index);
      }
   }

   /**
    * Finds an index by name.
    * @param sName The index name.
    * @return The index object, or null if not found.
    */
   public Index findIndex(String sName)
   {
      for (int i = 0, n = m_indexList.size(); i < n; ++i)
      {
         Index index = (Index)m_indexList.get(i);

         if (index.getName().equals(sName))
         {
            return index;
         }
      }

      return null;
   }
   
   /**
    * Gets an index by name.
    * @param sName The index name.
    * @return The index object.
    * @throws MetadataLookupException if the index does not exist.
    */
   public Index getIndex(String sName)
   {
      Index index = findIndex(sName);

      if (index == null)
      {
         throw new MetadataLookupException("err.meta.tableIndexLookup", sName, this);
      }

      return index;
   }
   
   /**
    * Gets an index by ordinal number.
    * @param nOrdinal The index ordinal number (0-based).
    * @return The index object.
    */
   public Index getIndex(int nOrdinal)
   {
      return (Index)m_indexList.get(nOrdinal);
   }
   
   /**
    * @return The index count.
    */
   public int getIndexCount()
   {
      return m_indexList.size();
   }
   
   /**
    * @return An iterator for the contained index objects.
    */
   public Iterator getIndexIterator()
   {
      return m_indexList.iterator();
   }
   
   /**
    * Sets the primary key for this table.
    * @param primaryKey The primary key to set.
    */
   public void setPrimaryKey(Index primaryKey)
   {
      verifyNotReadOnly();
      m_primaryKey = primaryKey;
      primaryKey.setUnique(true);
   }

   /**
    * @return The primary key for this table.
    */
   public Index getPrimaryKey()
   {
      return m_primaryKey;
   }

   /**
    * Adds a new related key (foreign key referencing this table) to the table.
    * @param relatedKey The related key (foreign key referencing this table) to add.
    */
   public void addRelatedKey(Index relatedKey)
   {
      verifyNotReadOnly();
      m_relatedKeyList.add(relatedKey);
      relatedKey.setRelatedTable(this);
   }

   /**
    * Gets a related key (foreign key referencing this table) by ordinal number.
    * @param nOrdinal The related key (foreign key referencing this table) ordinal number (0-based).
    * @return The related key (foreign key referencing this table) object.
    */
   public Index getRelatedKey(int nOrdinal)
   {
      return (Index)m_relatedKeyList.get(nOrdinal);
   }

   /**
    * @return The related key (foreign key referencing this table) count.
    */
   public int getRelatedKeyCount()
   {
      return m_relatedKeyList.size();
   }

   /**
    * @return An iterator for the contained related key (foreign key referencing this table) objects.
    */
   public Iterator getRelatedKeyIterator()
   {
      return m_relatedKeyList.iterator();
   }
   
   /**
    * Adds a primitive mapping to the table.
    * @param mapping The primitive mapping to add.
    * @throws MetadataException If the mapping is duplicate.
    */
   public void addMapping(RelationalPrimitiveMapping mapping) throws MetadataException
   {
      verifyNotReadOnly();
      
      assert mapping.getColumn().getTable() == this;
      
      RelationalPrimitiveMapping[] mappings = (RelationalPrimitiveMapping[])
         m_mappingMap.get(mapping.getPersistenceMapping());
      
      if (mappings == null)
      {
         mappings = new RelationalPrimitiveMapping[getColumnCount()];
         m_mappingMap.put(mapping.getPersistenceMapping(), mappings);
      }

      if (mappings[mapping.getColumn().getOrdinal()] != null)
      {
         throw new MetadataException("err.meta.dupColumnMapping",
            new Object[]{mapping.getAttribute().getName(),
               mapping.getAttribute().getMetaclass().getName(),
               mapping.getColumn().getName(), getName(),
               mappings[mapping.getColumn().getOrdinal()].getAttribute().getName()});
      }
      
      mappings[mapping.getColumn().getOrdinal()] = mapping;
   }

   /**
    * Finds the primitive mapping array, indexed by column number, for a relational mapping.
    * @param mapping The relational mapping.
    * @return The mapping array, or null if not found.
    */
   public RelationalPrimitiveMapping[] findMappingArray(RelationalMapping mapping)
   {
      return (RelationalPrimitiveMapping[])m_mappingMap.get(mapping);
   }

   /**
    * @return The mapping array iterator.
    */
   public Iterator getMappingArrayIterator()
   {
      return m_mappingMap.valueIterator();
   }

   /**
    * @return The relational mapping iterator.
    */
   public Iterator getMappingIterator()
   {
      return m_mappingMap.iterator();
   }

   /**
    * Sets the view SQL script.
    * @param viewScript The view SQL script to set.
    */
   public void setViewScript(SQLScript viewScript)
   {
      verifyNotReadOnly();
      m_viewScript = viewScript;
   }

   /**
    * @return The view SQL script.
    */
   public SQLScript getViewScript()
   {
      return m_viewScript;
   }

   /**
    * @see nexj.core.meta.Pointcut#isPointcut()
    */
   public boolean isPointcut()
   {
      return m_nType == MANAGED || m_nType == QUERY || m_nType == VIEW;
   }

   /**
    * @see nexj.core.meta.Pointcut#addAspect(nexj.core.meta.Aspect)
    */
   public void addAspect(Aspect aspect) throws MetadataException
   {
      verifyNotReadOnly();

      if (m_pointcutHelper == null)
      {
         m_pointcutHelper = new TablePointcutHelper(this);
      }

      m_pointcutHelper.addAspect(aspect);
   }

   /**
    * @see nexj.core.meta.Pointcut#removeAspect(nexj.core.meta.Aspect)
    */
   public boolean removeAspect(Aspect aspect)
   {
      verifyNotReadOnly();

      if (m_pointcutHelper != null)
      {
         return m_pointcutHelper.removeAspect(aspect);
      }

      return false;
   }

   /**
    * @see nexj.core.meta.Pointcut#getAspect(int)
    */
   public Aspect getAspect(int nOrdinal)
   {
      return m_pointcutHelper.getAspect(nOrdinal);
   }

   /**
    * @see nexj.core.meta.Pointcut#hasAspect(nexj.core.meta.Aspect)
    */
   public boolean hasAspect(Aspect aspect)
   {
      if (m_pointcutHelper == null)
      {
         return false;
      }

      return m_pointcutHelper.hasAspect(aspect);
   }

   /**
    * @see nexj.core.meta.Pointcut#getAspectCount()
    */
   public int getAspectCount()
   {
      if (m_pointcutHelper == null)
      {
         return 0;
      }

      return m_pointcutHelper.getAspectCount();
   }

   /**
    * @see nexj.core.meta.Pointcut#addAspectOverride(nexj.core.meta.Aspect, boolean)
    */
   public void addAspectOverride(Aspect aspect, boolean bInclusive) throws MetadataException
   {
      verifyNotReadOnly();

      if (isAspect())
      {
         throw new MetadataException("err.meta.nestedTableAspect",
            new Object[]{m_sName});
      }

      if (!aspect.isAspect())
      {
         throw new MetadataException("err.meta.tableAspectType",
            new Object[]{aspect.getName(), m_sName});
      }

      if (m_pointcutHelper == null)
      {
         m_pointcutHelper = new TablePointcutHelper(this);
      }

      m_pointcutHelper.addAspectOverride(aspect, bInclusive);
   }

   /**
    * @see nexj.core.meta.Pointcut#removeAspectOverride(nexj.core.meta.Aspect)
    */
   public final boolean removeAspectOverride(Aspect aspect)
   {
      verifyNotReadOnly();

      if (m_pointcutHelper != null)
      {
         return m_pointcutHelper.removeAspectOverride(aspect);
      }

      return false;
   }

   /**
    * @see nexj.core.meta.Pointcut#findAspectOverride(nexj.core.meta.Aspect)
    */
   public int findAspectOverride(Aspect aspect)
   {
      if (m_pointcutHelper == null)
      {
         return -1;
      }

      return m_pointcutHelper.findAspectOverride(aspect);
   }

   /**
    * @see nexj.core.meta.Pointcut#getAspectOverride(int)
    */
   public Aspect getAspectOverride(int nOrdinal)
   {
      return m_pointcutHelper.getAspectOverride(nOrdinal);
   }

   /**
    * @see nexj.core.meta.Pointcut#isAspectOverrideInclusive(int)
    */
   public boolean isAspectOverrideInclusive(int nOrdinal)
   {
      return m_pointcutHelper.isAspectOverrideInclusive(nOrdinal);
   }

   /**
    * @see nexj.core.meta.Pointcut#getAspectOverrideCount()
    */
   public int getAspectOverrideCount()
   {
      if (m_pointcutHelper == null)
      {
         return 0;
      }

      return m_pointcutHelper.getAspectOverrideCount();
   }

   /**
    * @see nexj.core.meta.Aspect#isAspect()
    */
   public boolean isAspect()
   {
      return m_nType == ASPECT;
   }

   /**
    * @see nexj.core.meta.Aspect#addPointcutPattern(java.lang.String, boolean)
    */
   public void addPointcutPattern(String sPattern, boolean bInclusive)
   {
      verifyNotReadOnly();

      if (!isAspect())
      {
         throw new MetadataException("err.meta.tablePointcut",
            new Object[]{m_sName});
      }

      if (m_aspectHelper == null)
      {
         m_aspectHelper = new TableAspectHelper(this);
      }

      m_aspectHelper.addPointcutPattern(sPattern, bInclusive);
   }

   /**
    * @see nexj.core.meta.Aspect#getPointcutPattern(int)
    */
   public String getPointcutPattern(int nOrdinal)
   {
      return m_aspectHelper.getPointcutPattern(nOrdinal);
   }

   /**
    * @see nexj.core.meta.Aspect#isPointcutPatternInclusive(int)
    */
   public boolean isPointcutPatternInclusive(int nOrdinal)
   {
      return m_aspectHelper.isPointcutPatternInclusive(nOrdinal);
   }

   /**
    * @see nexj.core.meta.Aspect#getPointcutPatternCount()
    */
   public int getPointcutPatternCount()
   {
      if (m_aspectHelper == null)
      {
         return 0;
      }

      return m_aspectHelper.getPointcutPatternCount();
   }

   /**
    * @see nexj.core.meta.Aspect#isMatching(nexj.core.meta.Pointcut)
    */
   public boolean isMatching(Pointcut pointcut)
   {
      if (m_aspectHelper == null)
      {
         return false;
      }
      
      return m_aspectHelper.isMatching(pointcut);
   }

   /**
    * @see nexj.core.meta.Aspect#addTo(nexj.core.meta.Pointcut)
    */
   public void addTo(Pointcut pointcut)
   {
      AspectHelper helper = m_aspectHelper;

      if (helper == null && m_nType == ASPECT)
      {
         helper = new TableAspectHelper(this);
      }

      if (helper != null)
      {
         helper.addTo(pointcut);
      }
   }

   /**
    * @see nexj.core.meta.Aspect#applyTo(nexj.core.meta.Pointcut, int)
    */
   public void applyTo(Pointcut pointcut, int nPass) throws MetadataException
   {
      Table table = (Table)pointcut;

      switch (nPass)
      {
      case 0:
         for (int i = 0, n = getColumnCount(); i < n; ++i)
         {
            table.addColumn(((Column)getColumn(i).clone()));
         }

         break;

      case 1:
         for (int i = 0, n = getIndexCount(); i < n; ++i)
         {
            Index aspectIndex = getIndex(i);

            if (aspectIndex.getType() != Index.ASPECT)
            {
               Index index = aspectIndex.clone(table);

               index.setTable(null);
               index.setName(null);
               index.setTable(table);
               index.setName(aspectIndex.getName(table));

               for (int j = 0, m = index.getIndexColumnCount(); j < m; ++j)
               {
                  IndexColumn indexColumn = index.getIndexColumn(j);

                  indexColumn.setColumn(table.getColumn(indexColumn.getColumn().getName()));
               }

               table.addIndex(index);

               if (index.getRelatedTable() != null)
               {
                  index.getRelatedTable().addRelatedKey(index);
               }
            }
         }

         break;
      }
   }

   /**
    * @see nexj.core.meta.Aspect#removeFrom(nexj.core.meta.Pointcut)
    */
   public boolean removeFrom(Pointcut pointcut)
   {
      if (pointcut.removeAspect(this))
      {
         Table table = (Table)pointcut;

         for (int i = 0, n = getIndexCount(); i < n; ++i)
         {
            Index aspectIndex = getIndex(i);

            if (aspectIndex.getType() != Index.ASPECT)
            {
               table.getSchema().removeIndex(table.getIndex(aspectIndex.getName(table)));
            }
         }

         for (int i = 0, n = getColumnCount(); i < n; ++i)
         {
            table.removeColumn(table.getColumn(getColumn(i).getName()));
         }

         return true;
      }

      return false;
   }

   /**
    * @return True if an identity key generator is used.
    */
   public boolean isIdentityKeyGenerator()
   {
      for (Iterator itr = getMappingIterator(); itr.hasNext();)
      {
         RelationalMapping mapping = (RelationalMapping)itr.next();

         if (mapping != null &&
             mapping.getPrimaryTable() != null &&
             mapping.getPrimaryTable().getName().equals(getName()) &&
             mapping.getKeyGenerator() == RelationalMapping.KEY_GEN_IDENTITY)
         {
            return true;
         }
      }

      return false;
   }

   /**
    * @return The long column indicator.
    */
   public boolean isLongColumn()
   {
      return m_bLongColumn;
   }

   /**
    * @return True if the table contains any case insensitive columns.
    */
   public boolean isCaseInsensitive()
   {
      for (int i = 0, n = getColumnCount(); i != n; ++i)
      {
         if (getColumn(i).isCaseInsensitive())
         {
            return true;
         }
      }
      
      return false;
   }

   /**
    * Computes the primary key parts for all the indexes.
    */
   public void computePrimaryKeyParts()
   {
      for (int i = 0, n = getIndexCount(); i < n; ++i)
      {
         getIndex(i).computePrimaryKeyParts();
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      for (Iterator itr = getColumnIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      super.makeReadOnly();

      ((ArrayList)m_columnList).trimToSize(); // free unused memory
      ((ArrayList)m_indexList).trimToSize(); // free unused memory
      ((ArrayList)m_relatedKeyList).trimToSize(); // free unused memory
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      marker.setTypeName("Table");
      marker.setProperty("dataSource", m_schema.getDataSource().getName());
      marker.setProperty("table", m_sName);
   }

   /**
    * Clones the table (deep copy).
    * @param schema The new schema instance.
    * @see nexj.core.meta.MetadataObject#clone()
    */
   public Table clone(final RelationalSchema schema)
   {
      Table table = (Table)super.clone();

      table.m_schema = schema;
      table.m_columnMap = new HashTab(m_columnMap.size());
      table.m_columnList = new ArrayList(m_columnList.size());
      table.m_indexList = new ArrayList(m_indexList.size());

      for (int i = 0, n = m_columnList.size(); i < n; ++i)
      {
         Column column = (Column)((Column)m_columnList.get(i)).clone();

         column.m_table = table;
         table.m_columnList.add(column);
         table.m_columnMap.put(column.getName(), column);
      }

      for (int i = 0, n = m_indexList.size(); i < n; ++i)
      {
         Index oldIndex = (Index)m_indexList.get(i);
         Index newIndex = oldIndex.clone(table);

         table.m_indexList.add(newIndex);

         if (oldIndex == m_primaryKey)
         {
            table.m_primaryKey = newIndex;
         }

         if (schema != m_schema && !isTemporary())
         {
            schema.m_indexMap.put(newIndex.getName(), newIndex);
         }
      }

      if (m_pointcutHelper != null)
      {
         table.m_pointcutHelper = (TablePointcutHelper)m_pointcutHelper.clone(
            new PointcutHelper.AspectLookup()
            {
               public Aspect get(String sName) throws MetadataException
               {
                  return schema.getTable(sName);
               }
            });

         table.m_pointcutHelper.m_table = table;
      }

      if (m_aspectHelper != null)
      {
         table.m_aspectHelper = (TableAspectHelper)m_aspectHelper.clone();
         table.m_aspectHelper.m_table = table;
      }

      table.m_mappingMap = new HashTab(m_mappingMap.size());

      // make a copy of the Metaclass Attribute->Column mappings since modified on column add/remove
      for (Lookup.Iterator itr = m_mappingMap.iterator(); itr.hasNext();)
      {
         table.m_mappingMap.put(itr.next(), ((RelationalPrimitiveMapping[])itr.getValue()).clone());
      }

      return table;
   }

   /**
    * Create a deep clone of the RDBMS relevant information in the table with null schema.
    * @return The clone of RDBMS relevant information from this table and with null schema.
    */
   public Table cloneTemporary()
   {
      Table table = new Table(m_schema);

      table.copyFrom(this);

      return table;
   }

   /**
    * Overrides the RDBMS relevant information in this table with the information from source table.
    * @param source The source table to get RDBMS relevant information from.
    */
   public void copyFrom(Table source)
   {
      m_sDescription = source.getDescription();
      m_sName = source.getName();
      m_sOwnerName = source.getOwnerName();
      m_sPrefix = source.getPrefix();
      m_sAlias = source.getAlias();
      m_sTableName = source.getTableName();

      setType(source.getType());
      setIndexspaceName(source.getIndexspaceName());
      setLongspaceName(source.getLongspaceName());
      setTablespaceName(source.getTablespaceName());
      m_columnList.clear(); // remove existing columns
      m_columnMap.clear(); // remove existing columns
      m_indexList.clear(); // remove existing indexes
      m_mappingMap.clear(); // remove existing columns
      m_pointcutHelper = null; // remove pointcut columns
      m_relatedKeyList.clear(); // clear related indexes
      m_primaryKey = null; // remove existing PK mapping
      m_viewScript = null; // remove view script

      // clone columns
      for (int i = 0, nCount = source.getColumnCount(); i < nCount; ++i)
      {
         addColumn((Column)source.getColumn(i).clone());
      }

      // clone indexes
      for (int i = 0, nCount = source.getIndexCount(); i < nCount; ++i)
      {
         Index index = source.getIndex(i);
         Index clone = new Index(index.getName(), index.getType(), this);

         clone.setFill(index.getFill());
         clone.setUnique(index.isUnique());

         // clone index columns
         for (int j = 0, nCountCol = index.getIndexColumnCount(); j < nCountCol; ++j)
         {
            IndexColumn column = index.getIndexColumn(j);
            IndexColumn cloneCol =
               new IndexColumn(getColumn(column.getColumn().getOrdinal()), column.isAscending());

            clone.addIndexColumn(cloneCol);
         }

         m_indexList.add(clone);

         // make sure a primary index is still a primary index in the new table
         if (index.isObjectKey())
         {
            setPrimaryKey(clone);
         }
      }

      // clone list of Metaclasses mapped to this table so that isIdentityKeyGenerator() works
      for (Lookup.Iterator itr = source.m_mappingMap.iterator(); itr.hasNext();)
      {
         m_mappingMap.put(itr.next(), ((RelationalPrimitiveMapping[])itr.getValue()).clone());
      }

      // clone view script
      SQLScript script = source.getViewScript();

      if (script != null)
      {
         m_viewScript = new SQLScript();

         for (int i = 0, nCount = script.getStatementCount(); i < nCount; ++i)
         {
            // do not clone the adapter list as that should stay static, hence no need
            m_viewScript.addStatement((SQLStatement)script.getStatement(i).clone());
         }
      }

      if (source.m_prerequisiteSet != null)
      {
         m_prerequisiteSet = (Set)((HashHolder)source.m_prerequisiteSet).clone();
      }

      if (source.m_hintSet != null)
      {
         m_hintSet = (Set)((HashHolder)source.m_hintSet).clone();
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, nexj.core.util.ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      validate(metadata, warnings, null);
   }

   /**
    * This is invoked on objects that have been fully loaded to check the object validity.
    * Used by Upgrade step validation to limit View SQL validation to select set of adapters.
    * @param metadata The root metadata object.
    * @param warnings The exception holder where warnings should be appended. Can be null.
    * @param adapterArray An array of adapters that should be validated (null == schema's adapter).
    * @throws MetadataException if the object is invalid, e.g. with broken referential integrity.
    */
   public void validate(
      ContextMetadata metadata, ExceptionHolder warnings, DataSourceAdapter[] adapterArray)
   {
      super.validate(metadata, warnings);

      if (m_viewScript != null)
      {
         if (m_nType != QUERY && m_nType != VIEW)
         {
            throw new MetadataException("err.meta.viewTableType", new Object[]{m_sName});
         }

         try
         {
            m_viewScript.validate(m_schema, this, adapterArray);
         }
         catch (UncheckedException e)
         {
            MetadataValidationException x = new MetadataValidationException(e);

            setProperties(x);

            throw x;
         }
      }
      else if (getType() == QUERY || getType() == VIEW)
      {
         throw new MetadataException("err.meta.viewTableScript", new Object[]{m_sName});
      }

      // validate that all specified hints are valid/defined
      if (m_hintSet != null && !m_hintSet.isEmpty())
      {
         MetadataCompoundValidationException eh = null;

         for (Iterator/*<String>*/ itr = m_hintSet.iterator(); itr.hasNext();)
         {
            String sHint = itr.next().toString();

            if (!m_schema.isHintSupported(sHint))
            {
               eh = addException(
                  eh, new MetadataException("err.meta.tableHint", new Object[]{sHint, m_sName}));
            }
         }

         if (eh != null)
         {
            throw eh;
         }
      }
   }

   // inner classes

   /**
    * Table-specific aspect helper.
    */
   protected final static class TableAspectHelper extends AspectHelper
   {
      protected Table m_table; 
      
      public TableAspectHelper(Table table)
      {
         m_table = table;
      }
      
      /**
       * @see nexj.core.meta.AspectHelper#getContainer()
       */
      protected Aspect getContainer()
      {
         return m_table;
      }
   }

   /**
    * Table-specific pointcut helper.
    */
   protected final static class TablePointcutHelper extends PointcutHelper
   {
      protected Table m_table;

      public TablePointcutHelper(Table table)
      {
         m_table = table;
      }

      /**
       * @see nexj.core.meta.PointcutHelper#getContainer()
       */
      protected Pointcut getContainer()
      {
         return m_table;
      }

      /**
       * @see nexj.core.meta.PointcutHelper#getContainerType()
       */
      protected String getContainerType()
      {
         return "table";
      }
   }
}
