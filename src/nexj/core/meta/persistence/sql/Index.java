// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Aspect;
import nexj.core.meta.AspectHelper;
import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.NamedMetadataObject;
import nexj.core.meta.Pointcut;
import nexj.core.meta.PointcutHelper;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.Key;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.util.PrintWriter;
import nexj.core.util.Printable;
import nexj.core.util.StringUtil;

/**
 * Index or constraint defined for a given table.
 */
public final class Index extends NamedMetadataObject implements Key, Aspect, Pointcut, Printable
{
   // constants

   /**
    * Maximum index name length.
    */
   public final static int MAX_NAME_LENGTH = 30;

   // constants

   /**
    * The index is not created in the physical schema.
    */
   public final static byte VIRTUAL = 0;

   /**
    * The index is not created in the physical schema,
    * but counts as a sort key.
    */
   public final static byte QUERY = 1;

   /**
    * The index is a template prepended to other indexes,
    * according to their aspect specification.
    */
   public final static byte ASPECT = 2;

   /**
    * Full-Text Search index.
    */
   public final static byte TEXT = 3;

   /**
    * B-tree index.
    */
   public final static byte BTREE = 4;

   /**
    * Cluster index.
    */
   public final static byte CLUSTER = 5;

   // attributes

   /**
    * The index type (BTREE, VIRTUAL, ...).
    */
   protected byte m_nType = BTREE;

   /**
    * The index page fill factor percent. 0 for the database type default. -1 for the schema default.
    */
   protected byte m_nFill = -1;

   /**
    * The unique constraint flag.
    * True if this is a unique constraint.
    */
   protected boolean m_bUnique;
   
   /**
    * True if this key is a primary key part.
    */
   protected boolean m_bPrimaryKeyPart;

   /**
    * True if the non-unique key is shared between two or more classes.
    */
   protected boolean m_bMultiplexed;

   // associations

   /**
    * The containing table.
    */
   protected Table m_table;
   
   /**
    * The index column collection.
    */
   protected List m_indexColumnList = new ArrayList(4); // of type IndexColumn

   /**
    * The related table (table providing the primary key for this foreign key).
    */
   protected Table m_relatedTable;

   /**
    * The last mapped metaclass (for multiplexing detection).
    */
   protected Metaclass m_lastMetaclass;

   /**
    * The aspect helper.
    */
   protected IndexAspectHelper m_aspectHelper;

   /**
    * The pointcut helper.
    */
   protected IndexPointcutHelper m_pointcutHelper;

   // constructors

   /**
    * Creates an index with a given name.
    * @param sName The index name.
    * @param nType The index type, one of the Index.* constants.
    * @param table The index table.
    */
   public Index(String sName, byte nType, Table table)
   {
      m_table = table;
      m_nType = nType;
      setName(sName);
   }

   /**
    * Creates an index.
    * @param table The index table.
    */
   public Index(Table table)
   {
      m_table = table;
   }

   // operations

   /**
    * @see nexj.core.meta.NamedMetadataObject#setName(java.lang.String)
    */
   public void setName(String sName)
   {
      sName = StringUtil.intern(sName); // make sure always use interned value from setName()

      if (m_nType != ASPECT && sName != null)
      {
         int nMaxLen = MAX_NAME_LENGTH;

         if (m_table != null && m_table.getSchema() != null)
         {
            String sPrefix = m_table.getSchema().getPrefix();

            if (sPrefix != null)
            {
               nMaxLen -= sPrefix.length() - sPrefix.lastIndexOf('.') - 1;

               if (nMaxLen < 0)
               {
                  nMaxLen = 0;
               }
            }
         }

         if (sName.length() > nMaxLen)
         {
            if (m_table == null || !m_table.isAspect() &&
                (m_table.getSchema() == null || m_table.getSchema().isPortable()) &&
                !m_table.isTemporary())
            {
               throw new MetadataException("err.meta.indexNameLength",
                  new Object[]{sName, Primitive.createInteger(nMaxLen)});
            }
         }
      }

      if (m_sName != null)
      {
         if (m_table != null && m_table.getSchema() != null)
         {
            if (sName != null)
            {
               Index index = m_table.getSchema().findIndex(sName);

               if (index != null && index != this)
               {
                  throw new MetadataException("err.meta.indexDup", new Object[]{sName,
                     m_table.getSchema().getDataSource().getName()});
               }
            }

            if (!m_table.isTemporary() &&
                m_table.getSchema().m_indexMap.remove(m_sName) != null &&
                sName != null)
            {
               m_table.getSchema().m_indexMap.put(sName, this);
            }
         }
      }

      super.setName(sName);
   }

   /**
    * Generates a index name for a given table.
    * @param table The new table.
    * @return The new index name. 
    */
   public String getName(Table table)
   {
      String sSuffix = m_sName;
      int i = sSuffix.lastIndexOf('.');

      if (i < 0)
      {
         sSuffix = '.' + sSuffix;
      }
      else
      {
         sSuffix = sSuffix.substring(i);
      }

      return table.getName() + sSuffix;
   }

   /**
    * Sets the index type (BTREE, VIRTUAL, ...).
    * @param nType The index type (BTREE, VIRTUAL, ...) to set.
    */
   public void setType(byte nType)
   {
      verifyNotReadOnly();
      m_nType = nType;
   }

   /**
    * @return The index type (BTREE, VIRTUAL, ...).
    */
   public byte getType()
   {
      return m_nType;
   }

   /**
    * @return A string representation of the type.
    */
   public String getTypeString()
   {
      switch (m_nType)
      {
         case BTREE:
            return "btree";

         case CLUSTER:
            return "cluster";

         case VIRTUAL:
            return "virtual";

         case QUERY:
            return "query";

         case ASPECT:
            return "aspect";

         default:
            return null;
      }
   }

   /**
    * Sets the unique constraint flag.
    * @param bUnique The unique constraint flag to set.
    */
   public void setUnique(boolean bUnique)
   {
      verifyNotReadOnly();
      m_bUnique = bUnique;
   }

   /**
    * @return The unique constraint flag. True if the constraint is unique.
    */
   public boolean isUnique()
   {
      return m_bUnique;
   }

   /**
    * Sets the index page fill factor percent.
    * @param nFill The default index page fill factor percent to set.
    */
   public void setFill(int nFill)
   {
      verifyNotReadOnly();

      if (nFill < -1 || nFill > 100)
      {
         throw new MetadataException("err.meta.indexFill",
            new Object[]{Primitive.createInteger(nFill), getName()});
      }

      m_nFill = (byte)nFill;
   }

   /**
    * @return The index page fill factor percent.
    */
   public byte getFill()
   {
      return m_nFill;
   }

   /**
    * Sets the containing table.
    * @param table The containing table.
    */
   public void setTable(Table table)
   {
      verifyNotReadOnly();
      m_table = table;
   }

   /**
    * @return The containing table.
    */
   public Table getTable()
   {
      return m_table;
   }

   /**
    * Adds a new index column to the index in a specified position.
    * @param nOrdinal The column ordinal number, 0-based.
    * @param indexColumn The index column to add.
    */
   public void addIndexColumn(int nOrdinal, IndexColumn indexColumn)
   {
      verifyNotReadOnly();
      indexColumn.setOrdinal(nOrdinal);

      if (findIndexColumn(indexColumn.getColumn()) != null)
      {
         throw new MetadataException("err.meta.indexColumnDup",
            new Object[]{indexColumn.getColumn().getName(), getName()});
      }

      // address TT #41674 with arbitrary limits defined in Column
      if (getType() != TEXT && // TEXT indexes may contain LOB columns
          indexColumn.getColumn().isLOB(Column.MAX_NVARCHAR_PRECISION,
                                        Column.MAX_VARBINARY_PRECISION))
      {
         throw new MetadataException("err.meta.indexColumnLocator",
            new Object[]{indexColumn.getColumn().getName(), getName()});
      }

      m_indexColumnList.add(nOrdinal, indexColumn);
      indexColumn.setIndex(this);

      for (int i = nOrdinal + 1, n = m_indexColumnList.size(); i < n; ++i)
      {
         ((IndexColumn)m_indexColumnList.get(i)).setOrdinal(i);
      }
   }

   /**
    * Adds a new index column to the index.
    * @param indexColumn The index column to add.
    */
   public void addIndexColumn(IndexColumn indexColumn)
   {
      addIndexColumn(m_indexColumnList.size(), indexColumn);
   }

   /**
    * Removes an index column.
    * @param indexColumn The index column to remove.
    */
   public void removeIndexColumn(IndexColumn indexColumn)
   {
      verifyNotReadOnly();

      int i = indexColumn.getOrdinal();

      assert m_indexColumnList.get(i) == indexColumn;

      m_indexColumnList.remove(i);
      indexColumn.setOrdinal(-1);

      for (int n = m_indexColumnList.size(); i < n; ++i)
      {
         ((IndexColumn)m_indexColumnList.get(i)).setOrdinal(i);
      }
   }

   /**
    * Gets a index column by ordinal number.
    * @param nOrdinal The index column ordinal number (0-based).
    * @return The index column object.
    */
   public IndexColumn getIndexColumn(int nOrdinal)
   {
      return (IndexColumn)m_indexColumnList.get(nOrdinal);
   }

   /**
    * @return The index column count.
    */
   public int getIndexColumnCount()
   {
      return m_indexColumnList.size();
   }

   /**
    * @return An iterator for the contained index column objects.
    */
   public Iterator getIndexColumnIterator()
   {
      return m_indexColumnList.iterator();
   }

   /**
    * Finds an index column object by column.
    * @param column The column by which to search.
    * @return The index column object, or null if not found.
    */
   public IndexColumn findIndexColumn(Column column)
   {
      for (int i = 0, n = getIndexColumnCount(); i < n; ++i)
      {
         IndexColumn indexColumn = getIndexColumn(i);

         if (indexColumn.getColumn() == column)
         {
            return indexColumn;
         }
      }

      return null;
   }

   /**
    * Finds an index column object by column name.
    * @param sName The column name by which to search.
    * @return The index column object, or null if not found.
    */
   public IndexColumn findIndexColumn(String sName)
   {
      for (int i = 0, n = getIndexColumnCount(); i < n; ++i)
      {
         IndexColumn indexColumn = getIndexColumn(i);

         if (indexColumn.getColumn().getName().equals(sName))
         {
            return indexColumn;
         }
      }

      return null;
   }

   /**
    * Sets the related table (table providing the primary key for this foreign key).
    * @param relatedTable The related table (table providing the primary key for this foreign key) to set.
    * @throws MetadataException if the related table is an aspect.
    */
   public void setRelatedTable(Table relatedTable) throws MetadataException
   {
      verifyNotReadOnly();
      
      if (relatedTable != null &&
         relatedTable.isAspect())
      {
         throw new MetadataException("err.meta.persistence.sql.relatedTableAspect",
            new Object[]{relatedTable.getTableName(), m_sName, m_table.getSchema().getDataSource().getName()});
      }

      m_relatedTable = relatedTable;
   }

   /**
    * @return The related table (table providing the primary key for this foreign key).
    */
   public Table getRelatedTable()
   {
      return m_relatedTable;
   }
   
   /**
    * Checks if this key can be used in a natural join with another key.
    * @param index The index to compare against this index.
    * @return true if the keys are compatible.
    */
   public boolean isCompatible(Index index)
   {
      return PersistenceMapping.compatible(this, index);
   }

   /**
    * @see nexj.core.meta.persistence.Key#getPartType(int)
    */
   public Primitive getPartType(int nOrdinal)
   {
      return getIndexColumn(nOrdinal).getColumn().getType();
   }

   /**
    * @see nexj.core.meta.persistence.Key#isPartAscending(int)
    */
   public boolean isPartAscending(int nOrdinal)
   {
      return getIndexColumn(nOrdinal).isAscending();
   }

   /**
    * @see nexj.core.meta.persistence.Key#getPartCount()
    */
   public int getPartCount()
   {
      return getIndexColumnCount();
   }
   
   /**
    * @see nexj.core.meta.persistence.Key#isObjectKey()
    */
   public boolean isObjectKey()
   {
      return this == m_table.getPrimaryKey();
   }

   /**
    * @see nexj.core.meta.persistence.Key#isObjectKeyPart()
    */
   public boolean isObjectKeyPart()
   {
      return m_bPrimaryKeyPart;
   }

   /**
    * @see nexj.core.meta.persistence.Key#getObjectKeyPartOrdinal(int)
    */
   public int getObjectKeyPartOrdinal(int nOrdinal)
   {
      return getIndexColumn(nOrdinal).getPrimaryKeyPartOrdinal();
   }

   /**
    * Computes the primary key parts.
    */
   public void computePrimaryKeyParts()
   {
      verifyNotReadOnly();
      
      Index pk = m_table.getPrimaryKey();

      if (pk == null)
      {
         return;
      }

      m_bPrimaryKeyPart = true;

      if (this == pk)
      {
         for (int i = 0, nCount = getIndexColumnCount(); i < nCount; ++i)
         {
            IndexColumn indexColumn = getIndexColumn(i);
            
            indexColumn.setPrimaryKeyPartOrdinal(i);
            indexColumn.getColumn().setPrimary(true);
         }
      }
      else
      {
         for (int i = 0, nCount = getIndexColumnCount(); i < nCount; ++i)
         {
            IndexColumn icol = getIndexColumn(i);
            IndexColumn icolPK = pk.findIndexColumn(icol.getColumn());
            
            if (icolPK == null)
            {
               m_bPrimaryKeyPart = false;
            }
            else
            {
               icol.setPrimaryKeyPartOrdinal(icolPK.getOrdinal());
            }
         }
      }
   }

   /**
    * Adds a mapped attribute to the key.
    * @param attribute The attribute to add.
    */
   public void addAttribute(Attribute attribute)
   {
      verifyNotReadOnly();

      Metaclass metaclass = (Metaclass)attribute.getType();

      if (m_table.isAspect() != (metaclass instanceof Aspect && ((Aspect)metaclass).isAspect()))
      {
         throw new MetadataException((m_table.isAspect()) ?
            "err.meta.persistence.sql.classKeyAspectMismatch" :
            "err.meta.persistence.sql.classAspectKeyMismatch",
            new Object[]{attribute.getName(), metaclass.getName(),
               m_sName, m_table.getName()});
      }

      if (!m_bUnique)
      {
         if (m_lastMetaclass != null)
         {
            if (metaclass != m_lastMetaclass)
            {
               m_bMultiplexed = true;
            }
         }
         else
         {
            m_lastMetaclass = metaclass;
         }
         
         m_bMultiplexed |= m_bPrimaryKeyPart;
      }
   }

   /**
    * @return True if the non-unique key is shared between two or more metaclasses.
    */
   public boolean isMultiplexed()
   {
      return m_bMultiplexed;
   }

   /**
    * @see nexj.core.meta.persistence.Key#setMapped()
    */
   public void setMapped()
   {
      for (int i = getIndexColumnCount() - 1; i >= 0; --i)
      {
         getIndexColumn(i).getColumn().setCaseInsensitive(false);
      }
   }

   /**
    * @see nexj.core.meta.Pointcut#isPointcut()
    */
   public boolean isPointcut()
   {
      return m_nType != ASPECT;
   }

   /**
    * @see nexj.core.meta.Pointcut#addAspect(nexj.core.meta.Aspect)
    */
   public void addAspect(Aspect aspect) throws MetadataException
   {
      verifyNotReadOnly();

      if (m_pointcutHelper == null)
      {
         m_pointcutHelper = new IndexPointcutHelper(this);
      }

      m_pointcutHelper.addAspect(aspect);
   }

   /**
    * @see nexj.core.meta.Pointcut#removeAspect(nexj.core.meta.Aspect)
    */
   public boolean removeAspect(Aspect aspect)
   {
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
         throw new MetadataException("err.meta.nestedIndexAspect",
            new Object[]{m_sName});
      }

      if (!aspect.isAspect())
      {
         throw new MetadataException("err.meta.indexAspectType",
            new Object[]{aspect.getName(), m_sName});
      }

      if (m_pointcutHelper == null)
      {
         m_pointcutHelper = new IndexPointcutHelper(this);
      }

      m_pointcutHelper.addAspectOverride(aspect, bInclusive);
   }

   /**
    * @see nexj.core.meta.Pointcut#removeAspectOverride(nexj.core.meta.Aspect)
    */
   public final boolean removeAspectOverride(Aspect aspect)
   {
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
         throw new MetadataException("err.meta.indexPointcut",
            new Object[]{m_sName});
      }

      if (m_aspectHelper == null)
      {
         m_aspectHelper = new IndexAspectHelper(this);
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
         helper = new IndexAspectHelper(this);
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
      Index index = (Index)pointcut;

      for (int i = 0, n = getIndexColumnCount(); i < n; ++i)
      {
         IndexColumn indexColumn = getIndexColumn(i);

         index.addIndexColumn(i, new IndexColumn(
            index.getTable().getColumn(indexColumn.getColumn().getName()),
               indexColumn.isAscending()));
      }
   }

   /**
    * @see nexj.core.meta.Aspect#removeFrom(nexj.core.meta.Pointcut)
    */
   public boolean removeFrom(Pointcut pointcut)
   {
      if (pointcut.removeAspect(this))
      {
         Index index = (Index)pointcut;

         for (int i = 0, n = getIndexColumnCount(); i < n; ++i)
         {
            IndexColumn indexColumn = index.findIndexColumn(getIndexColumn(i).getColumn().getName());

            if (indexColumn != null)
            {
               index.removeIndexColumn(indexColumn);
            }
         }

         return true;
      }

      return false;
   }

   /**
    * @return True if the index is inherited.
    */
   public boolean isInherited()
   {
      for (int i = 0, n = m_table.getAspectCount(); i < n; ++i)
      {
         Table aspect = (Table)m_table.getAspect(i);

         for (int k = 0, m = aspect.getIndexCount(); k < m; ++k)
         {
            if (m_sName.equals(aspect.getIndex(k).getName(m_table)))
            {
               return true;
            }
         }
      }

      return false;
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      for (Iterator itr = getIndexColumnIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      super.makeReadOnly();

      ((ArrayList)m_indexColumnList).trimToSize(); // free unused memory
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      marker.setTypeName("Index");
      marker.setProperty("dataSource", m_table.getSchema().getDataSource().getName());
      marker.setProperty("table", m_table.getName());
      marker.setProperty("index", m_sName);
   }

   /**
    * Clones the index (deep copy).
    * @param table The new table instance.
    * @see nexj.core.meta.MetadataObject#clone()
    */
   protected Index clone(final Table table)
   {
      Index index = (Index)super.clone();

      index.m_table = table;
      index.m_indexColumnList = new ArrayList(m_indexColumnList.size());

      for (int i = 0, n = m_indexColumnList.size(); i < n; ++i)
      {
         index.m_indexColumnList.add(((IndexColumn)m_indexColumnList.get(i)).clone(index));
      }

      if (m_pointcutHelper != null)
      {
         index.m_pointcutHelper = (IndexPointcutHelper)m_pointcutHelper.clone(
            new PointcutHelper.AspectLookup()
            {
               public Aspect get(String sName) throws MetadataException
               {
                  return table.getSchema().getIndex(sName);
               }
            });
         index.m_pointcutHelper.m_index = index;
      }

      if (m_aspectHelper != null)
      {
         index.m_aspectHelper = (IndexAspectHelper)m_aspectHelper.clone();
         index.m_aspectHelper.m_index = index;
      }
      
      return index;
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.write("Index ");
      writer.write(m_sName);
   }

   /**
    * @see nexj.core.meta.NamedMetadataObject#toString()
    */
   public String toString()
   {
      return PrintWriter.toString(this);
   }

   // inner classes

   /**
    * Index-specific aspect helper.
    */
   protected final static class IndexAspectHelper extends AspectHelper
   {
      protected Index m_index;

      public IndexAspectHelper(Index index)
      {
         m_index = index;
      }

      /**
       * @see nexj.core.meta.AspectHelper#getContainer()
       */
      protected Aspect getContainer()
      {
         return m_index;
      }
   }

   /**
    * Index-specific pointcut helper.
    */
   protected final static class IndexPointcutHelper extends PointcutHelper
   {
      protected Index m_index;

      public IndexPointcutHelper(Index index)
      {
         m_index = index;
      }

      /**
       * @see nexj.core.meta.PointcutHelper#getContainer()
       */
      protected Pointcut getContainer()
      {
         return m_index;
      }

      /**
       * @see nexj.core.meta.PointcutHelper#getContainerType()
       */
      protected String getContainerType()
      {
         return "index";
      }
   }
}
