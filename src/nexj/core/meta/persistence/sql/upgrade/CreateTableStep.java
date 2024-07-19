// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Aspect;
import nexj.core.meta.AspectManager;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.Pointcut;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.RelationalSchemaAspectManager;
import nexj.core.meta.persistence.sql.SQLScript;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.Lookup;

/**
 * Upgrade step for creating a new table.
 */
public class CreateTableStep extends RelationalSchemaUpgradeStep
{
   // attributes

   /**
    * The table name.
    */
   protected String m_sName;

   /**
    * The table alias.
    */
   protected String m_sAlias;

   /**
    * The primary key name.
    */
   protected String m_sPrimaryKeyName;

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
    * Should QUERY table contents be automatically updated by the RDBMS from referenced tables.
    */
   protected boolean m_bViewAutoUpdated = true;

   // associations

   /**
    * The column outline map: ColumnOutline[String].
    */
   protected Lookup m_columnMap = new LinkedHashTab(8);

   /**
    * The index outline map: IndexOutline[String].
    */
   protected Lookup m_indexMap = new LinkedHashTab(2);

   /**
    * The aspect override list: sName[2*n], bInclusive[2*n+1].
    */
   protected List m_aspectOverrideList;

   /**
    * The pointcut pattern list: sPattern[2*n], bInclusive[2*n+1].
    */
   protected List m_pointcutPatternList;

   /**
    * The created table.
    */
   protected Table m_table;

   /**
    * The view SQL script.
    */
   protected SQLScript m_viewScript;

   /**
    * A list of DDL hints set on the table (lazy init)
    */
   protected List/*<String>*/ m_hintList;

   // operations

   /**
    * Sets the table name.
    * @param sName The table name to set.
    */
   public void setName(String sName)
   {
      verifyNotReadOnly();
      m_sName = sName;
   }

   /**
    * @return The table name.
    */
   public String getName()
   {
      return m_sName;
   }

   /**
    * Sets the table alias.
    * @param sAlias The table alias to set.
    */
   public void setAlias(String sAlias)
   {
      verifyNotReadOnly();
      m_sAlias = sAlias;
   }

   /**
    * @return The table alias.
    */
   public String getAlias()
   {
      return m_sAlias;
   }

   /**
    * Add the specified hint.
    * @param sHint The hint to add (not null).
    */
   public void addHint(String sHint)
   {
      verifyNotReadOnly();

      if (m_hintList == null)
      {
         m_hintList = new ArrayList/*<String>*/(2);
      }

      m_hintList.add(sHint);
   }

   /**
    * Sets the primary key name.
    * @param sPrimaryKeyName The primary key name to set.
    */
   public void setPrimaryKeyName(String sPrimaryKeyName)
   {
      verifyNotReadOnly();

      if (sPrimaryKeyName != null)
      {
         getIndexOutline(sPrimaryKeyName);
      }

      m_sPrimaryKeyName = sPrimaryKeyName;
   }

   /**
    * @return The primary key name.
    */
   public String getPrimaryKeyName()
   {
      return m_sPrimaryKeyName;
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
    * Adds a new column outline to the step.
    * @param column The column outline to add.
    * @throws MetadataException if a column outline
    * with the same name already exists.
    */
   public void addColumnOutline(ColumnOutline column)
   {
      verifyNotReadOnly();

      Object oldColumnOutline = m_columnMap.put(column.getName(), column);

      if (oldColumnOutline != null)
      {
         m_columnMap.put(column.getName(), oldColumnOutline);

         throw new MetadataException("err.meta.columnDup", new Object[]
         {
            column.getName(),
            getName()
         });
      }
   }

   /**
    * Gets a column outline by name.
    * @param sName The column outline name.
    * @return The column outline object.
    * @throws MetadataLookupException if the column outline does not exist.
    */
   public ColumnOutline getColumnOutline(String sName)
   {
      ColumnOutline column = (ColumnOutline) m_columnMap.get(sName);

      if (column != null)
      {
         return column;
      }

      throw new MetadataLookupException("err.meta.columnLookup", sName, this);
   }

   /**
    * @return The column outline count.
    */
   public int getColumnOutlineCount()
   {
      return m_columnMap.size();
   }

   /**
    * @return An iterator for the contained column outline objects.
    */
   public Iterator getColumnOutlineIterator()
   {
      return m_columnMap.valueIterator();
   }

   /**
    * Adds a new index outline to the step.
    * @param index The index outline to add.
    * @throws MetadataException if a index outline
    * with the same name already exists.
    */
   public void addIndexOutline(IndexOutline index)
   {
      verifyNotReadOnly();

      Object oldIndexOutline = m_indexMap.put(index.getName(), index);

      if (oldIndexOutline != null)
      {
         m_indexMap.put(index.getName(), oldIndexOutline);

         throw new MetadataException("err.meta.indexDup", new Object[]
         {
            index.getName(),
            getName()
         });
      }
   }

   /**
    * Gets a index outline by name.
    * @param sName The index outline name.
    * @return The index outline object.
    * @throws MetadataLookupException if the index outline does not exist.
    */
   public IndexOutline getIndexOutline(String sName)
   {
      IndexOutline index = (IndexOutline) m_indexMap.get(sName);

      if (index != null)
      {
         return index;
      }

      throw new MetadataLookupException("err.meta.tableIndexLookup", sName, this);
   }

   /**
    * @return The index outline count.
    */
   public int getIndexOutlineCount()
   {
      return m_indexMap.size();
   }

   /**
    * @return An iterator for the contained index outline objects.
    */
   public Iterator getIndexOutlineIterator()
   {
      return m_indexMap.valueIterator();
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
      m_bViewAutoUpdated = bAutoUpdate;
   }

   /**
    * Adds an aspect override to the step.
    * @param sName The aspect name.
    * @param bInclusive True if the aspect is inclusive.
    */
   public void addAspectOverride(String sName, boolean bInclusive)
   {
      verifyNotReadOnly();

      if (m_aspectOverrideList == null)
      {
         m_aspectOverrideList = new ArrayList(2);
      }

      m_aspectOverrideList.add(sName);
      m_aspectOverrideList.add(Boolean.valueOf(bInclusive));
   }

   /**
    * Adds a pointcut to the step.
    * @param sName The pointcut pattern.
    * @param bInclusive True if the aspect is inclusive.
    */
   public void addPointcutPattern(String sPattern, boolean bInclusive)
   {
      verifyNotReadOnly();

      if (m_pointcutPatternList == null)
      {
         m_pointcutPatternList = new ArrayList(2);
      }

      m_pointcutPatternList.add(sPattern);
      m_pointcutPatternList.add(Boolean.valueOf(bInclusive));
   }

   /**
    * @return The created table.
    */
   public Table getTable()
   {
      return m_table;
   }

   /**
    * Add all the aspects to the specified table.
    * @param schema The schema to query for aspects.
    * @param pointcut The table to add the aspects to.
    * @param state Upgrade state. Non-null for the undo phase.
    */
   protected void addAspects(RelationalSchema schema, Table pointcut,
      RelationalSchemaUpgradeState state)
   {
      if (m_aspectOverrideList != null)
      {
         for (int i = 0, n = m_aspectOverrideList.size(); i < n; i += 2)
         {
            String sName = (String)m_aspectOverrideList.get(i);

            if (state == null || !state.containsTable(sName))
            {
               pointcut.addAspectOverride(schema.getTable(sName),
                  ((Boolean)m_aspectOverrideList.get(i + 1)).booleanValue());
            }
         }
      }

      if (m_pointcutPatternList != null)
      {
         for (int i = 0, n = m_pointcutPatternList.size(); i < n; i += 2)
         {
            pointcut.addPointcutPattern((String)m_pointcutPatternList.get(i),
               ((Boolean)m_pointcutPatternList.get(i + 1)).booleanValue());
         }
      }
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#apply(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void apply(RelationalSchemaUpgradeState state)
   {
      final RelationalSchema schema = state.getSchema();

      m_table = new Table(schema);

      m_table.setType(m_nType);
      m_table.setName(m_sName);
      m_table.setAlias(m_sAlias);
      m_table.setTablespaceName(m_sTablespaceName);
      m_table.setIndexspaceName(m_sIndexspaceName);
      m_table.setLongspaceName(m_sLongspaceName);

      schema.addTable(m_table);

      for (Lookup.Iterator itr = m_columnMap.valueIterator(); itr.hasNext();)
      {
         ColumnOutline outline = (ColumnOutline)itr.next();
         Column column = new Column(outline.getName(), m_table);

         outline.copyTo(column);
         m_table.addColumn(column);
      }

      m_table.setViewScript(m_viewScript);
      m_table.setViewAutoUpdated(m_bViewAutoUpdated);
      addAspects(schema, m_table, null);

      RelationalSchemaAspectManager aspectManager = new RelationalSchemaAspectManager()
      {
         protected Iterator getTableAspectIterator()
         {
            return schema.getTableIterator();
         }

         protected Iterator getTablePointcutIterator()
         {
            return Collections.singletonList(m_table).iterator();
         }

         protected Iterator getIndexAspectIterator()
         {
            return schema.getIndexIterator();
         }

         protected Iterator getIndexPointcutIterator()
         {
            return m_table.getIndexIterator();
         }
      };

      aspectManager.applyAspects(0);

      for (Lookup.Iterator itr = m_indexMap.valueIterator(); itr.hasNext();)
      {
         IndexOutline outline = (IndexOutline)itr.next();
         Index index = new Index(outline.getName(), outline.getType(), m_table);

         outline.copyTo(index);
         m_table.addIndex(index);
      }

      aspectManager.applyAspects(1);

      for (Lookup.Iterator itr = m_indexMap.valueIterator(); itr.hasNext();)
      {
         IndexOutline outline = (IndexOutline)itr.next();
         Index index = m_table.getIndex(outline.getName());

         if (outline.getRelatedTableName() != null)
         {
            schema.getTable(outline.getRelatedTableName()).addRelatedKey(index);
         }
      }

      aspectManager.applyAspects(2);

      if (m_hintList != null)
      {
         for (int i = 0, nCount = m_hintList.size(); i < nCount; ++i)
         {
            m_table.addHint(m_hintList.get(i).toString());
         }
      }

      if (m_sPrimaryKeyName != null)
      {
         m_table.setPrimaryKey(schema.getIndex(m_sPrimaryKeyName));
      }

      m_table.computePrimaryKeyParts();
      m_table.validate(state.getSchema().getMetadata(), null, state.getAdapters());
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#undo(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void undo(final RelationalSchemaUpgradeState state)
   {
      final RelationalSchema schema = state.getSchema();

      if (!state.removeTable(m_sName))
      {
         schema.removeTable(schema.getTable(m_sName));
      }

      for (Iterator itr = m_indexMap.iterator(); itr.hasNext();)
      {
         state.removeIndex((String)itr.next());
      }

      // Apply all table-aspects to the table to get full list of indexes coming from aspects
      final Table pointcut = new Table(new RelationalSchema());
      AspectManager aspectManager = new AspectManager()
      {
         // override to avoid logging
         public void applyAspects(int nPass) throws MetadataException
         {
            if (nPass == 0)
            {
               addAspects();
            }

            for (int i = 0, n = pointcut.getAspectCount(); i < n; ++i)
            {
               Aspect aspect = pointcut.getAspect(i);

               if (m_aspectNameSet.contains(aspect.getName()))
               {
                  aspect.applyTo(pointcut, nPass);
               }
            }
         }

         protected Iterator getAspectIterator()
         {
            return schema.getTableIterator();
         }

         protected Iterator getPointcutIterator()
         {
            return Collections.singletonList(pointcut).iterator();
         }

         protected void setProperties(MetadataMarker e, Pointcut pointcut, Aspect aspect)
         {
         }
      };

      pointcut.setType(Table.EXTERNAL);
      pointcut.setName(getName());
      pointcut.setType(getType());
      addAspects(schema, pointcut, state);
      aspectManager.applyAspects(0);
      aspectManager.applyAspects(1);
      state.undoTableAspects(pointcut);

      for (int i = pointcut.getIndexCount() - 1; i >= 0; --i)
      {
         state.removeIndex(pointcut.getIndex(i).getName());
      }

      state.removeTableAspect(m_sName);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "CreateTable(name=" + m_sName + ')';
   }
}
