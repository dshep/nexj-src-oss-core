// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Iterator;

import nexj.core.meta.Aspect;
import nexj.core.meta.Attribute;
import nexj.core.meta.Component;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.GenericPersistenceMapping;
import nexj.core.meta.persistence.Key;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Logger;

/**
 * Mapping of a class to a set of tables.
 */
public final class RelationalMapping extends GenericPersistenceMapping
{
   // constants

   /**
    * Identity key generator.
    */
   public final static Component KEY_GEN_IDENTITY = new Component("identity");

   static
   {
      KEY_GEN_IDENTITY.setType(IdentityKeyGenerator.class);
      KEY_GEN_IDENTITY.makeReadOnly();
   }

   // attributes

   /**
    * The mapped table count.
    */
   private int m_nTableCount;

   /**
    * The denorm count.
    */
   private int m_nDenormCount;

   // associations

   /**
    * The primary key generator component. Can be null.
    */
   private Component m_keyGenerator;

   /**
    * The table containing the primary key.
    */
   private Table m_primaryTable;

   /**
    * The mapped table collection.
    * The first table is the primary one.
    */
   private Table[] m_tableArray = new Table[2];

   /**
    * The source denorm collection.
    */
   private RelationalDenorm[] m_denormArray;

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(RelationalMapping.class);

   // operations

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#isCompatible(nexj.core.meta.persistence.PersistenceMapping)
    */
   public boolean isCompatible(PersistenceMapping mapping)
   {
      return mapping instanceof RelationalMapping &&
         ((RelationalMapping)mapping).m_primaryTable == m_primaryTable;
   }

   /**
    * Sets the primary key generator component.
    * @param keyGenerator The primary key generator component to set.
    * @throws MetadataException if the component is not suitable for the mapping.
    */
   public void setKeyGenerator(Component keyGenerator) throws MetadataException
   {
      verifyNotReadOnly();

      if (m_primaryTable != null)
      {
         validateKeyGenerator(keyGenerator);
      }

      m_keyGenerator = keyGenerator;
   }

   /**
    * @return The primary key generator component.
    */
   public Component getKeyGenerator()
   {
      return m_keyGenerator;
   }

   /**
    * Sets the primary key table.
    * @param primaryTable The primary key table to set.
    */
   public void setPrimaryTable(Table primaryTable)
   {
      verifyNotReadOnly();

      if (primaryTable != null)
      {
         if (m_nTableCount != 0)
         {
            throw new IllegalStateException("The primary table must be added first");
         }

         addTable(primaryTable);
      }
      else
      {
         if (m_metaclass != null)
         {
            if (!(m_metaclass instanceof Aspect) || !((Aspect)m_metaclass).isAspect())
            {
               throw new MetadataException("err.meta.persistence.sql.noPrimaryTable",
                  new Object[]{m_metaclass.getName()});
            }
         }
      }

      m_primaryTable = primaryTable;
   }

   /**
    * @return The primary key table.
    */
   public Table getPrimaryTable()
   {
      return m_primaryTable;
   }

   /**
    * Adds a new attribute mapping to the relational mapping.
    * @param mapping The attribute mapping to add.
    * @throws MetadataException if an attribute mapping
    * with the same name already exists.
    */
   public void addAttributeMapping(AttributeMapping mapping)
   {
      super.addAttributeMapping(mapping);

      if (mapping instanceof RelationalPrimitiveMapping)
      {
         RelationalPrimitiveMapping primitiveMapping = (RelationalPrimitiveMapping)mapping;
         Column column = primitiveMapping.getColumn();

         addTable(column.getTable());
         column.getTable().addMapping(primitiveMapping);
      }
      else
      {
         RelationalClassMapping classMapping = (RelationalClassMapping)mapping;

         if (classMapping.getSourceKey() != null)
         {
            addTable(classMapping.getSourceKey().getTable());
         }
      }
   }

   /**
    * Adds a table to the mapping, if it does not exist.
    * @param table The table to add.
    * @return The ordinal number of the added table.
    * @throws MetadataException if the table primary key is
    * incompatible with the primary table primary key.
    */
   public int addTable(Table table) throws MetadataException
   {
      verifyNotReadOnly();

      for (int i = m_nTableCount - 1; i >= 0; --i)
      {
         if (m_tableArray[i] == table)
         {
            return i;
         }
      }

      if (m_metaclass != null &&
         (m_metaclass instanceof Aspect && ((Aspect)m_metaclass).isAspect()) != table.isAspect())
      {
         throw new MetadataException(
            (table.isAspect()) ? "err.meta.persistence.sql.classAspectTableMismatch" :
               "err.meta.persistence.sql.aspectClassTableMismatch",
               new Object[]{m_metaclass.getName(), table.getName()});
      }

      if (m_nTableCount == m_tableArray.length)
      {
         Table[] tableArray = new Table[m_nTableCount << 1];
         System.arraycopy(m_tableArray, 0, tableArray, 0, m_nTableCount);
         m_tableArray = tableArray;
      }

      if (m_primaryTable != null && m_primaryTable.getPrimaryKey() != null && table.getPrimaryKey() != null &&
         !m_primaryTable.getPrimaryKey().isCompatible(table.getPrimaryKey()))
      {
         throw new MetadataException("err.meta.incompatibleTablePrimaryKeys",
            new Object[]{m_primaryTable.getName(), table.getName()});
      }

      m_tableArray[m_nTableCount] = table;

      return m_nTableCount++;
   }

   /**
    * Gets a mapped table by ordinal number.
    * @param nOrdinal The table ordinal number, 0-based.
    * @return The mapped table.
    */
   public Table getTable(int nOrdinal)
   {
      if (nOrdinal >= m_nTableCount)
      {
         throw new IndexOutOfBoundsException("Invalid ordinal number in RelationalMapping.getTable()");
      }

      return m_tableArray[nOrdinal];
   }

   /**
    * Finds the ordinal number of a mapped table.
    * @param table The mapped table.
    * @return The ordinal number of the table, or -1 if not found.
    */
   public int findTableOrdinal(Table table)
   {
      for (int i = 0; i != m_nTableCount; ++i)
      {
         if (m_tableArray[i] == table)
         {
            return i;
         }
      }

      return -1;
   }

   /**
    * @return The mapped table count (at least 1).
    */
   public int getTableCount()
   {
      return m_nTableCount;
   }

   /**
    * Adds a source denorm to the mapping (the source value is usually elsewhere,
    * the destination value is on one of the tables of this class).
    * @param denorm The denorm to add.
    */
   public void addDenorm(RelationalDenorm denorm)
   {
      if (m_denormArray == null)
      {
         m_denormArray = new RelationalDenorm[2];
      }
      else if (m_nDenormCount == m_denormArray.length)
      {
         RelationalDenorm[] denormArray = new RelationalDenorm[m_nDenormCount << 1];

         System.arraycopy(m_denormArray, 0, denormArray, 0, m_nDenormCount);
         m_denormArray = denormArray;
      }

      m_denormArray[m_nDenormCount++] = denorm;
   }

   /**
    * Gets a denorm by ordinal number.
    * @param nOrdinal The denorm ordinal number.
    * @return The denorm.
    */
   public RelationalDenorm getDenorm(int nOrdinal)
   {
      if (nOrdinal >= m_nDenormCount)
      {
         throw new ArrayIndexOutOfBoundsException(nOrdinal);
      }

      return m_denormArray[nOrdinal];
   }

   /**
    * @return The denorm count.
    */
   public int getDenormCount()
   {
      return m_nDenormCount;
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#getObjectKey()
    */
   public Key getObjectKey()
   {
      return m_primaryTable.getPrimaryKey();
   }

   /**
    * @see nexj.core.meta.persistence.GenericPersistenceMapping#getForeignKey(java.lang.String)
    */
   protected Key getForeignKey(String sName) throws MetadataException
   {
      return ((RelationalSchema)getDataSource().getSchema()).getIndex(sName);
   }

   /**
    * Verifies the locking attribute and performs precomputations.
    * @throws MetadataException if the verification fails.
    */
   public void resolveInheritance() throws MetadataException
   {
      PersistenceMapping baseMapping = getBaseMapping();

      // Inherit the persistence mappings
      if (baseMapping != null && baseMapping.getDataSource() == m_dataSource)
      {
         RelationalMapping mapping = (RelationalMapping)baseMapping;

         if (m_primaryTable == mapping.getPrimaryTable())
         {
            for (int i = 0, nCount = mapping.getTableCount(); i < nCount; ++i)
            {
               addTable(mapping.getTable(i));
            }

            for (int i = 0, nCount = mapping.getMetaclass().getInstanceAttributeCount(); i < nCount; ++i)
            {
               Attribute baseAttribute = mapping.getMetaclass().getInstanceAttribute(i);
               AttributeMapping baseAttrMapping = mapping.getAttributeMapping(baseAttribute);

               if (baseAttrMapping != null)
               {
                  Attribute derivedAttribute = m_metaclass.getDerivedAttribute(baseAttribute);
                  AttributeMapping derivedAttrMapping = getAttributeMapping(derivedAttribute);

                  if (derivedAttrMapping == null)
                  {
                     if (derivedAttribute.getType() == baseAttribute.getType())
                     {
                        derivedAttrMapping = (AttributeMapping)baseAttrMapping.clone(derivedAttribute);
                        derivedAttrMapping.setAttribute(derivedAttribute);
                        addAttributeMapping(derivedAttrMapping);
                     }
                  }
                  else if (m_typeCodeAttribute != null || mapping.m_typeCodeAttribute != null)
                  {
                     boolean bMismatch;

                     if (baseAttrMapping instanceof RelationalPrimitiveMapping)
                     {
                        bMismatch = (((RelationalPrimitiveMapping)derivedAttrMapping).getColumn() !=
                           ((RelationalPrimitiveMapping)baseAttrMapping).getColumn());
                     }
                     else
                     {
                        RelationalClassMapping baseClassMapping = (RelationalClassMapping)baseAttrMapping;
                        RelationalClassMapping classMapping = (RelationalClassMapping)derivedAttrMapping;

                        bMismatch = (classMapping.getDestinationKey() != baseClassMapping.getDestinationKey() ||
                           classMapping.getSourceKey() != baseClassMapping.getSourceKey());
                     }

                     if (bMismatch)
                     {
                        MetadataValidationException e = new MetadataValidationException(
                           "err.meta.persistenceMappingMismatch",
                           new Object[]{derivedAttribute.getName(), m_metaclass.getName(), mapping.getMetaclass().getName()});

                        derivedAttribute.setProperties(e);

                        throw e;
                     }
                  }
               }
            }
         }
         else
         {
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Primary table " + mapping.getPrimaryTable().getName() +
                  " overridden by " + m_primaryTable.getName() + " in class " + m_metaclass.getName());
            }
         }
      }

      super.resolveInheritance();

      if (!(m_metaclass instanceof Aspect))
      {
         if (m_lockingAttribute != null)
         {
            if (((RelationalPrimitiveMapping)getAttributeMapping(m_lockingAttribute)).getColumn().getTable() != getPrimaryTable())
            {
               throw new MetadataException("err.meta.lockingAttributeNotInPrimaryTable",
                  new Object[]{m_lockingAttribute.getName(), m_metaclass.getName()});
            }
         }

         if (m_typeCodeAttribute != null)
         {
            ((RelationalPrimitiveMapping)getAttributeMapping(m_typeCodeAttribute)).getColumn().setCaseInsensitive(false);
         }
      }

      if (s_logger.isDebugEnabled())
      {
         StringBuffer buf = new StringBuffer(64);

         buf.append("Tables for class ");
         buf.append(m_metaclass.getName());

         if (isContext())
         {
            buf.append(" (context mapping)");
         }

         buf.append(": ");

         for (int i = 0; i < m_nTableCount; ++i)
         {
            if (i > 0)
            {
               buf.append(", ");
            }

            buf.append(m_tableArray[i].getName());
         }

         s_logger.debug(buf.toString());
      }
   }

   /**
    * @see PersistenceMapping#deriveAttributeMapping(Attribute, Attribute, AttributeMapping)
    */
   protected void deriveAttributeMapping(Attribute attribute, Attribute base, AttributeMapping baseMapping)
   {
      if (attribute.getType() == base.getType())
      {
         if (attribute.getMetaclass() instanceof Aspect)
         {
            if (getAttributeMapping(attribute) == null)
            {
               AttributeMapping mapping = (AttributeMapping)baseMapping.clone();

               mapping.setAttribute(attribute);
               addAttributeMapping(mapping);
            }
         }
         else
         {
            if (baseMapping instanceof RelationalPrimitiveMapping)
            {
               RelationalPrimitiveMapping basePrimitiveMapping = (RelationalPrimitiveMapping)baseMapping;

               for (int nTable = 0, nTableCount = getTableCount(); nTable < nTableCount; ++nTable)
               {
                  Table table = getTable(nTable);

                  if (table.hasAspect(basePrimitiveMapping.getColumn().getTable()))
                  {
                     RelationalPrimitiveMapping primitiveMapping = (RelationalPrimitiveMapping)getAttributeMapping(attribute);

                     if (primitiveMapping == null)
                     {
                        primitiveMapping = (RelationalPrimitiveMapping)basePrimitiveMapping.clone();
                        primitiveMapping.setColumn(table.getColumn(basePrimitiveMapping.getColumn().getName()));
                        primitiveMapping.setAttribute(attribute);

                        addAttributeMapping(primitiveMapping);
                     }
                     else if (table != primitiveMapping.getColumn().getTable())
                     {
                        RelationalPrimitiveDenorm denorm = new RelationalPrimitiveDenorm(primitiveMapping);

                        denorm.setColumn(table.getColumn(basePrimitiveMapping.getColumn().getName()));
                        denorm.getColumn().addAttribute(attribute);
                        primitiveMapping.addDenorm(denorm);
                        addDenorm(denorm);
                     }
                  }
               }
            }
            else
            {
               RelationalClassMapping baseClassMapping = (RelationalClassMapping)baseMapping;
               Index sourceKey = baseClassMapping.getSourceKey();

               if (sourceKey == null)
               {
                  RelationalClassMapping classMapping = (RelationalClassMapping)getAttributeMapping(attribute);

                  if (classMapping == null)
                  {
                     classMapping = (RelationalClassMapping)baseClassMapping.clone(attribute);
                     classMapping.setSourceKey(m_primaryTable.getPrimaryKey());
                     classMapping.setDestinationKey(classMapping.getDestinationKey());
                     addAttributeMapping(classMapping);
                  }
               }
               else
               {
                  for (int nTable = 0, nTableCount = getTableCount(); nTable < nTableCount; ++nTable)
                  {
                     Table table = getTable(nTable);

                     if (table.hasAspect(sourceKey.getTable()))
                     {
                        RelationalClassMapping classMapping = (RelationalClassMapping)getAttributeMapping(attribute);

                        if (classMapping == null)
                        {
                           classMapping = (RelationalClassMapping)baseClassMapping.clone(attribute);
                           classMapping.setSourceKey(table.getIndex(sourceKey.getName(table)));
                           classMapping.setAttribute(attribute);
                           addAttributeMapping(classMapping);
                        }
                        else if (!sourceKey.isObjectKeyPart() && table != classMapping.getSourceKey().getTable())
                        {
                           RelationalClassDenorm denorm = new RelationalClassDenorm(classMapping);

                           denorm.setSourceKey(table.getIndex(sourceKey.getName(table)));
                           classMapping.addDenorm(denorm);
                           addDenorm(denorm);
                        }
                     }
                  }
               }
            }
         }
      }
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#resolveInheritance2()
    */
   public void resolveInheritance2()
   {
      super.resolveInheritance2();

      // Set up the default reverse association mappings

      for (int i = 0, nCount = m_metaclass.getInstanceAttributeCount(); i < nCount; ++i)
      {
         Attribute attribute = m_metaclass.getInstanceAttribute(i);
         Attribute reverse = attribute.getReverse();

         if (reverse != null && attribute.isSymmetric())
         {
            RelationalClassMapping mapping = (RelationalClassMapping)getAttributeMapping(attribute);

            if (mapping == null)
            {
               AttributeMapping attributeMapping = reverse.findPersistenceMapping(this, true);

               if (attributeMapping instanceof RelationalClassMapping)
               {
                  RelationalClassMapping reverseMapping = (RelationalClassMapping)attributeMapping;

                  if (reverseMapping != null)
                  {
                     mapping = new RelationalClassMapping();
                     mapping.setPersistenceMapping(this);
                     mapping.setAttribute(attribute);
                     mapping.setMapping(attributeMapping.getPersistenceMapping());
                     mapping.setSourceKey((Index)reverseMapping.getDestinationKey());
                     mapping.setDestinationKey(reverseMapping.getSourceKey());
                     addAttributeMapping(mapping);
                  }
               }
            }
         }
      }

      PersistenceMapping baseMapping = getBaseMapping();

      // Inherit the key generators

      if (baseMapping != null && baseMapping.getDataSource() == getDataSource())
      {
         RelationalMapping mapping = (RelationalMapping)baseMapping;

         if (m_keyGenerator == null)
         {
            m_keyGenerator = mapping.getKeyGenerator();
         }
      }
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#getSortKeys(nexj.core.meta.Attribute[], nexj.core.meta.persistence.PersistenceMapping[], Attribute[])
    */
   public Pair getSortKeys(Attribute[] assocs, PersistenceMapping[] mappings, Attribute[] restrictions)
   {
      Pair sortKeys = null;
      Pair lastKey = null;

      for (int nTable = 0; nTable < m_nTableCount; ++nTable)
      {
         Table table = m_tableArray[nTable];
         RelationalPrimitiveMapping[] primitiveMappings = table.findMappingArray(this);
         BitSet filterSet = null;
         BitSet optionalSet = null;
         int nFilterCount = 0;

         if (primitiveMappings == null)
         {
            continue;
         }

         // Create a set of columns, which participate in equality
         // comparisons due to the associations.
         if (assocs != null && assocs.length > 0)
         {
            filterSet = new BitSet(table.getColumnCount());

            for (int i = 0; i < assocs.length; ++i)
            {
               PersistenceMapping mapping = mappings[i];

               if (mapping == null)
               {
                  continue;
               }

               AttributeMapping attributeMapping = mapping.getAttributeMapping(assocs[i]);

               if (attributeMapping instanceof RelationalClassMapping)
               {
                  RelationalClassMapping classMapping = (RelationalClassMapping)attributeMapping;

                  if (classMapping.getMapping() == this &&
                     classMapping.getDestinationKey() instanceof Index)
                  {
                     Index key = (Index)classMapping.getDestinationKey();

                     if (key.getTable() != table)
                     {
                        key = table.getPrimaryKey();
                     }

                     if (!key.isUnique())
                     {
                        for (int k = key.getIndexColumnCount() - 1; k >= 0; --k)
                        {
                           filterSet.set(key.getIndexColumn(k).getColumn().getOrdinal());
                        }
                     }
                  }
               }
            }

            nFilterCount = filterSet.cardinality();
         }

         // Create a set of columns, which participate in equality
         // comparisons and can be optionally excluded from the index.
         if (restrictions != null && restrictions.length > 0)
         {
            optionalSet = new BitSet(table.getColumnCount());

            for (int i = 0; i < restrictions.length; ++i)
            {
               AttributeMapping attributeMapping = getAttributeMapping(restrictions[i]);

               if (attributeMapping instanceof RelationalClassMapping)
               {
                  Index index = ((RelationalClassMapping)attributeMapping).getSourceKey();

                  for (int k = index.getIndexColumnCount() - 1; k >= 0; --k)
                  {
                     optionalSet.set(index.getIndexColumn(k).getColumn().getOrdinal());
                  }
               }
               else if (attributeMapping instanceof RelationalPrimitiveMapping)
               {
                  optionalSet.set(((RelationalPrimitiveMapping)attributeMapping).getColumn().getOrdinal());
               }
            }
         }

         // Collect the sort keys
         for (int nIndex = 0; nIndex < table.getIndexCount(); ++nIndex)
         {
            Index index = table.getIndex(nIndex);

            if (index.getType() != Index.CLUSTER &&
               index.getType() != Index.BTREE &&
               index.getType() != Index.QUERY)
            {
               continue;
            }

            // Ignore indexes that do not start with the columns specified
            // in the association filter, in arbitrary order
            if (nFilterCount > 0)
            {
               if (index.getIndexColumnCount() < nFilterCount)
               {
                  continue;
               }

               int i;

               for (i = 0; i < nFilterCount; ++i)
               {
                  if (!filterSet.get(index.getIndexColumn(i).getColumn().getOrdinal()))
                  {
                     break;
                  }
               }

               if (i < nFilterCount)
               {
                  continue;
               }
            }

            Index pk = table.getPrimaryKey();
            boolean bMatch = true;
            boolean bAscending = true;
            Pair key = null;

            // Check whether the primary key columns are a subset of the index columns
            for (int i = 0; i < pk.getIndexColumnCount(); ++i)
            {
               IndexColumn pkIndexColumn = pk.getIndexColumn(i);
               Column pkColumn = pkIndexColumn.getColumn();

               if (optionalSet != null &&
                  optionalSet.get(pkColumn.getOrdinal()) &&
                  index.findIndexColumn(pkColumn) != null)
               {
                  continue;
               }

               int k;

               for (k = 0; k < index.getIndexColumnCount(); ++k)
               {
                  IndexColumn indexColumn = index.getIndexColumn(k);

                  if (indexColumn.getColumn() == pkColumn)
                  {
                     if (i == 0)
                     {
                        bAscending = indexColumn.isAscending();
                     }

                     break;
                  }
               }

               if (k == index.getIndexColumnCount())
               {
                  bMatch = false;

                  break;
               }
            }

            // If the above is true, then append the primary key
            // at the end of the sort key
            if (bMatch)
            {
               key = new Pair(new Pair(new Pair(Symbol.AT),
                  Boolean.valueOf(!(pk.getIndexColumn(0).isAscending() ^ bAscending))));
            }
            else
            {
               pk = null;
            }

            int nCount;

            for (nCount = nFilterCount; nCount < index.getIndexColumnCount(); ++nCount)
            {
               int nOrdinal = index.getIndexColumn(nCount).getColumn().getOrdinal();

               if (primitiveMappings[nOrdinal] == null)
               {
                  if (optionalSet == null || !optionalSet.get(nOrdinal))
                  {
                     break;
                  }
               }
            }

            bMatch = true;

            if (pk != null)
            {
               if (nCount == index.getIndexColumnCount())
               {
                  key = null;
               }
               else
               {
                  for (int i = nCount; i < index.getIndexColumnCount(); ++i)
                  {
                     if (!index.getIndexColumn(i).getColumn().isPrimary())
                     {
                        bMatch = false;
                        break;
                     }
                  }
               }
            }

            if (!bMatch)
            {
               continue;
            }

            boolean bPrimitive = false;

            for (int i = nCount - 1; i >= nFilterCount; --i)
            {
               IndexColumn indexColumn = index.getIndexColumn(i);
               int nOrdinal = indexColumn.getColumn().getOrdinal();
               RelationalPrimitiveMapping mapping = primitiveMappings[nOrdinal];

               if (mapping != null)
               {
                  if (optionalSet == null || !optionalSet.get(nOrdinal))
                  {
                     key = new Pair(new Pair(mapping.getAttribute().getSymbol(),
                        Boolean.valueOf(indexColumn.isAscending())), key);
                     bPrimitive = true;
                  }
               }
            }

            // Append the new sort key to the end of the sort key list
            if (key != null)
            {
               lastKey = addKey(new Pair(Boolean.valueOf(pk != null ||
                  index.isUnique() && nCount == index.getIndexColumnCount()), key), sortKeys, lastKey);

               if (sortKeys == null)
               {
                  sortKeys = lastKey;
               }

               if (pk == index && bPrimitive)
               {
                  lastKey = addKey(new Pair(Boolean.TRUE,
                     new Pair(new Pair(new Pair(Symbol.AT), Boolean.TRUE))), sortKeys, lastKey);
               }
            }
         }
      }

      return sortKeys;
   }

   /**
    * Adds a key to the end of a key list.
    * @param key The key to add.
    * @param tail The tail of the key list. Can be null.
    * @param head The head of the key list. Can be null.
    * @return The new tail of the key list.
    */
   protected static Pair addKey(Pair key, Pair head, Pair tail)
   {
      if (tail == null)
      {
         tail = new Pair(key);
      }
      else
      {
         Pair pair;

         // Filter out the duplicates
         for (pair = head; pair != null; pair = pair.getNext())
         {
            if (pair.getHead().equals(key))
            {
               break;
            }
         }

         if (pair == null)
         {
            key = new Pair(key);
            tail.setTail(key);
            tail = key;
         }
      }

      return tail;
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#getUniqueKeys()
    */
   public Pair getUniqueKeys()
   {
      Pair uniqueKeys = null; // The resulting key list
      int[] ordinalArray = null; // Contains the ordinal numbers of attributes
      boolean[] useArray = null; // Flags index columns as used

      for (int nTable = 0; nTable < m_nTableCount; ++nTable)
      {
         Table table = getTable(nTable);

         for (int nIndex = 0, nIndexCount = table.getIndexCount(); nIndex < nIndexCount; ++nIndex)
         {
            Index index = table.getIndex(nIndex);

            if (!index.isUnique())
            {
               continue;
            }

            int nIndexColumnCount = index.getIndexColumnCount();

            if (nIndexColumnCount == 0)
            {
               continue;
            }

            // Find all the sets of no more than nIndexColumnCount attributes
            // which are mapped to the columns of the unique index, so that
            // no unmapped columns are left and each column is used only once

            // Allocate an array of at least nIndexColumnCount counters
            if (ordinalArray == null || ordinalArray.length < nIndexColumnCount)
            {
               ordinalArray = new int[Math.max(nIndexColumnCount, 4)];
               useArray = new boolean[ordinalArray.length];
            }
            else
            {
               ordinalArray[0] = 0;
               Arrays.fill(useArray, false);
            }

            uniqueKeys = getUniqueKeys(index, uniqueKeys, ordinalArray, useArray);
         }
      }

      return uniqueKeys;
   }

   /**
    * Determines the unique keys for a given index.
    * @param index The index for which to determine the unique keys.
    * @param uniqueKeys The list of unique keys to which to prepend.
    * @return A list of unique keys.
    * @see nexj.core.meta.persistence.PersistenceMapping#getUniqueKeys()
    */
   public Pair getUniqueKeys(Index index)
   {
      if (index.isUnique())
      {
         int nIndexColumnCount = index.getIndexColumnCount();

         if (nIndexColumnCount != 0)
         {
            return getUniqueKeys(index, null, new int[nIndexColumnCount],
               new boolean[nIndexColumnCount]);
         }
      }

      return null;
   }

   /**
    * Helper method determining the unique keys for a given index.
    * @param index The index for which to determine the unique keys.
    * @param uniqueKeys The list of unique keys to which to prepend.
    * @param ordinalArray Work array containing attribute ordinal numbers on return.
    * Must be able to contain at least as many entries as there are index columns.
    * @param useArray Work array containing used index column flags
    * on return, same size as ordinalArray.
    * @return The augmented unique key list.
    * @see nexj.core.meta.persistence.PersistenceMapping#getUniqueKeys()
    */
   protected Pair getUniqueKeys(Index index, Pair uniqueKeys,
      int[] ordinalArray, boolean[] useArray)
   {
      int nIndexColumnCount = index.getIndexColumnCount();
      int nAttributeCount = m_metaclass.getInstanceAttributeCount();
      int nTop = 0; // The current ordinal counter
      int nColumnCount = 0; // The mapped index column count

      for (;;)
      {
         if (ordinalArray[nTop] >= nAttributeCount)
         {
            if (--nTop >= 0)
            {
               nColumnCount -= mark(getAttributeMapping(
                  m_metaclass.getInstanceAttribute(ordinalArray[nTop]++)),
                  index, useArray, false);

               continue;
            }

            break;
         }

         AttributeMapping mapping = getAttributeMapping(m_metaclass.getInstanceAttribute(ordinalArray[nTop]));

         if (mapping != null && !mapping.getAttribute().isCollection())
         {
            if (!(mapping instanceof RelationalClassMapping) ||
               ((RelationalClassMapping)mapping).getDestinationKey().isUnique())
            {
               int nSize =  getSubsetSize(mapping, index, useArray);

               if (nSize > 0)
               {
                  if (nColumnCount + nSize == nIndexColumnCount)
                  {
                     Pair key = null;

                     for (int i = nTop; i >= 0; --i)
                     {
                        key = new ConstPair(m_metaclass.getInstanceAttribute(ordinalArray[i]).getSymbol(), key);
                     }

                     uniqueKeys = new ConstPair(key, uniqueKeys);
                  }
                  else if (nTop < nIndexColumnCount - 1)
                  {
                     nColumnCount += mark(mapping, index, useArray, true);
                     ordinalArray[nTop + 1] = ordinalArray[nTop];
                     ++nTop;
                  }
               }
            }
         }

         ++ordinalArray[nTop];
      }

      return uniqueKeys;
   }

   /**
    * Determines if an attribute mapping columns are a subset of index columns.
    * @param mapping The attribute mapping.
    * @param index The index, a subset of which is sought.
    * @param excludeArray A boolean array with true values set to exclude corresponding index columns.
    * @return The subset size.
    */
   private static int getSubsetSize(AttributeMapping mapping, Index index, boolean excludeArray[])
   {
      if (mapping instanceof RelationalPrimitiveMapping)
      {
         IndexColumn indexColumn = index.findIndexColumn(((RelationalPrimitiveMapping)mapping).getColumn());

         return (indexColumn != null && !excludeArray[indexColumn.getOrdinal()]) ? 1 : 0;
      }

      Index sourceIndex = ((RelationalClassMapping)mapping).getSourceKey();

      for (int i = 0, n = sourceIndex.getIndexColumnCount(); i < n; ++i)
      {
         IndexColumn indexColumn = index.findIndexColumn(sourceIndex.getIndexColumn(i).getColumn());

         if (indexColumn == null || excludeArray[indexColumn.getOrdinal()])
         {
            return 0;
         }
      }

      return sourceIndex.getIndexColumnCount();
   }

   /**
    * Marks the index columns corresponding to attribute mapping columns in a boolean array.
    * @param mapping The attribute mapping.
    * @param index The index.
    * @param flagArray The flag array.
    * @param bFlag The flag to set.
    * @return The count of the marked columns.
    */
   private static int mark(AttributeMapping mapping, Index index, boolean flagArray[], boolean bFlag)
   {
      if (mapping instanceof RelationalPrimitiveMapping)
      {
         flagArray[index.findIndexColumn(((RelationalPrimitiveMapping)mapping).getColumn()).getOrdinal()] = bFlag;

         return 1;
      }

      Index sourceIndex = ((RelationalClassMapping)mapping).getSourceKey();

      for (int i = 0, n = sourceIndex.getIndexColumnCount(); i < n; ++i)
      {
         flagArray[index.findIndexColumn(sourceIndex.getIndexColumn(i).getColumn()).getOrdinal()] = bFlag;
      }

      return sourceIndex.getIndexColumnCount();
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#create()
    */
   public PersistenceMapping create()
   {
      RelationalMapping mapping = new RelationalMapping();

      mapping.setPrimaryTable(m_primaryTable);

      return mapping;
   }

   /**
    * Computes the unmapped column nullable flag.
    * @param metadata The root metadata object.
    */
   public static void resolve(Metadata metadata)
   {
      for (Iterator itr = metadata.getMetaclassIterator(); itr.hasNext();)
      {
         final Metaclass metaclass = (Metaclass)itr.next();

         metaclass.visit(new PersistenceMapping.Visitor()
         {
            public void visit(PersistenceMapping mapping)
            {
               if (mapping instanceof RelationalMapping)
               {
                  RelationalMapping relMapping = (RelationalMapping)mapping;
                  BitSet[] bitSetArray = new BitSet[relMapping.getTableCount()];

                  for (int i = 0; i != relMapping.getTableCount(); ++i)
                  {
                     bitSetArray[i] = new BitSet(relMapping.getTable(i).getColumnCount());
                  }

                  for (int nAttr = 0; nAttr != metaclass.getInstanceAttributeCount(); ++nAttr)
                  {
                     AttributeMapping attrMapping = relMapping.getAttributeMapping(metaclass.getInstanceAttribute(nAttr));

                     if (attrMapping instanceof RelationalPrimitiveMapping)
                     {
                        RelationalPrimitiveMapping primitiveMapping = (RelationalPrimitiveMapping)attrMapping;

                        Column column = primitiveMapping.getColumn();

                        bitSetArray[relMapping.findTableOrdinal(column.getTable())].set(column.getOrdinal());

                        for (int i = 0; i != primitiveMapping.getDenormCount(); ++i)
                        {
                           column = ((RelationalPrimitiveDenorm)primitiveMapping.getDenorm(i)).getColumn();
                        }

                        bitSetArray[relMapping.findTableOrdinal(column.getTable())].set(column.getOrdinal());
                     }
                     else if (attrMapping instanceof RelationalClassMapping)
                     {
                        Index index = ((RelationalClassMapping)attrMapping).getSourceKey();
                        BitSet bitSet = bitSetArray[relMapping.findTableOrdinal(index.getTable())];

                        for (int i = 0; i != index.getIndexColumnCount(); ++i)
                        {
                           bitSet.set(index.getIndexColumn(i).getColumn().getOrdinal());
                        }
                     }
                  }

                  for (int i = 0; i != relMapping.getTableCount(); ++i)
                  {
                     Table table = relMapping.getTable(i);
                     Index index = table.getPrimaryKey();
                     BitSet bitSet = bitSetArray[i];

                     for (int k = 0; k != index.getIndexColumnCount(); ++k)
                     {
                        bitSet.set(index.getIndexColumn(k).getColumn().getOrdinal());
                     }

                     for (int k = bitSet.nextClearBit(0); k >= 0 && k < table.getColumnCount(); k = bitSet.nextClearBit(k + 1))
                     {
                        table.getColumn(k).setRequired(false, true);
                     }
                  }
               }
            }
         });
      }
   }

   // inner classes

   /**
    * Stub for the identity key generator.
    */
   public final static class IdentityKeyGenerator
   {
      public final static int VALUE_COUNT = 1;
   }
}
