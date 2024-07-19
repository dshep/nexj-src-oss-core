// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataObject;
import nexj.core.util.EmptyIterator;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;

/**
 * Single element in a transformation source mapping path. 
 */
public class TransformationSource extends MetadataObject
{
   // attributes

   /**
    * The transformation source level.
    */
   protected int m_nLevel;

   /**
    * The primary end-point part flag. A primary part starts a transformation loop.
    */
   protected boolean m_bPrimary;

   /**
    * The null flag.
    */
   protected boolean m_bNull;

   /**
    * The default value flag.
    */
   protected boolean m_bDefault;
   
   // associations
   
   /**
    * The end-point part of the item providing the data.
    */
   protected EndpointPart m_part;
   
   /**
    * The parent transformation source.
    */
   protected TransformationSource m_parent;
   
   /**
    * The path to this source.
    */
   protected TransformationSource[] m_pathArray;

   /**
    * The child transformation source collection.
    */
   protected List m_childList; // of type TransformationSource

   /**
    * Mapping of message part names to transformation sources.
    */
   protected Lookup m_childMap; // of type TransformationSource[String]

   /**
    * The mapping collection.
    */
   protected List m_mappingList; // of type TransformationMapping

   // constructors
   
   /**
    * Constructs the transformation source.
    * @param part The end-point part of the item providing the data.
    */
   public TransformationSource(EndpointPart part)
   {
      m_part = part;
   }
   
   // operations

   /**
    * Sets the end-point part providing the data.
    * @param part The end-point part of the item providing the data to set.
    */
   public void setPart(EndpointPart part)
   {
      verifyNotReadOnly();
      m_part = part;
   }

   /**
    * @return The end-point part of the item providing the data.
    */
   public EndpointPart getPart()
   {
      return m_part;
   }

   /**
    * Sets the transformation source level.
    * @param nLevel The transformation source level to set.
    */
   public void setLevel(int nLevel)
   {
      verifyNotReadOnly();
      m_nLevel = nLevel;

      if (m_pathArray == null || m_pathArray.length != nLevel + 1)
      {
         m_pathArray = new TransformationSource[nLevel + 1];
      }

      for (TransformationSource source = this; source != null; source = source.getParent())
      {
         m_pathArray[source.getLevel()] = source;
      }
   }

   /**
    * @return The transformation source level.
    */
   public int getLevel()
   {
      return m_nLevel;
   }

   /**
    * Gets a path element corresponding to a given level.
    * @param nLevel The level of the path element.
    */
   public TransformationSource getPath(int nLevel)
   {
      return m_pathArray[nLevel];
   }

   /**
    * Sets the parent transformation source.
    * @param parent The parent transformation source to set.
    */
   public void setParent(TransformationSource parent)
   {
      verifyNotReadOnly();
      m_parent = parent;
   }

   /**
    * @return The parent transformation source.
    */
   public TransformationSource getParent()
   {
      return m_parent;
   }

   /**
    * Adds a child with a given name to the source.
    * Creates it if necessary.
    * @param sName The child end-point part name.
    * @return The child source.
    */
   public TransformationSource addChild(String sName)
   {
      if (m_part.isPrimitive())
      {
         throw new MetadataException("err.meta.transformation.primitiveParent", new Object[]{m_part.getName()});
      }

      return addChild(m_part.getChild(sName));
   }
   
   /**
    * Adds a child corresponding to a given end-point part to the source.
    * Creates it if necessary.
    * @param part The child end-point part.
    * @return The child source.
    */
   protected TransformationSource addChild(EndpointPart part)
   {
      if (m_childMap != null)
      {
         TransformationSource child = (TransformationSource)m_childMap.get(part.getName());

         if (child != null && child.getPart() == part)
         {
            return child;
         }
      }

      TransformationSource child = new TransformationSource(part);

      addChild(child);

      return child;
   }
   
   /**
    * Adds a new child transformation source to the object.
    * @param child The child transformation source to add.
    */
   public void addChild(TransformationSource child)
   {
      verifyNotReadOnly();
      
      if (m_childList == null)
      {
         m_childList = new ArrayList(4);
         m_childMap = new HashTab(4);
      }
      
      m_childList.add(child);
      m_childMap.put(child.getPart().getName(), child);
      child.setParent(this);
      child.setLevel(m_nLevel + 1);
   }

   /**
    * Gets the transformation source associated with the given end-point part.
    * @param sName The name of the end-point part to look up the transformation source.
    * @return The child transformation source.
    */
   public TransformationSource findChild(String sName)
   {
      if (m_childMap == null)
      {
         return null;
      }

      return (TransformationSource)m_childMap.get(sName);
   }

   /**
    * Gets a child transformation source by ordinal number.
    * @param nOrdinal The child transformation source ordinal number (0-based).
    * @return The child transformation source object.
    */
   public TransformationSource getChild(int nOrdinal)
   {
      return (TransformationSource)m_childList.get(nOrdinal);
   }

   /**
    * @return The child transformation source count.
    */
   public int getChildCount()
   {
      if (m_childList == null)
      {
         return 0;
      }
      
      return m_childList.size();
   }

   /**
    * @return An iterator for the contained child transformation source objects.
    */
   public Iterator getChildIterator()
   {
      if (m_childList == null)
      {
         return EmptyIterator.getInstance();
      }
      
      return m_childList.iterator();
   }

   /**
    * Sets the primary end-point part flag.
    * @param bPrimary The primary end-point part flag to set.
    */
   public void setPrimary(boolean bPrimary)
   {
      verifyNotReadOnly();

      if (bPrimary && !m_bPrimary && m_parent != null)
      {
         m_parent.setPrimary(true);
      }

      m_bPrimary = bPrimary;
   }

   /**
    * @return The primary end-point part flag.
    */
   public boolean isPrimary()
   {
      return m_bPrimary;
   }

   /**
    * Sets the null flag.
    * @param bNull The null flag to set.
    */
   public void setNull(boolean bNull)
   {
      verifyNotReadOnly();

      if (bNull && !m_bNull && m_parent != null)
      {
         m_parent.setNull(true);
      }

      m_bNull = bNull;
   }

   /**
    * @return The null flag.
    */
   public boolean isNull()
   {
      return m_bNull;
   }

   /**
    * Sets the default value flag.
    * @param bDefaultMapped The default value flag to set.
    */
   public void setDefault(boolean bDefault)
   {
      verifyNotReadOnly();

      if (bDefault && !m_bDefault && m_parent != null)
      {
         m_parent.setDefault(true);
      }

      m_bDefault = bDefault;
   }

   /**
    * @return The default value flag.
    */
   public boolean isDefault()
   {
      return m_bDefault;
   }
   
   /**
    * Adds a new mapping to the source.
    * @param mapping The mapping to add.
    */
   public void addMapping(TransformationMapping mapping)
   {
      verifyNotReadOnly();
      
      if (m_mappingList == null)
      {
         m_mappingList = new ArrayList(2);
      }
      
      m_mappingList.add(mapping);
      
      if (mapping.getDestinationCount() != 0)
      {
         setPrimary(true);
      }
   }

   /**
    * Gets a mapping by ordinal number.
    * @param nOrdinal The mapping ordinal number (0-based).
    * @return The mapping object.
    */
   public TransformationMapping getMapping(int nOrdinal)
   {
      return (TransformationMapping)m_mappingList.get(nOrdinal);
   }

   /**
    * @return The mapping count.
    */
   public int getMappingCount()
   {
      if (m_mappingList == null)
      {
         return 0;
      }
      
      return m_mappingList.size();
   }

   /**
    * @return An iterator for the contained mapping objects.
    */
   public Iterator getMappingIterator()
   {
      if (m_mappingList == null)
      {
         return EmptyIterator.getInstance();
      }
      
      return m_mappingList.iterator();
   }

   /**
    * Computes the maximum level for this source and all children.
    * @return The maximum level.
    */
   public int getMaxLevel()
   {
      int nMax = m_nLevel;

      for (int i = 0, nCount = getChildCount(); i < nCount; i++)
      {
         nMax = Math.max(nMax, ((TransformationSource)m_childList.get(i)).getMaxLevel());
      }

      return nMax;
   }

   /**
    * Updates the end-point parts of this source and all its children. Assigns the
    * given part to this source. Assigns children of "part" to the
    * children of this source, using the name of the old part as the key to
    * look up the new part.
    * 
    * Used to ensure that a tree of sources that were copied from a base transformation
    * are associated with end-point parts from the message of the derived transformation.
    * 
    * @param part The new end-point part for the source.
    */
   protected void updateParts(EndpointPart part)
   {
      m_part = part;

      int nCount = getChildCount();

      if (nCount > 0)
      {
         for (int i = 0; i < nCount; i++)
         {
            TransformationSource childSource = getChild(i);

            childSource.updateParts(part.getChild(childSource.getPart().getName()));
         }
      }
   }

   /**
    * Resolves inheritance for this source and all of its children.
    * @param base The equivalent source from the base transformation.
    */
   public void resolveInheritance(TransformationSource base)
   {
      List sourceList = new ArrayList(4);
      Lookup sourceMap = new HashTab(4);

      for (int i = 0, nSize = base.getChildCount(); i < nSize; i++)
      {
         TransformationSource baseChildSource = base.getChild(i);
         TransformationSource childSource = findChild(baseChildSource.getPart().getName());

         if (childSource == null)
         {
            TransformationSource sourceCopy = (TransformationSource)baseChildSource.clone();

            sourceList.add(sourceCopy);
            sourceMap.put(sourceCopy.getPart().getName(), sourceCopy);

            sourceCopy.setParent(this);
            sourceCopy.updateParts(getPart().getChild(baseChildSource.getPart().getName()));
            sourceCopy.setNull(false);
         }
         else
         {
            m_childMap.remove(childSource.getPart().getName());
            sourceList.add(childSource);
            sourceMap.put(childSource.getPart().getName(), childSource);
            childSource.resolveInheritance(baseChildSource);
         }
      }

      if (m_childList != null)
      {
         // Move children of this source after the children defined in base.
         for (Lookup.Iterator itr = m_childMap.valueIterator(); itr.hasNext(); )
         {
            TransformationSource source = (TransformationSource)itr.next();

            sourceList.add(source);
            sourceMap.put(source.getPart().getName(), source);
         }
      }

      m_childList = sourceList;
      m_childMap = sourceMap;

      resolveLevels(base);
   }

   /**
    * Computes the level and the path array for this source and all of its children.
    * @param base The equivalent source from the base transformation.
    */
   protected void resolveLevels(TransformationSource base)
   {
      for (int i = 0, nSize = base.getChildCount(); i < nSize; i++)
      {
         TransformationSource baseChildSource = base.getChild(i);
         TransformationSource childSource = findChild(baseChildSource.getPart().getName());

         childSource.setLevel(baseChildSource.getLevel());
         childSource.resolveLevels(baseChildSource);
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#clone()
    */
   public Object clone()
   {
      TransformationSource copy = (TransformationSource)super.clone();

      assert m_mappingList == null;

      copy.m_pathArray = null;

      if (copy.m_childList != null)
      {
         List childList = new ArrayList(copy.m_childList.size());
         Lookup childMap = new HashTab(copy.m_childMap.size());

         for (Iterator itr = copy.m_childList.iterator(); itr.hasNext(); )
         {
            TransformationSource sourceOld = (TransformationSource)itr.next();
            TransformationSource source = (TransformationSource)sourceOld.clone();

            childList.add(source);
            childMap.put(source.getPart().getName(), source);

            source.setParent(copy);
            source.setLevel(sourceOld.getLevel());
         }

         copy.m_childList = childList;
         copy.m_childMap = childMap;
      }

      return copy;
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();

      for (int i = 0, nSize = getChildCount(); i < nSize; i++)
      {
         getChild(i).makeReadOnly();
      }

      for (int i = 0, nSize = getMappingCount(); i < nSize; i++)
      {
         getMapping(i).makeReadOnly();
      }

      if (m_mappingList != null)
      {
         ((ArrayList)m_mappingList).trimToSize();
      }

      if (m_childList != null)
      {
         ((ArrayList)m_childList).trimToSize();
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer((m_nLevel + 1) * 16);

      buf.append(ObjUtil.getShortClassName(this));
      
      for (int i = 0; i <= m_nLevel; ++i)
      {
         buf.append(' ');
         buf.append(getPath(i).getPart().getName());
      }

      return buf.toString();
   }
}
