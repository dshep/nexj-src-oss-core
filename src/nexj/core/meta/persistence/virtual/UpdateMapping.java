// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.virtual;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataObject;
import nexj.core.persistence.PersistenceException;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.HashHolder;
import nexj.core.util.HashHolderList;
import nexj.core.util.Holder;
import nexj.core.util.HolderList;
import nexj.core.util.Lookup;

/**
 * A virtual persistence adapter update mapping. Holds a collection of cases,
 * one for each set of attributes to update.
 */
public class UpdateMapping extends MetadataObject
{
   // associations

   /**
    * The update mapping cases.
    */
   protected HolderList m_updateMappingSet = new HashHolderList();

   /**
    * The persistence mapping that contains this mapping.
    */
   protected final VirtualMapping m_virtualMapping;

   // constructors

   /**
    * Creates a new update mapping.
    * @param virtualMapping The parent persistence mapping.
    */
   public UpdateMapping(VirtualMapping virtualMapping)
   {
      super();
      m_virtualMapping = virtualMapping;
   }

   // operations

   /**
    * Compiles this mapping.
    * @param machine The virtual machine for compilation.
    * @param textPosMap The text position map.
    */
   public void compile(Machine machine, Lookup textPosMap)
   {
      String sURLPrefix = "class:" + m_virtualMapping.getMetaclass().getName() + ".persistence.update";

      for (int i = 0, nOrdinal = 0, nCount = m_updateMappingSet.size(); i < nCount; i++)
      {
         UpdateMappingCase updateMappingCase = (UpdateMappingCase)m_updateMappingSet.get(i);

         if (m_virtualMapping.getMetaclass() == updateMappingCase.getDeclarator())
         {
            updateMappingCase.compile(machine, sURLPrefix + '.' + nOrdinal, textPosMap);
            nOrdinal++;
         }
      }
   }

   /**
    * Adds an update mapping case to this persistence mapping.
    * @param update The update mapping case to add.
    * @param attributes List of attribute symbols handled by this update case.
    */
   public void addCase(UpdateMappingCase update, Pair attributes)
   {
      update.setAttributeSet(getBitSet(attributes));

      m_updateMappingSet.add(update);
   }

   /**
    * Gets the update case iterator.
    * @return An iterator over the update cases.
    */
   public Iterator getCaseIterator()
   {
      return m_updateMappingSet.iterator();
   }

   /**
    * Gets a list of update cases to execute, given a set of attributes to update.
    * The algorithm attempts to find the minimal set of update cases that covers
    * all of the attributes to update, but the Set-Cover problem is NP-complete,
    * so this greedy algorithm may not always return the optimal result.
    * @param updateAttributeSet The set of attributes to update.
    * @return The list of update cases covering the set of attributes to update.
    */
   public List getUpdateCases(BitSet updateAttributeSet)
   {
      int nSize = m_updateMappingSet.size();
      List updateList = new ArrayList(nSize);
      BitSet updateSet = (BitSet)updateAttributeSet.clone();

      // Compute cover
      Holder potentialMappingSet = new HashHolder(nSize);

      for (int i = 0; i < nSize; i++)
      {
         potentialMappingSet.add(m_updateMappingSet.get(i));
      }

      boolean bSelectUnbatchable = false;

      while (!(potentialMappingSet.isEmpty() || updateSet.isEmpty()))
      {
         int nBestScore = 0;
         UpdateMappingCase bestMapping = null;

         for (Iterator itr = potentialMappingSet.iterator(); itr.hasNext(); )
         {
            UpdateMappingCase candidateMapping = (UpdateMappingCase)itr.next();

            if (candidateMapping.isBatch() ^ bSelectUnbatchable)
            {
               BitSet candidateSet = (BitSet)candidateMapping.getAttributeSet().clone();

               candidateSet.and(updateSet);

               int nScore = candidateSet.cardinality();

               if (nScore > nBestScore)
               {
                  nBestScore = nScore;
                  bestMapping = candidateMapping;
               }
            }
         }

         if (bestMapping != null)
         {
            potentialMappingSet.remove(bestMapping);
            updateList.add(bestMapping);
            updateSet.andNot(bestMapping.getAttributeSet());
         }
         else
         {
            if (bSelectUnbatchable)
            {
               throw new PersistenceException("err.persistence.virtual.updateNotCovered",
                  new Object[]
                  {
                     appendAttrSet(new StringBuilder(), updateSet, m_virtualMapping.getMetaclass()).toString(),
                     m_virtualMapping.getMetaclass().getName()
                  }
               );
            }
            else
            {
               bSelectUnbatchable = true;
            }
         }
      }

      return updateList;
   }

   /**
    * Resolves inheritance for this update mapping. Update mapping cases in this mapping
    * override base mapping cases where the attribute lists are identical.
    * @param base The base class update mapping.
    */
   public void resolveInheritance(UpdateMapping base)
   {
      if (base == null)
      {
         return;
      }

      int nBaseCount = base.m_updateMappingSet.size();
      int nDerivedCount = m_updateMappingSet.size();
      HolderList updateMappingSet = new HashHolderList(nBaseCount + nDerivedCount);

      for (int i = 0; i < nBaseCount; i++)
      {
         Object baseUpdateMapping = base.m_updateMappingSet.get(i);
         UpdateMappingCase derivedUpdateMapping = (UpdateMappingCase)m_updateMappingSet.get(baseUpdateMapping);

         if (derivedUpdateMapping != null)
         {
            updateMappingSet.add(derivedUpdateMapping);
            m_updateMappingSet.remove(derivedUpdateMapping);
         }
         else
         {
            updateMappingSet.add(baseUpdateMapping);
         }
      }

      updateMappingSet.addAll(m_updateMappingSet);

      m_updateMappingSet = updateMappingSet;
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();

      for (int i = 0, nCount = m_updateMappingSet.size(); i < nCount; i++)
      {
         ((UpdateMappingCase)m_updateMappingSet.get(i)).makeReadOnly();
      }

      m_updateMappingSet.trimToSize();
   }

   /**
    * Appends a set of attribute names to a buffer. Puts commas between the attribute
    * names.
    * @param buf The buffer to which the attribute string should be appended.
    * @param attrSet The attribute bitmap to append.
    * @param metaclass The class on which the attributes are defined.
    * @return The buffer, buf.
    */
   public static StringBuilder appendAttrSet(StringBuilder buf, BitSet attrSet, Metaclass metaclass)
   {
      boolean bAppendComma = false;

      for (int i = attrSet.nextSetBit(0); i >= 0; i = attrSet.nextSetBit(i + 1))
      {
         if (bAppendComma)
         {
            buf.append(", ");
         }

         bAppendComma = true;

         buf.append(metaclass.getInstanceAttribute(i).getName());
      }

      return buf;
   }

   /**
    * Converts a list of attributes to an attribute bitmap. The position in the
    * bitmap corresponds to the attribute ordinal in the class.
    * @param attributes The attribute name Symbol list.
    * @return The attribute bitmap.
    */
   public BitSet getBitSet(Pair attributes)
   {
      Metaclass metaclass = m_virtualMapping.getMetaclass();
      int nCount = metaclass.getInstanceAttributeCount();
      BitSet attrSet = new BitSet(nCount);

      while (attributes != null)
      {
         Symbol sym = (Symbol)attributes.getHead();
         Attribute attr = metaclass.getAttribute(sym);

         if (attr.isStatic())
         {
            throw new MetadataException("err.meta.persistence.virtual.updateStaticAttribute",
               new Object[]{attr.getName(), metaclass.getName()});
         }

         attrSet.set(attr.getOrdinal());
         attributes = attributes.getNext();
      }

      return attrSet;
   }

   /**
    * Converts an attribute bitmap to a list of attributes.
    * @param bitmap The attribute bitmap.
    * @return An attribute list.
    */
   public Pair getAttributes(BitSet bitmap)
   {
      Metaclass metaclass = m_virtualMapping.getMetaclass();
      Pair attrList = null;

      for (int i = bitmap.nextSetBit(0); i >= 0; i = bitmap.nextSetBit(i + 1))
      {
         attrList = new Pair(metaclass.getInstanceAttribute(i).getSymbol(), attrList);
      }

      return attrList;
   }
}
