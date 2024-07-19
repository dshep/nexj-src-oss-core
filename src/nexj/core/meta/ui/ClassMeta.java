// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.ui;

import java.util.Collection;
import java.util.Iterator;

import nexj.core.meta.GenericType;
import nexj.core.scripting.Pair;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.MultiMap;

/**
 * Class metadata for UI subset of Domain Model.
 */
public abstract class ClassMeta extends GenericType
{
   /**
    * @param sName
    */
   public ClassMeta(String sName)
   {
      super(sName);
   }

   /**
    * @return ClassMeta caption.
    */
   public abstract String getCaption();
   
   /**
    * @see nexj.core.meta.Metaclass#getAttributeMeta(java.lang.String)
    */
   public abstract AttributeMeta getAttributeMeta(String sName);

   /**
    * @see nexj.core.meta.Metaclass#findAttributeMeta(java.lang.String)
    */
   public abstract AttributeMeta findAttributeMeta(String sName);

   /**
    * @see nexj.core.meta.Metaclass#getAttributeMetaCount()
    */
   public abstract int getAttributeMetaCount();

   /**
    * @see nexj.core.meta.Metaclass#getAttributeMetaIterator()
    */
   public abstract Iterator getAttributeMetaIterator();

   /**
    * @see nexj.core.meta.Metaclass#getLockingAttributeMeta()
    */
   public abstract AttributeMeta getLockingAttributeMeta();

   /**
    * @see nexj.core.meta.Metaclass#getNameAttributeMeta()
    */
   public abstract AttributeMeta getNameAttributeMeta();

   /**
    * @see nexj.core.meta.Metaclass#getBaseClassMeta()
    */
   public abstract ClassMeta getBaseClassMeta();

   /**
    * @see nexj.core.meta.Metaclass#isUpcast(nexj.core.meta.Metaclass)
    */
   public abstract boolean isUpcast(ClassMeta metaclass);

   /**
    * @see nexj.core.meta.Metaclass#getDerivedClassMeta(java.lang.String)
    */
   public abstract ClassMeta getDerivedClassMeta(String sName);

   /**
    * @see nexj.core.meta.Metaclass#getDerivedCount()
    */
   public abstract int getDerivedAttributeMetaCount();

   /**
    * @see nexj.core.meta.Metaclass#getDerivedIterator()
    */
   public abstract Iterator getDerivedAttributeMetaIterator();

   /**
    * Finds a derived ClassMeta.
    * @param sDerivedName The name of the derived ClassMeta.
    * @return The ClassMeta; null if not found.
    */
   public abstract ClassMeta findDerivedClassMeta(String sDerivedName);

   /**
    * @see nexj.core.meta.Metaclass#getReadPrivilegeOrdinal()
    */
   public abstract int getReadPrivilegeOrdinal();

   /**
    * @see nexj.core.meta.Metaclass#getUpdatePrivilegeOrdinal()
    */
   public abstract int getUpdatePrivilegeOrdinal();

   /**
    * @see nexj.core.meta.Metaclass#getDeletePrivilegeOrdinal()
    */
   public abstract int getDeletePrivilegeOrdinal();

   /**
    * @see nexj.core.meta.Metaclass#getCreatePrivilegeOrdinal()
    */
   public abstract int getCreatePrivilegeOrdinal();

   /**
    * @see nexj.core.meta.Metaclass#getUpdateAccessAttributeMeta()
    */
   public abstract AttributeMeta getUpdateAccessAttributeMeta();

   /**
    * @see nexj.core.meta.Metaclass#getDeleteAccessAttributeMeta()
    */
   public abstract AttributeMeta getDeleteAccessAttributeMeta();
   
   /**
    * @see nexj.core.meta.Metaclass#getInstanceAttribute(int)
    */
   public abstract AttributeMeta getInstanceAttributeMeta(int nOrdinal);

   /**
    * Gets an instance attribute by ordinal number.
    * @param nOrdinal The instance attribute ordinal number.
    * @return The attribute object.
    */
   public abstract AttributeMeta getStaticAttributeMeta(int nOrdinal);

   /**
    * Helper method to compute pseudo inverse dependencies. This map depends on
    * the set of class meta and attribute meta present on the client-side.
    * 
    * @param classIter Class meta iterator
    * @return A lookup of ClassMeta arrays keyed by ClassMeta
    */
   public static Lookup createValueInverseDependencyMap(Iterator classIter)
   {
      MultiMap valueInvDepMap = new MultiMap();

      while (classIter.hasNext())
      {
         final ClassMeta clazz = (ClassMeta)classIter.next();

         for (Iterator attrIter = clazz.getAttributeMetaIterator(); attrIter.hasNext();)
         {
            final AttributeMeta startAttr = (AttributeMeta)attrIter.next();

            if (startAttr.isClientCalculable())
            {
               curDep: for (Pair depencies = startAttr.getValueDependencyAssociations(); depencies != null; depencies = depencies
                  .getNext())
               {
                  ClassMeta cur = clazz;

                  for (Pair path = (Pair)depencies.getHead(); path != null; path = path.getNext())
                  {
                     String sAttributeName = (String)path.getHead();
                     AttributeMeta segmentAttr = cur.findAttributeMeta(sAttributeName);

                     if (segmentAttr == null || segmentAttr.isCollection())
                     {
                        continue curDep;
                     }

                     valueInvDepMap.add(segmentAttr, startAttr);

                     if (!(segmentAttr.getType() instanceof ClassMeta))
                     {
                        continue curDep;
                     }

                     cur = (ClassMeta)segmentAttr.getType();
                  }
               }
            }
         }
      }

      Lookup out = new HashTab(valueInvDepMap.keyCount());

      // Transfer to a map of arrays
      for (Lookup.Iterator it = valueInvDepMap.iterator(); it.hasNext();)
      {
         AttributeMeta key = (AttributeMeta)it.next();
         Collection col = (Collection)it.getValue();

         out.put(key, col.toArray(new AttributeMeta[col.size()]));
      }

      return out;
   }
}