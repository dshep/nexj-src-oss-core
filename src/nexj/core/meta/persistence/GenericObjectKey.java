// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence;

import nexj.core.meta.Attribute;
import nexj.core.meta.Primitive;

/**
 * Generic implementation of an object key.
 */
public class GenericObjectKey implements Key
{
   // associations

   /**
    * The array of key part types.
    */
   protected Primitive[] m_typeArray;

   // constructors

   /**
    * Constructs a new object key.
    * @param typeArray The array of key part types.
    */
   public GenericObjectKey(Primitive[] typeArray)
   {
      m_typeArray = typeArray;
   }

   // operations

   /**
    * @see nexj.core.meta.persistence.Key#addAttribute(nexj.core.meta.Attribute)
    */
   public void addAttribute(Attribute attribute)
   {
   }

   /**
    * @see nexj.core.meta.persistence.Key#getObjectKeyPartOrdinal(int)
    */
   public int getObjectKeyPartOrdinal(int nOrdinal)
   {
      if (nOrdinal >= 0 && nOrdinal < m_typeArray.length)
      {
         return nOrdinal;
      }

      throw new IndexOutOfBoundsException();
   }

   /**
    * @see nexj.core.meta.persistence.Key#getPartCount()
    */
   public int getPartCount()
   {
      return m_typeArray.length;
   }

   /**
    * @see nexj.core.meta.persistence.Key#getPartType(int)
    */
   public Primitive getPartType(int nOrdinal)
   {
      return m_typeArray[nOrdinal];
   }

   /**
    * @see nexj.core.meta.persistence.Key#isMultiplexed()
    */
   public boolean isMultiplexed()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.persistence.Key#isObjectKey()
    */
   public boolean isObjectKey()
   {
      return true;
   }

   /**
    * @see nexj.core.meta.persistence.Key#isObjectKeyPart()
    */
   public boolean isObjectKeyPart()
   {
      return true;
   }

   /**
    * @see nexj.core.meta.persistence.Key#isPartAscending(int)
    */
   public boolean isPartAscending(int nOrdinal)
   {
      if (nOrdinal >= 0 && nOrdinal < m_typeArray.length)
      {
         return true;
      }

      throw new IndexOutOfBoundsException();
   }

   /**
    * @see nexj.core.meta.persistence.Key#isUnique()
    */
   public boolean isUnique()
   {
      return true;
   }

   /**
    * @see nexj.core.meta.persistence.Key#setMapped()
    */
   public void setMapped()
   {
   }
}
