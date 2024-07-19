// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.virtual;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.Key;
import nexj.core.util.Named;

/**
 * Represents an attribute holding a foreign key.
 */
public class VirtualKey extends MetadataObject implements Key, Named
{
   // attributes

   /**
    * False if key is made from one or more primitive attributes; true if key is made
    * from one attribute whose value is an OID.
    */
   protected boolean m_bOIDAttribute;

   // associations

   /**
    * The attribute(s) holding the foreign key.
    */
   protected Attribute[] m_attributeArray;

   /**
    * Key part array.
    */
   protected Primitive[] m_partTypeArray;

   /**
    * Key part sort direction array.
    */
   protected boolean[] m_bPartAscendingArray;

   // constructors

   /**
    * Constructs a new virtual key.
    * @param attribute The attribute holding the foreign key.
    */
   public VirtualKey(Attribute attribute)
   {
      m_attributeArray = new Attribute[]{attribute};

      if (attribute.getType().isPrimitive())
      {
         m_partTypeArray = new Primitive[] {(Primitive)attribute.getType()};
         m_bPartAscendingArray = new boolean[] {true};
      }
      else
      {
         Key objKey = ((Metaclass)attribute.getType()).getPersistenceMapping().getObjectKey();
         int nPartCount = objKey.getPartCount();

         m_bOIDAttribute = true;
         m_partTypeArray = new Primitive[nPartCount];
         m_bPartAscendingArray = new boolean[nPartCount];

         for (int i = 0; i < nPartCount; i++)
         {
            m_partTypeArray[i] = objKey.getPartType(i);
            m_bPartAscendingArray[i] = objKey.isPartAscending(i);
         }
      }
   }

   /**
    * Constructs a new virtual key.
    * @param attributeArray The array of attributes holding the primitive parts comprising
    * the foreign key.
    */
   public VirtualKey(Attribute[] attributeArray)
   {
      int nCount = attributeArray.length;

      m_attributeArray = new Attribute[nCount];
      m_partTypeArray = new Primitive[nCount];
      m_bPartAscendingArray = new boolean[nCount];

      System.arraycopy(attributeArray, 0, m_attributeArray, 0, nCount);

      for (int i = 0; i < nCount; i++)
      {
         m_partTypeArray[i] = (Primitive)attributeArray[i].getType();
         m_bPartAscendingArray[i] = true;
      }
   }

   // operations

   /**
    * Gets the virtual key attribute.
    * @return The attribute holding the key or key part.
    */
   public Attribute getAttribute(int nOrdinal)
   {
      return m_attributeArray[nOrdinal];
   }

   /**
    * Gets the number of attributes holding the key parts.
    * @return The number of attributes in the key.
    */
   public int getAttributeCount()
   {
      return m_attributeArray.length;
   }

   /**
    * @return True if this key is for a single attribute that holds an OID; false if
    * the key's attributes hold primitives.
    */
   public boolean isOIDAttribute()
   {
      return m_bOIDAttribute;
   }

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
      return -1;
   }

   /**
    * @see nexj.core.meta.persistence.Key#getPartCount()
    */
   public int getPartCount()
   {
      return m_partTypeArray.length;
   }

   /**
    * @see nexj.core.meta.persistence.Key#getPartType(int)
    */
   public Primitive getPartType(int nOrdinal)
   {
      return m_partTypeArray[nOrdinal];
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
      return false;
   }

   /**
    * @see nexj.core.meta.persistence.Key#isObjectKeyPart()
    */
   public boolean isObjectKeyPart()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.persistence.Key#isPartAscending(int)
    */
   public boolean isPartAscending(int nOrdinal)
   {
      return m_bPartAscendingArray[nOrdinal];
   }

   /**
    * @see nexj.core.meta.persistence.Key#isUnique()
    */
   public boolean isUnique()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.persistence.Key#setMapped()
    */
   public void setMapped()
   {
   }

   /**
    * Supports metadata export.
    * @see nexj.core.util.Named#getName()
    */
   public String getName()
   {
      StringBuilder buf = new StringBuilder();

      for (int i = 0; i < m_attributeArray.length; i++)
      {
         if (i != 0)
         {
            buf.append(' ');
         }

         buf.append(m_attributeArray[i].getName());
      }

      return buf.toString();
   }
}
