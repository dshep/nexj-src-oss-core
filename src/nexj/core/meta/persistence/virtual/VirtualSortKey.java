// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.virtual;

import java.util.ArrayList;
import java.util.List;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataObject;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Represents a sort key for a virtual-mapped class. The key's components are attributes
 * of it's class.
 */
public class VirtualSortKey extends MetadataObject
{
   // attributes

   /**
    * True if the sort key is unique.
    */
   protected boolean m_bUnique;

   // associations

   /**
    * The components of the sort key.
    */
   protected Attribute[] m_attributeArray;

   /**
    * The sort direction for each attribute: true for ascending; false for descending.
    */
   protected boolean[] m_bDirectionArray;

   // constructors

   /**
    * Constructs a new sort key.
    * @param keyDef The key definition. A list of attribute names and/or (attribute . direction)
    * pairs.
    * @param bUnique True if the key is unique; false otherwise.
    * @param metaclass The class on which the key is defined.
    */
   public VirtualSortKey(Pair keyDef, boolean bUnique, Metaclass metaclass)
   {
      m_bUnique = bUnique;

      List attrDirectionList = new ArrayList();

      for (; keyDef != null; keyDef = keyDef.getNext())
      {
         Object head = keyDef.getHead();

         if (head instanceof Pair)
         {
            attrDirectionList.add(head);
         }
         else
         {
            attrDirectionList.add(new Pair(head, Boolean.TRUE));
         }
      }

      int nCount = attrDirectionList.size();

      m_attributeArray = new Attribute[nCount];
      m_bDirectionArray = new boolean[nCount];

      for (int i = 0; i < nCount; i++)
      {
         Symbol attrSym = (Symbol)((Pair)attrDirectionList.get(i)).getHead();

         if (attrSym != null)
         {
            m_attributeArray[i] = metaclass.getAttribute(attrSym);
         }

         m_bDirectionArray[i] = ((Boolean)((Pair)attrDirectionList.get(i)).getTail()).booleanValue();
      }
   }

   // operations

   /**
    * Gets the number of components of the sort key.
    * @return The number of components of the sort key.
    */
   public int getAttributeCount()
   {
      return m_attributeArray.length;
   }

   /**
    * Gets the given component of the sort key.
    * @param nOrdinal The key component ordinal.
    * @return The key component; null for "@" (OID).
    */
   public Attribute getAttribute(int nOrdinal)
   {
      return m_attributeArray[nOrdinal];
   }

   /**
    * Returns whether a given component of the sort key is sorted in ascending or descending
    * order.
    * @param nOrdinal The key component ordinal.
    * @return True for ascending; false for descending.
    */
   public boolean isAscending(int nOrdinal)
   {
      return m_bDirectionArray[nOrdinal];
   }

   /**
    * @return True if the sort key is unique; false otherwise.
    */
   public boolean isUnique()
   {
      return m_bUnique;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder("VirtualSortKey(unique=");

      buf.append(m_bUnique);
      buf.append(", ");

      for (int i = 0; i < m_attributeArray.length; i++)
      {
         if (i > 0)
         {
            buf.append(' ');
         }

         buf.append('(');

         if (m_attributeArray[i] == null)
         {
            buf.append('@');
         }
         else
         {
            buf.append(m_attributeArray[i].getName());
         }

         buf.append(" . ");
         buf.append(m_bDirectionArray[i]);
         buf.append(')');
      }

      buf.append(')');

      return buf.toString();
   }
}
