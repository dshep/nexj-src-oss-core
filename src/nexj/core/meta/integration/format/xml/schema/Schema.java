// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.schema;

import java.util.Comparator;
import java.util.Iterator;

import nexj.core.meta.Documented;
import nexj.core.util.EmptyIterator;
import nexj.core.util.HashTab2D;
import nexj.core.util.Lookup2D;

/**
 * The definition of the elements, attributes, and types for a single namespace.
 */
public class Schema implements Documented
{
   // constants

   /**
    * The namespace token for element items.
    */
   private final static Object NS_ELEMENT = new Object();

   /**
    * The namespace token for attribute items.
    */
   private final static Object NS_ATTRIBUTE = new Object();

   /**
    * The namespace token for type items.
    */
   private final static Object NS_TYPE = new Object();

   /**
    * Namespace URI comparator.
    */
   public final static Comparator COMPARATOR = new Comparator()
   {
      public int compare(Object left, Object right)
      {
         return ((Schema)left).getURI().compareTo(((Schema)right).getURI());
      }
   };

   // attributes

   /**
    * The namespace URI. This is the unique identifier for the schema.
    */
   protected String m_sURI;

   /**
    * The prefix to use in XML documents for representing the namespace URI.
    */
   protected String m_sPreferredPrefix;

   /**
    * The schema documentation.
    */
   protected String m_sDescription;

   // associations

   /**
    * Map of schema item name and namespace constant (NS_*) to SchemaItem: SchemaItem[String, Object]
    */
   protected Lookup2D m_itemMap;

   // constructors

   /**
    * Constructs a new schema.
    * @param sURI The namespace URI, the schema's unique identifier.
    */
   public Schema(String sURI)
   {
      m_sURI = sURI;
   }

   // operations

   /**
    * Gets the namespace URI, the schema's unique identifier.
    * @return The namespace URI.
    */
   public String getURI()
   {
      return m_sURI;
   }

   /**
    * Gets the preferred prefix.
    * @return The prefix to use in XML documents for representing the namespace URI.
    */
   public String getPreferredPrefix()
   {
      return m_sPreferredPrefix;
   }

   /**
    * Sets the preferred prefix.
    * @param sPrefix The prefix to use in XML documents for representing the namespace URI.
    */
   public void setPreferredPrefix(String sPrefix)
   {
      m_sPreferredPrefix = sPrefix;
   }

   /**
    * Adds an item to the schema.
    * @param item The item to add.
    */
   public void addItem(SchemaItem item)
   {
      if (item.getName() == null)
      {
         throw new IllegalArgumentException("Missing name on " + item + " being added to " + this);
      }

      if (m_itemMap == null)
      {
         m_itemMap = new HashTab2D();
      }

      if (m_itemMap.put(item.getName(), getTypeKey(item.getItemType()), item) != null)
      {
         throw new IllegalStateException("Redefinition of " + item + " in " + this);
      }

      item.setSchema(this);
   }

   /**
    * Gets an item from the schema.
    * @param sName The item name.
    * @param nType The type of item to get. (Schema item names may be duplicated, as long
    * as the items have different types).
    * @return The item; null if not found.
    */
   public SchemaItem findItem(String sName, byte nType)
   {
      if (m_itemMap == null)
      {
         return null;
      }

      return (SchemaItem)m_itemMap.get(sName, getTypeKey(nType));
   }

   /**
    * Gets an iterator over the items defined in the top-level of this schema.
    * @return An iterator over this schema's items.
    */
   public Iterator getItemIterator()
   {
      if (m_itemMap == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_itemMap.valueIterator();
   }

   /**
    * Gets the number of items defined in the top-level of this schema.
    * @return The number of items in this schema.
    */
   public int getItemCount()
   {
      return (m_itemMap == null) ? 0 : m_itemMap.size();
   }

   /**
    * Gets the namespace token for a schema item of the given type.
    * This is needed because schema item names are only unique within their particular type. For example,
    * a schema may have both an attribute called "item" as well as an element called "item".
    * @param nType The schema item type.
    * @return The namespace token.
    */
   public static Object getTypeKey(byte nType)
   {
      switch (nType)
      {
         case SchemaItem.ELEMENT:
         case SchemaItem.ELEMENT_REF:
            return NS_ELEMENT;

         case SchemaItem.ATTRIBUTE:
         case SchemaItem.ATTRIBUTE_REF:
            return NS_ATTRIBUTE;

         case SchemaItem.COMPOSITE_TYPE:
         case SchemaItem.PRIMITIVE_TYPE:
         case SchemaItem.ENUM_TYPE:
            return NS_TYPE;

         default:
            throw new IllegalStateException("Unknown type: " + nType);
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "Schema(" + m_sURI + ')';
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (obj instanceof Schema)
      {
         return m_sURI.equals(((Schema)obj).getURI());
      }

      return false;
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return m_sURI.hashCode();
   }

   /**
    * @see nexj.core.meta.Documented#setDescription(java.lang.String)
    */
   public void setDescription(String sDescription)
   {
      m_sDescription = sDescription;
   }

   /**
    * @see nexj.core.meta.Documented#getDescription()
    */
   public String getDescription()
   {
      return m_sDescription;
   }
}
