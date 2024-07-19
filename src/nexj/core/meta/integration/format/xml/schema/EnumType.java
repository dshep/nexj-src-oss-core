// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.schema;

import java.util.Iterator;
import java.util.Set;

import nexj.core.util.HashHolder;

/**
 * Definition of an enumeration.
 */
public class EnumType extends AtomicType
{
   // associations

   /**
    * Set of possible enumeration values.
    */
   protected Set m_enumValueSet = new HashHolder();

   // constructors

   /**
    * Constructs a new enumeration definition.
    * @param sName The name of the enumeration; null if unnamed.
    * @param base The base type that is restricted by this enumeration.
    */
   public EnumType(String sName, AtomicType base)
   {
      super(sName);
      m_base = base;
   }

   // operations

   /**
    * Adds a possible enumeration value to this enum.
    * @param value The enumeration value.
    */
   public void addValue(Object value)
   {
      m_enumValueSet.add(value);
   }

   /**
    * Gets an iterator over the possible enumeration values.
    * @return An iterator over the enumeration values.
    */
   public Iterator getValueIterator()
   {
      return m_enumValueSet.iterator();
   }

   /**
    * Gets the count of enumeration values.
    * @return The count of enumeration values.
    */
   public int getValueCount()
   {
      return m_enumValueSet.size();
   }

   /**
    * @see nexj.core.meta.integration.format.xml.schema.SchemaItem#getItemType()
    */
   public byte getItemType()
   {
      return ENUM_TYPE;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "EnumType(" + ((m_sName == null) ? "ANONYMOUS" : "\"" + m_sName + "\"") + ')';
   }
}
