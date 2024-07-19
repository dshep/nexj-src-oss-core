// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.schema;


/**
 * Definition of an element.
 */
public class Element extends Markup
{
   // attributes

   /**
    * The minimum number of times this element must appear.
    */
   protected int m_nMinCount = 1;

   /**
    * The maximum number of times this element may appear.
    */
   protected int m_nMaxCount = 1;

   /**
    * True if xsi:nil may appear on the element.
    */
   protected boolean m_bNillable;

   // associations

   /**
    * The type of this element's data.
    */
   protected Type m_type = PrimitiveType.ANY;

   // constructors

   /**
    * Constructs a new element definition.
    * @param sName The element name.
    */
   public Element(String sName)
   {
      m_sName = sName;
   }

   // operations

   /**
    * @see nexj.core.meta.integration.format.xml.schema.Markup#getType()
    */
   public Type getType()
   {
      return m_type;
   }

   /**
    * Sets the data type of this element.
    * @param type The type of this element's data.
    */
   public void setType(Type type)
   {
      m_type = type;
   }

   /**
    * Gets the minimum number of times this element may appear.
    * @return The minimum count.
    */
   public int getMinCount()
   {
      return m_nMinCount;
   }

   /**
    * Sets the minimum number of times this element may appear.
    * @param nMinCount The minimum count.
    */
   public void setMinCount(int nMinCount)
   {
      m_nMinCount = nMinCount;
   }

   /**
    * Gets the maximum number of times this element may appear.
    * @return The maximum count; 0 if infinite.
    */
   public int getMaxCount()
   {
      return m_nMaxCount;
   }

   /**
    * Sets the maximum number of times this element may appear.
    * @param nMaxCount The maximum count; 0 for infinite.
    */
   public void setMaxCount(int nMaxCount)
   {
      m_nMaxCount = nMaxCount;
   }

   /**
    * Gets the nillable flag.
    * @return True if xsi:nil may appear on the element; false otherwise.
    */
   public boolean isNillable()
   {
      return m_bNillable;
   }

   /**
    * Sets the nillable flag.
    * @param bNillable True if xsi:nil may appear on the element; false otherwise.
    */
   public void setNillable(boolean bNillable)
   {
      m_bNillable = bNillable;
   }

   /**
    * @see nexj.core.meta.integration.format.xml.schema.SchemaItem#getItemType()
    */
   public byte getItemType()
   {
      return ELEMENT;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "Element(\"" + m_sName + "\")";
   }
}
