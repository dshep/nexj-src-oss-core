// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.schema;


/**
 * Definition of an attribute.
 */
public class Attribute extends Markup
{
   // attributes

   /**
    * The required flag.
    */
   protected boolean m_bRequired;

   // associations

   /**
    * The type of this attribute's data.
    */
   protected AtomicType m_type;

   // constructors

   /**
    * Constructs an attribute definition.
    * @param sName The attribute name.
    */
   public Attribute(String sName)
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
    * Gets the data type of this attribute.
    * @return The type of this attribute's data.
    */
   public AtomicType getAttributeType()
   {
      return m_type;
   }

   /**
    * Sets the data type of this attribute.
    * @param type The type of this attribute's data.
    */
   public void setType(AtomicType type)
   {
      m_type = type;
   }

   /**
    * Gets the required flag.
    * @return True if the attribute is required; false otherwise.
    */
   public boolean isRequired()
   {
      return m_bRequired;
   }

   /**
    * Sets the required flag.
    * @param bRequired True if the attribute is required; false otherwise.
    */
   public void setRequired(boolean bRequired)
   {
      m_bRequired = bRequired;
   }

   /**
    * @see nexj.core.meta.integration.format.xml.schema.SchemaItem#getItemType()
    */
   public byte getItemType()
   {
      return ATTRIBUTE;
   }

   /**
    * @see nexj.core.meta.integration.format.xml.schema.Markup#setType(nexj.core.meta.integration.format.xml.schema.Type)
    */
   public void setType(Type type)
   {
      switch (type.getItemType()) 
      {
         case SchemaItem.ENUM_TYPE:
         case SchemaItem.FORMAT_TYPE:
         case SchemaItem.PRIMITIVE_TYPE:
            m_type = (AtomicType)type;
            break;

         default:
            throw new IllegalArgumentException();
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "Attribute(\""+ m_sName + "\")";
   }
}
