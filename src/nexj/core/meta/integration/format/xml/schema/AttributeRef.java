// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.schema;


/**
 * Definition of a reference to an attribute.
 */
public class AttributeRef extends Attribute
{
   // associations

   /**
    * The referred attribute.
    */
   protected Attribute m_referent;

   // constructors

   /**
    * Constructs a new attribute reference definition.
    * @param referent The referred attribute.
    */
   public AttributeRef(Attribute referent)
   {
      super(referent.getName());
      m_referent = referent;
   }

   /**
    * Constructs a new attribute reference definition.
    * @param sName The name of the referred attribute.
    */
   public AttributeRef(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * Gets the referent.
    * @return The referred attribute.
    */
   public Attribute getReferent()
   {
      return m_referent;
   }

   /**
    * Sets the referent.
    * @param referent The referred attribute.
    */
   public void setReferent(Attribute referent)
   {
      assert referent.getName().equals(m_sName);

      m_referent = referent;
   }

   /**
    * @see nexj.core.meta.integration.format.xml.schema.Attribute#getType()
    */
   public Type getType()
   {
      return m_referent.getType();
   }

   /**
    * @see nexj.core.meta.integration.format.xml.schema.Attribute#getAttributeType()
    */
   public AtomicType getAttributeType()
   {
      return m_referent.getAttributeType();
   }

   /**
    * @see nexj.core.meta.integration.format.xml.schema.Attribute#setType(nexj.core.meta.integration.format.xml.schema.AtomicType)
    */
   public void setType(AtomicType type)
   {
      throw new UnsupportedOperationException("Cannot set the type of an attribute reference");
   }

   /**
    * @see nexj.core.meta.integration.format.xml.schema.SchemaItem#getItemType()
    */
   public byte getItemType()
   {
      return ATTRIBUTE_REF;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "AttributeRef(ref=" + m_referent + ')';
   }
}
