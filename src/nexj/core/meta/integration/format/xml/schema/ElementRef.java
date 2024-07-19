// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.schema;


/**
 * Definition of a reference to an element.
 */
public class ElementRef extends Element
{
   // associations

   /**
    * The referred element.
    */
   protected Element m_referent;

   // constructors

   /**
    * Constructs a new element reference definition.
    * @param referent The referred element.
    */
   public ElementRef(Element referent)
   {
      super(referent.getName());
      m_referent = referent;
   }

   /**
    * Constructs a new element reference definition. 
    * @param sName The name of the referred element.
    */
   public ElementRef(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * Gets the referent.
    * @return The referred element.
    */
   public Element getReferent()
   {
      return m_referent;
   }

   /**
    * Sets the reference.
    * @param referent The referred element.
    */
   public void setReferent(Element referent)
   {
      assert referent.getName().equals(m_sName);

      m_referent = referent;
   }

   /**
    * @see nexj.core.meta.integration.format.xml.schema.Element#getType()
    */
   public Type getType()
   {
      return m_referent.getType();
   }

   /**
    * @see nexj.core.meta.integration.format.xml.schema.Element#setType(nexj.core.meta.integration.format.xml.schema.Type)
    */
   public void setType(Type type)
   {
      throw new IllegalStateException("Cannot set the type of an element reference");
   }

   /**
    * @see nexj.core.meta.integration.format.xml.schema.SchemaItem#getItemType()
    */
   public byte getItemType()
   {
      return ELEMENT_REF;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "ElementRef(ref=" + m_referent + ')';
   }
}
