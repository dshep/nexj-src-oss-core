// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.soa;

import nexj.core.meta.xml.XMLSOAMetadataLoader;
import nexj.core.meta.xml.XMLSOAMetadataLoader.SOAObject;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Symbol;

/**
 * An attribute of an information model type.
 */
public class Attribute extends SOAObject
{
   // attributes

   /**
    * True if the attribute must be non-null at RPC boundaries.
    */
   protected boolean m_bRequired;

   /**
    * True if the attribute holds a collection; false otherwise.
    */
   protected boolean m_bCollection;

   // associations

   /**
    * The parent type in which this attribute is declared.
    */
   protected ModelType m_parent;

   /**
    * The type of data stored in this attribute.
    */
   protected Symbol m_type;

   // constructors

   /**
    * Creates a new attribute.
    * @param parent The parent type in which this attribute is declared.
    */
   public Attribute(ModelType parent)
   {
      m_parent = parent;
   }

   // operations

   /**
    * Gets the type of data stored in this attribute.
    * @return The attribute data type.
    */
   public Symbol getType()
   {
      return m_type;
   }

   /**
    * Sets the type of data stored in this attribute.
    * @param type The attribute data type.
    */
   public void setType(Symbol type)
   {
      m_type = type;
   }

   /**
    * Gets whether the attribute must be non-null at RPC boundaries.
    * @return True if this attribute must be non-null; false otherwise.
    */
   public boolean isRequired()
   {
      return m_bRequired;
   }

   /**
    * Sets whether this attribute must be non-null at RPC boundaries.
    * @param bRequired True if this attribute must be non-null; false otherwise.
    */
   public void setRequired(boolean bRequired)
   {
      m_bRequired = bRequired;
   }

   /**
    * Gets whether the attribute holds a collection.
    * @return True if the attribute is a collection; false otherwise.
    */
   public boolean isCollection()
   {
      return m_bCollection;
   }

   /**
    * Sets whether the attribute holds a collection.
    * @param bCollection True if the attribute is a collection; false otherwise.
    */
   public void setCollection(boolean bCollection)
   {
      m_bCollection = bCollection;
   }

   /**
    * Generates the following code:
    *    (attribute <attribute name> :type <type> :collection <m_bCollection> :required <m_bRequired> "Description")
    *
    * @see nexj.core.meta.xml.XMLSOAMetadataLoader.SOAObject#getCode()
    */
   public Object getCode()
   {
      return ConstPair.list(XMLSOAMetadataLoader.ATTRIBUTE, Symbol.define(m_sName),
         XMLSOAMetadataLoader.TYPE, m_type, XMLSOAMetadataLoader.COLLECTION, Boolean.valueOf(m_bCollection),
         XMLSOAMetadataLoader.REQUIRED, Boolean.valueOf(m_bRequired), getDescription());
   }
}
