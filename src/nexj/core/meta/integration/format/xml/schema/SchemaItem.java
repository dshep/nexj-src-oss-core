// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.schema;

import nexj.core.meta.Documented;
import nexj.core.util.Named;

/**
 * Definition of an item in a schema.
 */
public abstract class SchemaItem implements Named, Documented
{
   // constants

   /**
    * An element definition.
    */
   public final static byte ELEMENT = 0;

   /**
    * An element reference.
    */
   public final static byte ELEMENT_REF = 1;

   /**
    * An attribute definition.
    */
   public final static byte ATTRIBUTE = 2;

   /**
    * An attribute reference.
    */
   public final static byte ATTRIBUTE_REF = 3;

   /**
    * A complex type definition.
    */
   public final static byte COMPOSITE_TYPE = 4;

   /**
    * A primitive type definition.
    */
   public final static byte PRIMITIVE_TYPE = 5;

   /**
    * An enumeration definition.
    */
   public final static byte ENUM_TYPE = 6;

   /**
    * A primitive type that is governed by a format string.
    */
   public final static byte FORMAT_TYPE = 7;

   // attributes

   /**
    * The schema item name.
    */
   protected String m_sName;

   /**
    * The schema item documentation.
    */
   protected String m_sDescription;

   // associations

   /**
    * The schema on which this item is defined.
    */
   protected Schema m_schema;

   // operations

   /**
    * Gets the type of this schema item.
    * @return The type, one of the SchemaItem.* constants.
    */
   public abstract byte getItemType();

   /**
    * @see nexj.core.util.Named#getName()
    */
   public String getName()
   {
      return m_sName;
   }

   /**
    * Sets the schema of this item.
    * @param schema The schema.
    */
   public void setSchema(Schema schema)
   {
      m_schema = schema;
   }

   /**
    * Gets the schema of this item.
    * @return The schema; null if not defined at the top level of a schema.
    */
   public Schema getSchema()
   {
      return m_schema;
   }

   /**
    * Determines if this item is defined in the top level of a schema.
    * @return True if defined at the top level of a schema; false otherwise.
    */
   public boolean isTopLevel()
   {
      return m_schema != null;
   }

   /**
    * @see nexj.core.meta.Documented#getDescription()
    */
   public String getDescription()
   {
      return m_sDescription;
   }

   /**
    * @see nexj.core.meta.Documented#setDescription(java.lang.String)
    */
   public void setDescription(String sDescription)
   {
      m_sDescription = sDescription;
   }
}
