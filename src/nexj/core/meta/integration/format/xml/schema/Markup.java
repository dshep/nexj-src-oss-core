// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.schema;


/**
 * Definition of some markup (an element or an attribute).
 */
public abstract class Markup extends SchemaItem
{
   // attributes

   /**
    * True to qualify this markup with a namespace prefix; false otherwise.
    */
   protected boolean m_bQualified;

   // operations

   /**
    * Sets the qualified flag.
    * @param bQualified True to qualify this markup with a namespace prefix; false otherwise.
    */
   public void setQualified(boolean bQualified)
   {
      m_bQualified = bQualified;
   }

   /**
    * Gets the qualified flag.
    * @return True to qualify this markup with a namespace prefix; false otherwise.
    */
   public boolean isQualified()
   {
      return m_bQualified;
   }

   /**
    * Gets the data type of this markup.
    * @return The type of this markup's data.
    */
   public abstract Type getType();

   /**
    * Sets the data type of this markup.
    * @param type The type.
    */
   public abstract void setType(Type type);
}
