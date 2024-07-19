// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.schema;


/**
 * Definition of a type.
 */
public abstract class Type extends SchemaItem
{
   // associations

   /**
    * The base type.
    */
   protected Type m_base;

   // operations

   /**
    * Gets the base type.
    * @return The base type.
    */
   public Type getBase()
   {
      return m_base;
   }
}
