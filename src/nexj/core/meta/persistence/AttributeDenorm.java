// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence;

import nexj.core.meta.MetadataObject;

/**
 * Attribute denormalization metadata.
 */
public abstract class AttributeDenorm extends MetadataObject
{
   // association

   /**
    * The attribute mapping.
    */
   protected AttributeMapping m_mapping;

   // constructors

   /**
    * Constructs the denorm.
    * @param mapping The attribute mapping.
    */
   protected AttributeDenorm(AttributeMapping mapping)
   {
      m_mapping = mapping;
   }
   
   // operations

   /**
    * Sets the attribute mapping.
    * @param mapping The attribute mapping to set.
    */
   public void setMapping(AttributeMapping mapping)
   {
      verifyNotReadOnly();
      m_mapping = mapping;
   }

   /**
    * @return The attribute mapping.
    */
   public AttributeMapping getMapping()
   {
      return m_mapping;
   }
}
