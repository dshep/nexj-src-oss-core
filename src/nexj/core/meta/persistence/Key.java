// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence;

import nexj.core.meta.Attribute;
import nexj.core.meta.Primitive;

/**
 * Interface representing a multi-part key.
 */
public interface Key
{
   /**
    * Gets the type of a key part with a given ordinal position.
    * @param nOrdinal The ordinal number of the part, 0-based.
    * @return The primitive value type of the part.
    */
   Primitive getPartType(int nOrdinal);

   /**
    * Determines whether the sort order of the key part is ascending.
    * @param nOrdinal The ordinal number of the part, 0-based.
    */
   boolean isPartAscending(int nOrdinal);

   /**
    * @return The number of the key parts.
    */ 
   int getPartCount();

   /**
    * @return True if this key is the object key.
    */
   boolean isObjectKey();
   
   /**
    * @return True if this key is part of the object key.
    */
   boolean isObjectKeyPart();
   
   /**
    * Determines the ordinal number of the key part in the object key.
    * @param The ordinal number of the part in this key.
    * @return The ordinal number of the part in the object key.
    */
   int getObjectKeyPartOrdinal(int nOrdinal);

   /**
    * Adds a mapped attribute to the key.
    * @param attribute The attribute to add.
    */
   public void addAttribute(Attribute attribute);

   /**
    * @return True if the key is a unique constraint.
    */
   boolean isUnique();
   
   /**
    * @return True if the key is shared between two or more classes.
    */
   boolean isMultiplexed();
   
   /**
    * Indicates that the key has been used in a persistence mapping.
    */
   public void setMapped();
}
