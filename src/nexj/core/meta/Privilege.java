// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.util.Captioned;

/**
 * Base class for privilege objects.
 */
public abstract class Privilege extends NamedMetadataObject implements Captioned
{
   // attributes
   
   /**
    * The caption string id.
    */
   protected String m_sCaption;

   // constructors
   
   /**
    * Constructs the privilege object.
    * @param sName The privilege name. Must be unique within the repository.
    */
   public Privilege(String sName)
   {
      super(sName);
      m_sCaption = sName;
   }
   
   // operations
   
   /**
    * @return True if the privilege is primitive.
    */
   public abstract boolean isPrimitive();
   
   /**
    * Adds the privilege to a set.
    * @param privilegeSet The set where to add the privilege.
    */
   public abstract void addTo(PrivilegeSet privilegeSet);

   /**
    * Sets the caption string id.
    * @param sCaption The caption string id to set.
    */
   public void setCaption(String sCaption)
   {
      verifyNotReadOnly();
      m_sCaption = sCaption;
   }

   /**
    * @return The caption string id.
    */
   public String getCaption()
   {
      return m_sCaption;
   }
}
