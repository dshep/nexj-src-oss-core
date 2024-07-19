// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

/**
 * Privilege that has an ordinal number in the
 * global set of privileges and can be assigned
 * to an attribute or an event. 
 */
public final class PrimitivePrivilege extends Privilege
{
   // attributes

   /**
    * The privilege ordinal number.
    */
   protected int m_nOrdinal = -1;

   // constructors

   /**
    * Creates a primitive privilege.
    * @param sName The privilege name. Must be unique within the repository.
    */
   public PrimitivePrivilege(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * @see nexj.core.meta.Privilege#isPrimitive()
    */
   public boolean isPrimitive()
   {
      return true;
   }
   
   /**
    * @see nexj.core.meta.Privilege#addTo(nexj.core.meta.PrivilegeSet)
    */
   public void addTo(PrivilegeSet privilegeSet)
   {
      privilegeSet.add(this);
   }

   /**
    * Sets the privilege ordinal number.
    * @param nOrdinal The privilege ordinal number to set.
    */
   public void setOrdinal(int nOrdinal)
   {
      verifyNotReadOnly();
      m_nOrdinal = nOrdinal;
   }

   /**
    * @return The privilege ordinal number.
    */
   public int getOrdinal()
   {
      return m_nOrdinal;
   }
}
