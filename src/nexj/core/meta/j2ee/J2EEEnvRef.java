// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.j2ee;

import nexj.core.util.Named;
import nexj.core.util.ObjUtil;

/**
 * J2EE environment reference.
 */
public class J2EEEnvRef implements Named
{
   // attributes

   /**
    * The resource reference name.
    */
   protected String m_sName;

   /**
    * The resource JNDI name.
    */
   protected String m_sJNDIName;

   /**
    * The resource class name.
    */
   protected String m_sClassName;

   // constructors

   /**
    * Constructs the environment reference.
    * @param sName The reference name.
    * @param sJNDIName The reference JNDI name.
    * @param sClassName The reference class name.
    */
   public J2EEEnvRef(String sName, String sJNDIName, String sClassName)
   {
      m_sName = sName;
      m_sJNDIName = sJNDIName;
      m_sClassName = sClassName;
   }

   // operations

   /**
    * Sets the resource reference name.
    * @param sName The resource reference name to set.
    */
   public void setName(String sName)
   {
      m_sName = sName;
   }

   /**
    * @return The resource reference name.
    */
   public String getName()
   {
      return m_sName;
   }

   /**
    * Sets the resource JNDI name.
    * @param sJNDIName The resource JNDI name to set.
    */
   public void setJNDIName(String sJNDIName)
   {
      m_sJNDIName = sJNDIName;
   }

   /**
    * @return The resource JNDI name.
    */
   public String getJNDIName()
   {
      return m_sJNDIName;
   }

   /**
    * Sets the resource class name.
    * @param sClassName The resource class name to set.
    */
   public void setClassName(String sClassName)
   {
      m_sClassName = sClassName;
   }

   /**
    * @return The resource class name.
    */
   public String getClassName()
   {
      return m_sClassName;
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (obj instanceof J2EEEnvRef)
      {
         J2EEEnvRef ref = (J2EEEnvRef)obj;

         return m_sName.equals(ref.getName());
      }

      return false;
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return m_sName.hashCode();
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return ObjUtil.getShortClassName(this) + "(name=" + m_sName + 
         ", jndi=" + m_sJNDIName + ", class=" + m_sClassName + ')';
   }
}
