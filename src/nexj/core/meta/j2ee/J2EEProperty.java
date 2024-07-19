// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.j2ee;

import nexj.core.util.Named;

/**
 * J2EE configuration property.
 */
public class J2EEProperty implements Named
{
   // attributes

   /**
    * The property name.
    */
   protected String m_sName;

   /**
    * The property value.
    */
   protected String m_sValue;

   /**
    * The property type name.
    */
   protected String m_sType;
   
   // constructors

   /**
    * Constructs a property.
    * @param sName The property name.
    * @param sValue The property value.
    * @param sType The property type.
    */
   public J2EEProperty(String sName, String sValue, String sType)
   {
      assert sName != null;
      assert sType != null;

      m_sName = sName;
      m_sValue = sValue;
      m_sType = sType;
   }

   /**
    * Constructs a string property.
    * @param sName The property name.
    * @param sValue The property value.
    */
   public J2EEProperty(String sName, String sValue)
   {
      this(sName, sValue, "java.lang.String");
   }

   /**
    * Constructs an integer property.
    * @param sName The property name.
    * @param nValue The property value.
    */
   public J2EEProperty(String sName, int nValue)
   {
      this(sName, String.valueOf(nValue), "int");
   }

   /**
    * Constructs a long property.
    * @param sName The property name.
    * @param lValue The property value.
    */
   public J2EEProperty(String sName, long lValue)
   {
      this(sName, String.valueOf(lValue), "long");
   }

   /**
    * Constructs a boolean property.
    * @param sName The property name.
    * @param bValue The property value.
    */
   public J2EEProperty(String sName, boolean bValue)
   {
      this(sName, String.valueOf(bValue), "boolean");
   }

   // operations

   /**
    * Sets the property name.
    * @param sName The property name to set.
    */
   public void setName(String sName)
   {
      m_sName = sName;
   }

   /**
    * @return The property name.
    */
   public String getName()
   {
      return m_sName;
   }

   /**
    * Sets the property value.
    * @param sValue The property value to set.
    */
   public void setValue(String sValue)
   {
      m_sValue = sValue;
   }

   /**
    * @return The property value.
    */
   public String getValue()
   {
      return m_sValue;
   }

   /**
    * Sets the property type name.
    * @param sType The property type name to set.
    */
   public void setTypeName(String sType)
   {
      m_sType = sType;
   }

   /**
    * @return The property type name.
    */
   public String getTypeName()
   {
      return m_sType;
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (obj instanceof J2EEProperty)
      {
         J2EEProperty property = (J2EEProperty)obj;

         return m_sName.equals(property.getName());
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
      return m_sName + ": " + m_sType + " = " + m_sValue;
   }
}
